use {
    crate::{
        green::{GreenElement, GreenNode, GreenToken},
        Kind, NodeOrToken,
    },
    rc_box::ArcBox,
    std::{
        collections::{HashMap, HashSet},
        hash,
        iter::TrustedLen,
        ptr,
        sync::Arc,
    },
};

#[derive(Debug, Clone)]
struct ThinEqNode(Arc<GreenNode>);

impl From<Arc<GreenNode>> for ThinEqNode {
    fn from(v: Arc<GreenNode>) -> Self {
        ThinEqNode(v)
    }
}

impl From<ArcBox<GreenNode>> for ThinEqNode {
    fn from(v: ArcBox<GreenNode>) -> Self {
        Arc::into(v.into())
    }
}

impl Eq for ThinEqNode {}
impl PartialEq for ThinEqNode {
    fn eq(&self, other: &Self) -> bool {
        self.0.kind == other.0.kind
            && self.0.text_len == other.0.text_len
            && self.0.children().zip(other.0.children()).all(|pair| match pair {
                (NodeOrToken::Node(lhs), NodeOrToken::Node(rhs)) => ptr::eq(&*lhs, &*rhs),
                (NodeOrToken::Token(lhs), NodeOrToken::Token(rhs)) => lhs == rhs,
                _ => false,
            })
    }
}

impl hash::Hash for ThinEqNode {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.kind.hash(state);
        self.0.text_len.hash(state);
        for child in self.0.children() {
            match child {
                NodeOrToken::Node(node) => ptr::hash(&*node, state),
                NodeOrToken::Token(token) => token.hash(state),
            }
        }
    }
}

/// A construction cache for green tree elements.
///
/// As the green tree is immutable, identical nodes can be deduplicated.
/// For example, all nodes representing the `#[inline]` attribute can
/// be deduplicated and refer to the same green node in memory,
/// despite their distribution throughout the source code.
#[derive(Debug, Default, Clone)]
pub struct GreenBuilder<'a> {
    nodes: HashSet<ThinEqNode>,
    tokens: HashMap<(Kind, &'a str), Arc<GreenToken>>,
}

impl<'a> GreenBuilder<'a> {
    /// Create a new node or clone a new Arc to an existing equivalent one.
    ///
    /// This checks children for identity equivalence, not structural,
    /// so it is `O(children.len())` and only caches higher-level nodes
    /// if the lower-level nodes have also been cached.
    pub fn node<I, E>(&mut self, kind: Kind, children: I) -> Arc<GreenNode>
    where
        E: Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>,
        I: IntoIterator<Item = E>,
        I::IntoIter: ExactSizeIterator + TrustedLen,
    {
        self.nodes.get_or_insert(GreenNode::new(kind, children).into()).0.clone()
    }

    /// Create a new token or clone a new Arc to an existing equivalent one.
    pub fn token(&mut self, kind: Kind, text: &'a str) -> Arc<GreenToken> {
        self.tokens
            .entry((kind, text))
            .or_insert_with(|| GreenToken::new(kind, text).into())
            .clone()
    }
}

/// A checkpoint for maybe wrapping a node. See [`GreenTreeBuilder::checkpoint`].
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Checkpoint(usize);

/// A builder for a green tree.
#[derive(Debug, Default)]
pub struct GreenTreeBuilder<'a> {
    cache: GreenBuilder<'a>,
    stack: Vec<(Kind, usize)>,
    children: Vec<GreenElement>,
}

impl<'a> GreenTreeBuilder<'a> {
    /// Create a new builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new builder, reusing a `GreenBuilder`.
    pub fn new_with(cache: GreenBuilder<'a>) -> Self {
        GreenTreeBuilder { cache, ..Self::default() }
    }

    /// The `GreenBuilder` used to create and dedupe nodes.
    pub fn builder(&self) -> &GreenBuilder<'a> {
        &self.cache
    }

    /// Add an element to the current branch.
    pub fn add(
        &mut self,
        element: impl Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>,
    ) -> &mut Self {
        self.children.push(element.into().into());
        self
    }

    /// Add a new token to the current branch.
    pub fn token(&mut self, kind: Kind, text: &'a str) -> &mut Self {
        let token = self.cache.token(kind, text);
        self.add(token)
    }

    /// Add a new node to the current branch.
    pub fn node<I, E>(&mut self, kind: Kind, children: I) -> &mut Self
    where
        E: Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>,
        I: IntoIterator<Item = E>,
        I::IntoIter: ExactSizeIterator + TrustedLen,
    {
        let node = self.cache.node(kind, children);
        self.add(node)
    }

    /// Start a new child node and make it the current branch.
    pub fn start_node(&mut self, kind: Kind) -> &mut Self {
        self.stack.push((kind, self.children.len()));
        self
    }

    /// Finish the current branch and restore its parent as current.
    pub fn finish_node(&mut self) -> &mut Self {
        let (kind, first_child) = self.stack.pop().unwrap_or_else(|| {
            panic!("called `GreenTreeBuilder::finish_node` without paired `start_node`")
        });
        // FIXME(rust-lang/rust#66759): skip this `collect`
        let children: Vec<_> = self.children.drain(first_child..).collect();
        let node = self.cache.node(kind, children);
        self.add(node)
    }

    /// Prepare for maybe wrapping the next node.
    ///
    /// The way wrapping works is that you first create a checkpoint,
    /// then you add all tokens you want to wrap,
    /// and then *maybe* call `start_node_at`.
    ///
    /// # Examples
    ///
    /// TODO: Port example
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.children.len())
    }

    /// Wrap the elements added after `checkpoint` in a new node
    /// and make it the current branch.
    pub fn start_node_at(&mut self, Checkpoint(checkpoint): Checkpoint, kind: Kind) -> &mut Self {
        assert!(
            checkpoint <= self.children.len(),
            "checkpoint no longer valid; was `finish_node` called early?",
        );

        if let Some(&(_, first_child)) = self.stack.last() {
            assert!(
                checkpoint >= first_child,
                "checkpoint no longer valid; was an unmatched `start_node` called?",
            )
        }

        self.stack.push((kind, checkpoint));
        self
    }

    /// Complete tree building.
    ///
    /// # Panics
    ///
    /// Panics if more nodes have been started than finished
    /// or the current branch has more than one element.
    pub fn finish(&mut self) -> Arc<GreenNode> {
        assert!(self.stack.is_empty());
        assert_eq!(self.children.len(), 1);
        self.children.pop().unwrap().into_node().unwrap()
    }

    /// Destroy this tree builder and recycle its build cache.
    pub fn recycle(self) -> GreenBuilder<'a> {
        self.cache
    }
}
