use std::{mem::size_of, sync::Arc};

use crate::{SmolStr, TextUnit, Types};

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
#[derive(Debug)]
pub struct GreenNode<T: Types> {
    kind: T::Kind,
    text_len: TextUnit,
    //TODO: implement llvm::trailing_objects trick
    children: Arc<[GreenElement<T>]>,
}

impl<T: Types> GreenNode<T> {
    /// Creates new Node.
    pub fn new(kind: T::Kind, children: Box<[GreenElement<T>]>) -> GreenNode<T> {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        GreenNode { kind, text_len, children: children.into() }
    }

    /// Kind of this node.
    pub fn kind(&self) -> T::Kind {
        self.kind
    }

    /// Length of the text, covered by this node.
    pub fn text_len(&self) -> TextUnit {
        self.text_len
    }
    /// Children of this node.
    pub fn children(&self) -> &[GreenElement<T>] {
        &self.children
    }

    /// Gets the child at index.
    pub(crate) fn get_child(&self, index: GreenIndex) -> Option<&GreenElement<T>> {
        self.children.get(index.0 as usize)
    }

    /// Number of memory bytes of occupied by subtree rooted at `self`.
    pub(crate) fn memory_size_of_subtree(&self) -> usize {
        let mut res = size_of::<Self>();
        self.children().iter().for_each(|el| match el {
            GreenElement::Token(token) => {
                res += size_of::<GreenToken<T>>();
                if token.text.len() > 22 {
                    res += token.text.len();
                }
            }
            GreenElement::Node(node) => res += node.memory_size_of_subtree(),
        });

        res
    }
}

impl<T: Types> Clone for GreenNode<T> {
    fn clone(&self) -> GreenNode<T> {
        GreenNode { kind: self.kind, text_len: self.text_len, children: Arc::clone(&self.children) }
    }
}

/// Index into a green node, which might refer to either Token or Node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct GreenIndex(pub(crate) u32);

impl GreenIndex {
    pub(crate) fn next(self) -> GreenIndex {
        GreenIndex(self.0 + 1)
    }

    pub(crate) fn prev(self) -> GreenIndex {
        // `GreenNode::get` does a bounds check anyway, so its ok to overflow
        GreenIndex(self.0.wrapping_sub(1))
    }
}

/// Leaf node in the immutable tree.
#[derive(Debug)]
pub struct GreenToken<T: Types> {
    kind: T::Kind,
    text: SmolStr,
}

impl<T: Types> GreenToken<T> {
    /// Creates new Token.
    pub fn new(kind: T::Kind, text: SmolStr) -> GreenToken<T> {
        GreenToken { kind, text }
    }
    /// Kind of this Token.
    pub fn kind(&self) -> T::Kind {
        self.kind
    }
    /// Text of this Token.
    pub fn text(&self) -> &SmolStr {
        &self.text
    }
    /// Text of this Token.
    pub fn text_len(&self) -> TextUnit {
        TextUnit::from_usize(self.text.len())
    }
}

impl<T: Types> Clone for GreenToken<T> {
    fn clone(&self) -> GreenToken<T> {
        GreenToken { kind: self.kind, text: self.text.clone() }
    }
}

/// Leaf or internal node in the immutable tree.
#[derive(Debug)]
pub enum GreenElement<T: Types> {
    /// Internal node.
    Node(GreenNode<T>),
    /// Leaf token.
    Token(GreenToken<T>),
}

impl<T: Types> Clone for GreenElement<T> {
    fn clone(&self) -> GreenElement<T> {
        match self {
            GreenElement::Node(it) => GreenElement::Node(it.clone()),
            GreenElement::Token(it) => GreenElement::Token(it.clone()),
        }
    }
}

impl<T: Types> From<GreenNode<T>> for GreenElement<T> {
    fn from(node: GreenNode<T>) -> GreenElement<T> {
        GreenElement::Node(node)
    }
}

impl<T: Types> From<GreenToken<T>> for GreenElement<T> {
    fn from(token: GreenToken<T>) -> GreenElement<T> {
        GreenElement::Token(token)
    }
}

impl<T: Types> GreenElement<T> {
    /// Returns kind of this element.
    pub fn kind(&self) -> T::Kind {
        match self {
            GreenElement::Node(it) => it.kind(),
            GreenElement::Token(it) => it.kind(),
        }
    }
    /// Returns length of the text covered by this element.
    pub fn text_len(&self) -> TextUnit {
        match self {
            GreenElement::Node(it) => it.text_len(),
            GreenElement::Token(it) => it.text_len(),
        }
    }
}

/// A checkpoint for maybe wrapping a node. See `GreenNodeBuilder::checkpoint` for details.
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(usize);

/// A builder for a green tree.
#[derive(Debug)]
pub struct GreenNodeBuilder<T: Types> {
    parents: Vec<(T::Kind, usize)>,
    children: Vec<GreenElement<T>>,
}

impl<T: Types> GreenNodeBuilder<T> {
    /// Creates new builder.
    pub fn new() -> Self {
        GreenNodeBuilder { parents: Vec::new(), children: Vec::new() }
    }
    /// Adds new token to the current branch.
    pub fn token(&mut self, kind: T::Kind, text: SmolStr) {
        let token = GreenToken { kind, text };
        self.children.push(token.into());
    }
    /// Start new node and make it current.
    pub fn start_node(&mut self, kind: T::Kind) {
        let len = self.children.len();
        self.parents.push((kind, len));
    }
    /// Finish current branch and restore previous
    /// branch as current.
    pub fn finish_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let children: Vec<_> = self.children.drain(first_child..).collect();
        let node = GreenNode::new(kind, children.into_boxed_slice());
        self.children.push(node.into());
    }
    /// Prepare for maybe wrapping the next node.
    /// The way wrapping works is that you first of all get a checkpoint,
    /// then you place all tokens you want to wrap, and then *maybe* call start_internal_at.
    /// Example:
    /// ```rust,ignore
    /// let checkpoint = builder.checkpoint();
    /// self.parse_expr();
    /// if self.peek() == Some(Token::Plus) {
    ///   // 1 + 2 = Add(1, 2)
    ///   builder.start_internal_at(checkpoint, Token::Operation);
    ///   self.parse_expr();
    ///   builder.finish_internal();
    /// }
    /// ```
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.children.len())
    }
    /// Wrap the previous branch marked by `checkpoint` in a new branch and
    /// make it current.
    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: T::Kind) {
        let Checkpoint(checkpoint) = checkpoint;
        assert!(
            checkpoint <= self.children.len(),
            "checkpoint no longer valid, was finish_internal called early?"
        );

        if let Some(&(_, first_child)) = self.parents.last() {
            assert!(
                checkpoint >= first_child,
                "checkpoint no longer valid, was an unmatched start_internal called?"
            );
        }

        self.parents.push((kind, checkpoint));
    }
    /// Complete tree building. Make sure that
    /// `start_internal` and `finish_internal` calls
    /// are paired!
    pub fn finish(mut self) -> GreenNode<T> {
        assert_eq!(self.children.len(), 1);
        match self.children.pop().unwrap() {
            GreenElement::Node(node) => node,
            GreenElement::Token(_) => panic!(),
        }
    }
}
