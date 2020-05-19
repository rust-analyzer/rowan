use rustc_hash::{FxHashMap, FxHasher};

use super::token::GreenTokenData;
use crate::{
    green::{GreenElement, GreenNode, GreenToken, SyntaxKind},
    NodeOrToken, SmolStr,
};
use smallvec::SmallVec;
use std::{
    fmt::Debug,
    hash::{Hash, Hasher},
};

#[derive(Default, Debug)]
pub struct NodeCache {
    nodes: FxHashMap<GreenNodeHash, GreenNode>,
    tokens: FxHashMap<GreenTokenData, GreenToken>,
}

struct GreenNodeHash {
    hasher: FxHasher,
    inner_hash: u64,
}

impl Eq for GreenNodeHash {}
impl PartialEq for GreenNodeHash {
    fn eq(&self, other: &Self) -> bool {
        self.inner_hash == other.inner_hash
    }
}

impl Debug for GreenNodeHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner_hash.fmt(f)
    }
}

impl GreenNodeHash {
    fn new<'a>(kind: SyntaxKind) -> Self {
        let mut hasher = FxHasher::default();
        kind.hash(&mut hasher);
        let inner_hash = hasher.finish();
        Self { hasher, inner_hash }
    }

    fn add_child(&mut self, child: &GreenElement) {
        child.hash(&mut self.hasher);
        self.inner_hash = self.hasher.finish();
    }
}

impl Hash for GreenNodeHash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner_hash.hash(state);
    }
}

impl NodeCache {
    fn node<I>(&mut self, kind: SyntaxKind, children: I) -> GreenNode
    where
        I: ExactSizeIterator<Item = GreenElement>,
    {
        let num_children = children.len();
        const MAX_CHILDREN: usize = 3;
        // Green nodes are fully immutable, so it's ok to deduplicate them.
        // This is the same optimization that Roslyn does
        // https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees
        //
        // For example, all `#[inline]` in this file share the same green node!
        // For `libsyntax/parse/parser.rs`, measurements show that deduping saves
        // 17% of the memory for green nodes!
        // Future work: make hashing faster by avoiding rehashing of subtrees.
        if num_children <= MAX_CHILDREN {
            let mut hash = GreenNodeHash::new(kind);
            let collected_children: SmallVec<[GreenElement; MAX_CHILDREN]> = children.collect();
            for child in collected_children.iter() {
                hash.add_child(child);
            }
            match self.nodes.get(&hash) {
                Some(existing) => existing.clone(),
                None => {
                    let node = GreenNode::new(kind, collected_children.into_iter());
                    self.nodes.insert(hash, node.clone());
                    node
                }
            }
        } else {
            GreenNode::new(kind, children)
        }
    }

    fn token(&mut self, kind: SyntaxKind, text: SmolStr) -> GreenToken {
        let token_data = GreenTokenData::new(kind, text.clone());

        match self.tokens.get(&token_data) {
            Some(existing) => existing.clone(),
            None => {
                let token = GreenToken::new(kind, text.clone());
                self.tokens.insert(token_data, token.clone());
                token
            }
        }
    }
}

#[derive(Debug)]
enum MaybeOwned<'a, T> {
    Owned(T),
    Borrowed(&'a mut T),
}

impl<T> std::ops::Deref for MaybeOwned<'_, T> {
    type Target = T;
    fn deref(&self) -> &T {
        match self {
            MaybeOwned::Owned(it) => it,
            MaybeOwned::Borrowed(it) => *it,
        }
    }
}

impl<T> std::ops::DerefMut for MaybeOwned<'_, T> {
    fn deref_mut(&mut self) -> &mut T {
        match self {
            MaybeOwned::Owned(it) => it,
            MaybeOwned::Borrowed(it) => *it,
        }
    }
}

impl<T: Default> Default for MaybeOwned<'_, T> {
    fn default() -> Self {
        MaybeOwned::Owned(T::default())
    }
}

/// A checkpoint for maybe wrapping a node. See `GreenNodeBuilder::checkpoint` for details.
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(usize);

/// A builder for a green tree.
#[derive(Default, Debug)]
pub struct GreenNodeBuilder<'cache> {
    cache: MaybeOwned<'cache, NodeCache>,
    parents: Vec<(SyntaxKind, usize)>,
    children: Vec<GreenElement>,
}

impl GreenNodeBuilder<'_> {
    /// Creates new builder.
    pub fn new() -> GreenNodeBuilder<'static> {
        GreenNodeBuilder::default()
    }

    /// Reusing `NodeCache` between different `GreenNodeBuilder`s saves memory.
    /// It allows to structurally share underlying trees.
    pub fn with_cache(cache: &mut NodeCache) -> GreenNodeBuilder<'_> {
        GreenNodeBuilder {
            cache: MaybeOwned::Borrowed(cache),
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    /// Adds new token to the current branch.
    #[inline]
    pub fn token(&mut self, kind: SyntaxKind, text: SmolStr) {
        let token = self.cache.token(kind, text);
        self.children.push(token.into());
    }

    /// Start new node and make it current.
    #[inline]
    pub fn start_node(&mut self, kind: SyntaxKind) {
        let len = self.children.len();
        self.parents.push((kind, len));
    }

    /// Finish current branch and restore previous
    /// branch as current.
    #[inline]
    pub fn finish_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let children = self.children.drain(first_child..);
        let node = self.cache.node(kind, children);
        self.children.push(node.into());
    }

    /// Prepare for maybe wrapping the next node.
    /// The way wrapping works is that you first of all get a checkpoint,
    /// then you place all tokens you want to wrap, and then *maybe* call
    /// `start_node_at`.
    /// Example:
    /// ```rust
    /// # use rowan::{GreenNodeBuilder, SyntaxKind};
    /// # const PLUS: SyntaxKind = SyntaxKind(0);
    /// # const OPERATION: SyntaxKind = SyntaxKind(1);
    /// # struct Parser;
    /// # impl Parser {
    /// #     fn peek(&self) -> Option<SyntaxKind> { None }
    /// #     fn parse_expr(&mut self) {}
    /// # }
    /// # let mut builder = GreenNodeBuilder::new();
    /// # let mut parser = Parser;
    /// let checkpoint = builder.checkpoint();
    /// parser.parse_expr();
    /// if parser.peek() == Some(PLUS) {
    ///   // 1 + 2 = Add(1, 2)
    ///   builder.start_node_at(checkpoint, OPERATION);
    ///   parser.parse_expr();
    ///   builder.finish_node();
    /// }
    /// ```
    #[inline]
    pub fn checkpoint(&self) -> Checkpoint {
        Checkpoint(self.children.len())
    }

    /// Wrap the previous branch marked by `checkpoint` in a new branch and
    /// make it current.
    #[inline]
    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: SyntaxKind) {
        let Checkpoint(checkpoint) = checkpoint;
        assert!(
            checkpoint <= self.children.len(),
            "checkpoint no longer valid, was finish_node called early?"
        );

        if let Some(&(_, first_child)) = self.parents.last() {
            assert!(
                checkpoint >= first_child,
                "checkpoint no longer valid, was an unmatched start_node_at called?"
            );
        }

        self.parents.push((kind, checkpoint));
    }

    /// Complete tree building. Make sure that
    /// `start_node_at` and `finish_node` calls
    /// are paired!
    #[inline]
    pub fn finish(mut self) -> GreenNode {
        assert_eq!(self.children.len(), 1);
        match self.children.pop().unwrap() {
            NodeOrToken::Node(node) => node,
            NodeOrToken::Token(_) => panic!(),
        }
    }
}
