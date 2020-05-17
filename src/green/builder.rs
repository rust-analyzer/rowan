use rustc_hash::FxHasher;

use super::token::GreenTokenData;
use crate::{
    green::{GreenElement, GreenNode, GreenToken, SyntaxKind},
    NodeOrToken, SmolStr,
};
use lru::LruCache;
use smallvec::SmallVec;
use std::{
    cell::RefCell,
    fmt::Debug,
    hash::{BuildHasherDefault, Hash, Hasher},
};

pub struct NodeCache {
    nodes: LruCache<GreenNodeHash, GreenNode, FxHasherDefault>,
    tokens: LruCache<GreenTokenData, GreenToken, FxHasherDefault>,
}

// FIXME: What is a good value for this?
const MAX_CACHED: usize = 650000;

type FxHasherDefault = BuildHasherDefault<FxHasher>;

impl NodeCache {
    pub(crate) fn new() -> Self {
        Self {
            nodes: LruCache::with_hasher(MAX_CACHED, FxHasherDefault::default()),
            tokens: LruCache::with_hasher(MAX_CACHED, FxHasherDefault::default()),
        }
    }
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
            let mut collected_children =
                SmallVec::<[GreenElement; MAX_CHILDREN]>::with_capacity(MAX_CHILDREN);
            for child in children {
                collected_children.push(child);
            }
            for child in collected_children.iter() {
                hash.add_child(child);
            }
            match self.nodes.get(&hash) {
                Some(existing) => existing.clone(),
                None => {
                    let node = GreenNode::new(kind, collected_children.into_iter());
                    self.nodes.put(hash, node.clone());
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
                self.tokens.put(token_data, token.clone());
                token
            }
        }
    }
}

/// A checkpoint for maybe wrapping a node. See `GreenNodeBuilder::checkpoint` for details.
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(usize);

/// A builder for a green tree.
#[derive(Debug, Default)]
pub struct GreenNodeBuilder {
    parents: Vec<(SyntaxKind, usize)>,
    children: Vec<GreenElement>,
}

// FIXME: LruCache is Sync so perhaps this is better made global? That will increase reuse but also contention
thread_local! {
    static CACHE: RefCell<NodeCache> = RefCell::new(NodeCache::new())
}

impl GreenNodeBuilder {
    /// Creates new builder.
    pub fn new() -> GreenNodeBuilder {
        Self { parents: Vec::default(), children: Vec::default() }
    }

    /// Adds new token to the current branch.
    #[inline]
    pub fn token(&mut self, kind: SyntaxKind, text: SmolStr) {
        CACHE.with(|f| {
            let mut cache = f.borrow_mut();
            let token = cache.token(kind, text);
            self.children.push(token.into());
        })
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
        CACHE.with(|f| {
            let (kind, first_child) = self.parents.pop().unwrap();
            let children = self.children.drain(first_child..);
            let mut cache = f.borrow_mut();
            let node = cache.node(kind, children);
            self.children.push(node.into());
        })
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
