use std::sync::Arc;

use super::*;
use crate::{cursor::SyntaxKind, NodeOrToken, SmolStr};

/// A checkpoint for maybe wrapping a node. See `GreenNodeBuilder::checkpoint` for details.
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(usize);

/// Green nodes are fully immutable, so it's ok to deduplicate them.
/// This is the same optimization that Roslyn does
/// https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees
///
/// For example, all `#[inline]` in this file share the same green node!
/// For `libsyntax/parse/parser.rs`, measurements show that deduping saves
/// 17% of the memory for green nodes!
/// Future work: make hashing faster by avoiding rehashing of subtrees.
#[derive(Default, Debug)]
struct Cache {
    node: rustc_hash::FxHashSet<Arc<GreenNode>>,
    token: rustc_hash::FxHashSet<Arc<GreenToken>>,
}

impl Cache {
    fn node(&mut self, mut node: Arc<GreenNode>) -> Arc<GreenNode> {
        // In lieu of a more sophisticated cache, we only cache "small" nodes.
        if node.children().len() <= 3 {
            match self.node.get(&node) {
                Some(cached) => node = cached.clone(),
                None => assert!(self.node.insert(node.clone())),
            }
        }
        node
    }

    fn token(&mut self, token: GreenToken) -> Arc<GreenToken> {
        match self.token.get(&token) {
            Some(token) => token.clone(),
            None => {
                let token = Arc::new(token);
                assert!(self.token.insert(token.clone()));
                token
            }
        }
    }
}

/// A builder for a green tree.
#[derive(Default, Debug)]
pub struct GreenNodeBuilder {
    cache: Cache,
    parents: Vec<(SyntaxKind, usize)>,
    children: Vec<GreenElement>,
}

impl GreenNodeBuilder {
    /// Creates new builder.
    #[inline]
    pub fn new() -> GreenNodeBuilder {
        GreenNodeBuilder::default()
    }

    /// Adds new token to the current branch.
    #[inline]
    pub fn token(&mut self, kind: SyntaxKind, text: SmolStr) {
        let token = self.cache.token(GreenToken::new(kind, text));
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
        let children: Box<[_]> = self.children.drain(first_child..).collect();
        let node = self.cache.node(GreenNode::new(kind, children));
        self.children.push(node.into());
    }

    /// Prepare for maybe wrapping the next node.
    /// The way wrapping works is that you first of all get a checkpoint,
    /// then you place all tokens you want to wrap, and then *maybe* call
    /// `start_node_at`.
    /// Example:
    /// ```rust
    /// # use rowan::{GreenNodeBuilder, cursor::SyntaxKind};
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
    pub fn finish(mut self) -> Arc<GreenNode> {
        assert_eq!(self.children.len(), 1);
        match self.children.pop().unwrap() {
            NodeOrToken::Node(node) => node,
            NodeOrToken::Token(_) => panic!(),
        }
    }
}
