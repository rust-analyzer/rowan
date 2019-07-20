use std::{mem::size_of, sync::Arc};

use crate::{SmolStr, TextUnit, cursor::SyntaxKind, NodeOrToken};

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenNode {
    kind: SyntaxKind,
    text_len: TextUnit,
    //TODO: implement llvm::trailing_objects trick
    children: Arc<[GreenElement]>,
}

impl GreenNode {
    /// Creates new Node.
    #[inline]
    pub fn new(kind: SyntaxKind, children: Box<[GreenElement]>) -> GreenNode {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        GreenNode { kind, text_len, children: children.into() }
    }

    /// Kind of this node.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Length of the text, covered by this node.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        self.text_len
    }
    /// Children of this node.
    #[inline]
    pub fn children(&self) -> &[GreenElement] {
        &self.children
    }
}

/// Leaf node in the immutable tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    kind: SyntaxKind,
    text: SmolStr,
}

impl GreenToken {
    /// Creates new Token.
    #[inline]
    pub fn new(kind: SyntaxKind, text: SmolStr) -> GreenToken {
        GreenToken { kind, text }
    }
    /// Kind of this Token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }
    /// Text of this Token.
    #[inline]
    pub fn text(&self) -> &SmolStr {
        &self.text
    }
    /// Text of this Token.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        TextUnit::from_usize(self.text.len())
    }
}

pub(crate) type GreenElement = NodeOrToken<GreenNode, GreenToken>;

impl From<GreenNode> for GreenElement {
    #[inline]
    fn from(node: GreenNode) -> GreenElement {
        NodeOrToken::Node(node)
    }
}

impl From<GreenToken> for GreenElement {
    #[inline]
    fn from(token: GreenToken) -> GreenElement {
        NodeOrToken::Token(token)
    }
}

impl GreenElement {
    /// Returns kind of this element.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        match self {
            NodeOrToken::Node(it) => it.kind(),
            NodeOrToken::Token(it) => it.kind(),
        }
    }
    /// Returns length of the text covered by this element.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        match self {
            NodeOrToken::Node(it) => it.text_len(),
            NodeOrToken::Token(it) => it.text_len(),
        }
    }
}

/// A checkpoint for maybe wrapping a node. See `GreenNodeBuilder::checkpoint` for details.
#[derive(Clone, Copy, Debug)]
pub struct Checkpoint(usize);

/// A builder for a green tree.
#[derive(Default, Debug)]
pub struct GreenNodeBuilder {
    cache: rustc_hash::FxHashSet<GreenNode>,
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
        let token = GreenToken { kind, text };
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
        let children: Vec<_> = self.children.drain(first_child..).collect();
        let mut node = GreenNode::new(kind, children.into_boxed_slice());
        // Green nodes are fully immutable, so it's ok to deduplicate them.
        // This is the same optimization that Roslyn does
        // https://github.com/KirillOsenkov/Bliki/wiki/Roslyn-Immutable-Trees
        //
        // For example, all `#[inline]` in this file share the same green node!
        // For `libsyntax/parse/parser.rs`, measurements show that deduping saves
        // 17% of the memory for green nodes!
        // Future work: make hashing faster by avoiding rehashing of subtrees.
        if node.children.len() <= 3 {
            match self.cache.get(&node) {
                Some(existing) => node = existing.clone(),
                None => assert!(self.cache.insert(node.clone())),
            }
        }
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
