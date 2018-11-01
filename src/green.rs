use std::sync::Arc;

use crate::{SmolStr, TextUnit, Types};

/// `GreenNode` is an immutable syntax tree,
/// which is cheap to update. It lacks parent
/// pointers and information about offsets.
#[derive(Debug)]
pub struct GreenNode<T: Types>(GreenNodeImpl<T>);

impl<T: Types> Clone for GreenNode<T> {
    fn clone(&self) -> GreenNode<T> {
        GreenNode(match &self.0 {
            GreenNodeImpl::Leaf { kind, text } => GreenNodeImpl::Leaf {
                kind: *kind,
                text: text.clone(),
            },
            GreenNodeImpl::Branch(branch) => GreenNodeImpl::Branch(Arc::clone(branch)),
        })
    }
}

/// A checkpoint for maybe wrapping a node. See `GreenNodeBuilder::wrap_checkpoint` for details.
#[derive(Clone, Copy, Debug)]
pub struct WrapCheckpoint(usize);

#[derive(Debug)]
enum GreenNodeImpl<T: Types> {
    Leaf { kind: T::Kind, text: SmolStr },
    Branch(Arc<GreenBranch<T>>),
}

/// A builder for a green tree.
#[derive(Debug)]
pub struct GreenNodeBuilder<T: Types> {
    parents: Vec<(T::Kind, usize)>,
    children: Vec<GreenNode<T>>,
}

impl<T: Types> GreenNodeBuilder<T> {
    /// Creates new builder.
    pub fn new() -> Self {
        GreenNodeBuilder {
            parents: Vec::new(),
            children: Vec::new(),
        }
    }
    /// Adds new leaf to the current branch.
    pub fn leaf(&mut self, kind: T::Kind, text: SmolStr) {
        self.children.push(GreenNode::new_leaf(kind, text));
    }
    /// Start new branch and make it current.
    pub fn start_internal(&mut self, kind: T::Kind) {
        let len = self.children.len();
        self.parents.push((kind, len));
    }
    /// Finish current branch and restore previous
    /// branch as current.
    pub fn finish_internal(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let children: Vec<_> = self.children.drain(first_child..).collect();
        self.children
            .push(GreenNode::new_branch(kind, children.into_boxed_slice()));
    }
    /// Prepare for maybe wrapping the next node.
    /// The way wrapping works is that you first of all get a checkpoint,
    /// then you place all tokens you want to wrap, and then *maybe* call start_internal_at.
    /// Example:
    /// ```rust,ignore
    /// let checkpoint = builder.wrap_checkpoint();
    /// self.parse_expr();
    /// if self.peek() == Some(Token::Plus) {
    ///   // 1 + 2 = Add(1, 2)
    ///   builder.start_internal_at(checkpoint, Token::Operation);
    ///   self.parse_expr();
    ///   builder.finish_internal();
    /// }
    /// ```
    pub fn wrap_checkpoint(&self) -> WrapCheckpoint {
        WrapCheckpoint(self.children.len())
    }
    /// Wrap the previous branch marked by wrap_checkpoint in a new branch and
    /// make it current.
    pub fn start_internal_at(&mut self, checkpoint: WrapCheckpoint, kind: T::Kind) {
        let WrapCheckpoint(checkpoint) = checkpoint;
        assert!(checkpoint <= self.children.len(), "checkpoint no longer valid, was finish_internal called early?");

        if let Some(&(_, first_child)) = self.parents.last() {
            assert!(checkpoint >= first_child, "checkpoint no longer valid, was an unmatched start_internal called?");
        }

        self.parents.push((kind, checkpoint));
    }
    /// Complete tree building. Make sure that
    /// `start_internal` and `finish_internal` calls
    /// are paired!
    pub fn finish(mut self) -> GreenNode<T> {
        assert_eq!(self.children.len(), 1);
        self.children.pop().unwrap()
    }
}

impl<T: Types> GreenNode<T> {
    /// Creates new leaf green node.
    pub fn new_leaf(kind: T::Kind, text: SmolStr) -> GreenNode<T> {
        GreenNode(GreenNodeImpl::Leaf { kind, text })
    }
    /// Creates new branch green node.
    pub fn new_branch(kind: T::Kind, children: Box<[GreenNode<T>]>) -> GreenNode<T> {
        GreenNode(GreenNodeImpl::Branch(Arc::new(GreenBranch::new(
            kind, children,
        ))))
    }
    /// Kind of this node.
    pub fn kind(&self) -> T::Kind {
        match &self.0 {
            GreenNodeImpl::Leaf { kind, .. } => *kind,
            GreenNodeImpl::Branch(b) => b.kind(),
        }
    }
    /// Length of the text, covered by this node.
    pub fn text_len(&self) -> TextUnit {
        match &self.0 {
            GreenNodeImpl::Leaf { text, .. } => TextUnit::from(text.len() as u32),
            GreenNodeImpl::Branch(b) => b.text_len(),
        }
    }
    /// Children of this node, empty for leaves.
    pub fn children(&self) -> &[GreenNode<T>] {
        match &self.0 {
            GreenNodeImpl::Leaf { .. } => &[],
            GreenNodeImpl::Branch(b) => b.children(),
        }
    }
    /// Text of this node, if it is a leaf, and
    /// None otherwise.
    pub fn leaf_text(&self) -> Option<&SmolStr> {
        match &self.0 {
            GreenNodeImpl::Leaf { text, .. } => Some(text),
            GreenNodeImpl::Branch(_) => None,
        }
    }
}

#[derive(Debug)]
struct GreenBranch<T: Types> {
    text_len: TextUnit,
    kind: T::Kind,
    children: Box<[GreenNode<T>]>,
}

impl<T: Types> GreenBranch<T> {
    fn new(kind: T::Kind, children: Box<[GreenNode<T>]>) -> GreenBranch<T> {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        GreenBranch {
            text_len,
            kind,
            children,
        }
    }
    fn kind(&self) -> T::Kind {
        self.kind
    }
    fn text_len(&self) -> TextUnit {
        self.text_len
    }
    fn children(&self) -> &[GreenNode<T>] {
        &*self.children
    }
}
