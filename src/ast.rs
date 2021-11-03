//! In rowan, syntax trees are transient objects.
//!
//! That means that we create trees when we need them, and tear them down to
//! save memory. In this architecture, hanging on to a particular syntax node
//! for a long time is ill-advisable, as that keeps the whole tree resident.
//!
//! Instead, we provide a [`SyntaxNodePtr`] type, which stores information about
//! *location* of a particular syntax node in a tree. Its a small type which can
//! be cheaply stored, and which can be resolved to a real [`SyntaxNode`] when
//! necessary.

use std::{
    fmt,
    hash::{Hash, Hasher},
    iter::successors,
};

use crate::{Language, SyntaxNode, TextRange};

/// The main trait to go from untyped `SyntaxNode` to a typed ast. The
/// conversion itself has zero runtime cost: ast and syntax nodes have exactly
/// the same representation: a pointer to the tree root and a pointer to the
/// node itself.
pub trait AstNode {
    type Language: Language;

    fn can_cast(kind: <Self::Language as Language>::Kind) -> bool
    where
        Self: Sized;

    fn cast(node: SyntaxNode<Self::Language>) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode<Self::Language>;

    fn clone_for_update(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_for_update()).unwrap()
    }

    fn clone_subtree(&self) -> Self
    where
        Self: Sized,
    {
        Self::cast(self.syntax().clone_subtree()).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNodePtr<L: Language> {
    kind: L::Kind,
    range: TextRange,
}

impl<L: Language> SyntaxNodePtr<L> {
    pub fn new(node: &SyntaxNode<L>) -> Self {
        Self { kind: node.kind(), range: node.text_range() }
    }

    /// "Dereference" the pointer to get the node it points to.
    ///
    /// Panics if node is not found, so make sure that `root` syntax tree is
    /// equivalent (is build from the same text) to the tree which was
    /// originally used to get this [`SyntaxNodePtr`].
    ///
    /// The complexity is linear in the depth of the tree and logarithmic in
    /// tree width. As most trees are shallow, thinking about this as
    /// `O(log(N))` in the size of the tree is not too wrong!
    pub fn to_node(&self, root: &SyntaxNode<L>) -> SyntaxNode<L> {
        assert!(root.parent().is_none());
        successors(Some(root.clone()), |node| {
            node.child_or_token_at_range(self.range).and_then(|it| it.into_node())
        })
        .find(|it| it.text_range() == self.range && it.kind() == self.kind)
        .unwrap_or_else(|| panic!("can't resolve local ptr to SyntaxNode: {:?}", self))
    }

    pub fn cast<N: AstNode<Language = L>>(self) -> Option<AstPtr<N>> {
        if !N::can_cast(self.kind) {
            return None;
        }
        Some(AstPtr { raw: self })
    }
}

/// Like `SyntaxNodePtr`, but remembers the type of node
pub struct AstPtr<N: AstNode> {
    raw: SyntaxNodePtr<N::Language>,
}

impl<N: AstNode> AstPtr<N> {
    pub fn new(node: &N) -> Self {
        Self { raw: SyntaxNodePtr::new(node.syntax()) }
    }

    pub fn to_node(&self, root: &SyntaxNode<N::Language>) -> N {
        N::cast(self.raw.to_node(root)).unwrap()
    }

    pub fn syntax_node_ptr(&self) -> SyntaxNodePtr<N::Language> {
        self.raw.clone()
    }

    pub fn cast<U: AstNode<Language = N::Language>>(self) -> Option<AstPtr<U>> {
        if !U::can_cast(self.raw.kind) {
            return None;
        }
        Some(AstPtr { raw: self.raw })
    }
}

impl<N: AstNode> fmt::Debug for AstPtr<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("AstPtr").field("raw", &self.raw).finish()
    }
}

impl<N: AstNode> Clone for AstPtr<N> {
    fn clone(&self) -> Self {
        Self { raw: self.raw.clone() }
    }
}

impl<N: AstNode> PartialEq for AstPtr<N> {
    fn eq(&self, other: &AstPtr<N>) -> bool {
        self.raw == other.raw
    }
}

impl<N: AstNode> Eq for AstPtr<N> {}

impl<N: AstNode> Hash for AstPtr<N> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state)
    }
}

impl<N: AstNode> From<AstPtr<N>> for SyntaxNodePtr<N::Language> {
    fn from(ptr: AstPtr<N>) -> SyntaxNodePtr<N::Language> {
        ptr.raw
    }
}
