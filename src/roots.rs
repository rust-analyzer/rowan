use std::{
    hash::{Hash, Hasher},
    ptr,
    sync::Arc,
};
use {red::RedNode, Types};

#[derive(Debug)]
#[doc(hidden)]
pub struct SyntaxRoot<T: Types> {
    pub(crate) red: RedNode<T>,
    pub(crate) data: T::RootData,
}

/// A marker trait to distinguish between owned and borrowed
/// trees. You don't need to implement it yourself,
/// use existing `OwnedRoot` or `RefRoot`.
pub trait TreeRoot<T: Types>: Clone + Send + Sync {
    #[doc(hidden)]
    fn borrowed(&self) -> RefRoot<T>;
    #[doc(hidden)]
    fn owned(&self) -> OwnedRoot<T>;

    #[doc(hidden)]
    fn syntax_root(&self) -> &SyntaxRoot<T>;
}

/// `TreeRoot` for owned flavor of `SyntaxNode`.
#[derive(Debug)]
pub struct OwnedRoot<T: Types>(pub(crate) Arc<SyntaxRoot<T>>);

/// `TreeRoot` for borrowed flavor of `SyntaxNode`.
// TODO: shared_from_this instead of double indirection
#[derive(Debug)]
pub struct RefRoot<'a, T: Types>(&'a OwnedRoot<T>);

impl<T: Types> Clone for OwnedRoot<T> {
    fn clone(&self) -> OwnedRoot<T> {
        OwnedRoot(Arc::clone(&self.0))
    }
}

impl<'a, T: Types> Clone for RefRoot<'a, T> {
    fn clone(&self) -> RefRoot<'a, T> {
        *self
    }
}

impl<'a, T: Types> Copy for RefRoot<'a, T> {}

impl<T: Types> TreeRoot<T> for OwnedRoot<T> {
    fn borrowed(&self) -> RefRoot<T> {
        RefRoot(&self)
    }
    fn owned(&self) -> OwnedRoot<T> {
        self.clone()
    }
    fn syntax_root(&self) -> &SyntaxRoot<T> {
        &*self.0
    }
}

impl<'a, T: Types> TreeRoot<T> for RefRoot<'a, T> {
    fn borrowed(&self) -> RefRoot<T> {
        *self
    }
    fn owned(&self) -> OwnedRoot<T> {
        self.0.clone()
    }
    fn syntax_root(&self) -> &SyntaxRoot<T> {
        self.0.syntax_root()
    }
}

impl<'a, T: Types> RefRoot<'a, T> {
    pub(crate) fn syntax_root(&self) -> &'a SyntaxRoot<T> {
        self.0.syntax_root()
    }
}

#[derive(Debug)]
pub(crate) struct RedPtr<T: Types>(ptr::NonNull<RedNode<T>>);

impl<T: Types> PartialEq for RedPtr<T> {
    fn eq(&self, other: &RedPtr<T>) -> bool {
        self.0 == other.0
    }
}

impl<T: Types> Eq for RedPtr<T> {}

impl<T: Types> Hash for RedPtr<T> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.hash(hasher)
    }
}

unsafe impl<T: Types> Send for RedPtr<T> {}
unsafe impl<T: Types> Sync for RedPtr<T> {}

impl<T: Types> Clone for RedPtr<T> {
    fn clone(&self) -> RedPtr<T> {
        *self
    }
}

impl<T: Types> Copy for RedPtr<T> {}

impl<T: Types> RedPtr<T> {
    pub(crate) fn new(red: &RedNode<T>) -> RedPtr<T> {
        RedPtr(red.into())
    }
    /// Safety: self.0 must be rooted(indirectly) by `_root`.
    pub(crate) unsafe fn get<'a>(self, _root: &'a SyntaxRoot<T>) -> &'a RedNode<T> {
        &*self.0.as_ptr()
    }
}
