use std::{mem, ptr, sync::Arc};

use crate::{swap_cell::SwapCell, GreenNode, TextUnit, Types};

#[derive(Debug)]
pub(crate) struct SyntaxRoot<T: Types> {
    pub(crate) root_node: SyntaxNode<T>,
    pub(crate) data: T::RootData,
}

#[derive(Debug)]
pub(crate) struct ParentData<T: Types> {
    pub(crate) parent: ptr::NonNull<SyntaxNode<T>>,
    pub(crate) start_offset: TextUnit,
    pub(crate) index_in_parent: u32,
}

/// An immutable lazy constructed syntax tree with
/// offsets and parent pointers.
///
/// The design is close to
/// https://github.com/apple/swift/tree/bc3189a2d265bf7728ea0cfeb55f032bfe5beaf1/lib/Syntax
///
/// `SyntaxNode` exists in two flavors:
///   * owned (R = OwnedRoot<T>)
///   * borrowed (R = RefRoot<'a, T>)
///
/// Borrowed `SyntaxNode` is `Copy`, but is parametrized over a lifetime,
/// with a corresponding ergonomics hit.
///
/// Owned `SyntaxNode` is `Clone` (using `Arc::clone` under the hood) and
/// is not parametrized over a lifetime. Note that because of the parent
/// links `SyntaxNode` keeps all of its ancestors alive, and not only descendants,
/// so keep an eye on memory leaks.
///
/// Methods like `parent` or `children` preserve the flavor (borrowed or owned)
/// of nodes, but you can switch between them at any time using `.borrowed()`
/// and `.owned()` methods. As a rule of thumb, when *processing* nodes, use
/// borrowed version to avoid excessive Arc traffic, and, when *storing* nodes
/// in data structures, use owned variant, to avoid dealing with lifetimes.
///
/// `SyntaxNode` have object identity equality and hash semantics.
pub struct SyntaxNode<T: Types> {
    root: *const SyntaxRoot<T>, // created from `Arc`
    pub(crate) parent: Option<ParentData<T>>,
    pub(crate) green: GreenNode<T>,
    // replace with
    // `children: [(TextUnit, AtomSetOnce<SyntaxNode<T><T>>)]`
    // once we can have var-length structs.
    // Perhaps even fold `TextUnit` into ptr using a spare bit?
    pub(crate) children: Box<[SwapCell<TextUnit, SyntaxNode<T>>]>,
}

unsafe impl<T> Send for SyntaxNode<T>
where
    T: Types,
    SyntaxNode<T>: Send,
{
}

unsafe impl<T> Sync for SyntaxNode<T>
where
    T: Types,
    SyntaxNode<T>: Send,
{
}

/// Owned smart pointer for syntax Nodes.
/// It can be used with any type implementing `TransparentNewType<SyntaxNode>`.
pub struct TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
{
    inner: *const N,
}

/// A marker trait for transparent newtypes.
///
/// If you declare a struct like
///
/// ```no-run
/// #[repr(transparent)]
/// struct Wrapper(Inner)
/// ```
///
/// it is safe to add
///
/// ```no-run
/// unsafe impl TransparentNewType for Wrapper { type Repr = Inner; }
/// ```
///
/// Implementing this trait allows one to cast safely between the wrapper and
/// the underlying representation.
pub unsafe trait TransparentNewType: Sized {
    /// Underlyng representation of a newtype.
    type Repr;
    /// Cast the uderlying repr into a wrapper.
    fn from_repr(repr: &Self::Repr) -> &Self {
        assert!(mem::size_of::<Self>() == mem::size_of::<Self::Repr>());
        unsafe { mem::transmute(repr) }
    }
    /// Cast wrapper to the underlying repr.
    fn into_repr(&self) -> &Self::Repr {
        assert!(mem::size_of::<Self>() == mem::size_of::<Self::Repr>());
        unsafe { mem::transmute(self) }
    }
}

unsafe impl<T: Types> TransparentNewType for SyntaxNode<T> {
    type Repr = SyntaxNode<T>;

    fn from_repr(repr: &Self::Repr) -> &Self {
        repr
    }

    fn into_repr(&self) -> &Self::Repr {
        self
    }
}

impl<T, N> TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
{
    pub(crate) fn new(node: &N) -> TreePtr<T, N> {
        let node: &SyntaxNode<T> = node.into_repr();
        let root: Arc<SyntaxRoot<T>> = unsafe { Arc::from_raw(node.root) };
        std::mem::forget(Arc::clone(&root));
        std::mem::forget(root);
        TreePtr {
            inner: N::from_repr(node) as *const N,
        }
    }

    /// Casts this ptr across equivalent reprs.
    pub fn cast<U>(this: TreePtr<T, N>) -> TreePtr<T, U>
    where
        U: TransparentNewType<Repr = SyntaxNode<T>>,
    {
        let r: &SyntaxNode<T> = this.into_repr();
        let n = U::from_repr(r);
        TreePtr::new(n)
    }
}

impl<T, N> Drop for TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
{
    fn drop(&mut self) {
        let node: &N = &*self;
        let node = node.into_repr();
        unsafe { Arc::from_raw(node.root) };
    }
}

unsafe impl<T, N> Send for TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>> + Send,
{
}

unsafe impl<T: Sync, N> Sync for TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>> + Sync,
{
}

impl<T, N> std::ops::Deref for TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
{
    type Target = N;
    fn deref(&self) -> &N {
        unsafe { &*self.inner }
    }
}

impl<T: Types> SyntaxNode<T> {
    pub(crate) fn new_root(green: GreenNode<T>, data: T::RootData) -> TreePtr<T, SyntaxNode<T>> {
        let root = Arc::new(SyntaxRoot {
            root_node: SyntaxNode::new_impl(ptr::null(), None, green),
            data,
        });
        let red_node = &root.root_node as *const SyntaxNode<T>;
        let root_ptr: *const SyntaxRoot<T> = Arc::into_raw(root);
        unsafe {
            (*(red_node as *mut SyntaxNode<T>)).root = root_ptr;
        }
        TreePtr { inner: red_node }
    }

    fn new_child(
        root: *const SyntaxRoot<T>,
        parent: ptr::NonNull<SyntaxNode<T>>,
        green: GreenNode<T>,
        start_offset: TextUnit,
        index_in_parent: usize,
    ) -> SyntaxNode<T> {
        let parent_data = ParentData {
            parent,
            start_offset,
            index_in_parent: index_in_parent as u32,
        };
        SyntaxNode::new_impl(root, Some(parent_data), green)
    }

    fn new_impl(
        root: *const SyntaxRoot<T>,
        parent: Option<ParentData<T>>,
        green: GreenNode<T>,
    ) -> SyntaxNode<T> {
        let mut start_offset = parent
            .as_ref()
            .map(|it| it.start_offset)
            .unwrap_or(0.into());
        let children = green
            .children()
            .iter()
            .map(|child| {
                let off = start_offset;
                start_offset += child.text_len();
                SwapCell::new(off)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        SyntaxNode {
            root,
            parent,
            green,
            children,
        }
    }

    pub(crate) fn root(&self) -> &SyntaxRoot<T> {
        unsafe { &*self.root }
    }

    pub(crate) fn parent_impl(&self) -> Option<&SyntaxNode<T>> {
        let ptr = self.parent.as_ref()?.parent;
        Some(unsafe { &*ptr.as_ptr() })
    }

    pub(crate) fn get_child(&self, idx: usize) -> Option<&SyntaxNode<T>> {
        if idx >= self.n_children() {
            return None;
        }
        let child = self.children[idx].get_or_init(|start_offset| {
            let green_children = self.green.children();
            SyntaxNode::new_child(
                self.root,
                ptr::NonNull::from(self),
                green_children[idx].clone(),
                start_offset,
                idx,
            )
        });
        Some(child)
    }
}
