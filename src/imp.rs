//! All tricky and unsafe bits of implementation are here. If you get a
//! segfault, this the module to scrutinze in the frist place :-)

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
    // Created from `Arc`. The ref-count on root is equal to the number of live
    // `TreePtr` which point into this tree.
    root: *const SyntaxRoot<T>,
    pub(crate) parent: Option<ParentData<T>>,
    pub(crate) green: GreenNode<T>,
    // replace with
    // `children: [(TextUnit, AtomSetOnce<SyntaxNode<T><T>>)]`
    // once we can have var-length structs.
    // Perhaps even fold `TextUnit` into ptr using a spare bit?
    pub(crate) children: Box<[SwapCell<TextUnit, SyntaxNode<T>>]>,
}

// Manualy send/sync impls due to `root: *const SyntaxRoot<T>` field.
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
// Conceptually, it also plays an `Arc<TreeRoot<T>>` role as well.
pub struct TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
{
    pub(crate) inner: *const N,
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
    /// Creates a new owned node from a reference to a node, by bumping root's
    /// refcount.
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
        // We can avoid an Arc bump if we want here, but lets leverage existing
        // `new`, to minimize unsafety.
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
    // Drop refcount
    fn drop(&mut self) {
        // Be careful not to leave dangling references to node.
        let root: *const SyntaxRoot<T> = {
            let node: &N = &*self;
            let node = node.into_repr();
            node.root
        };
        drop(unsafe { Arc::from_raw(root) });
        // inner may be a dangling pointer at this point, but it is okey: it's a
        // raw pointer after all
    }
}

// Manual impls due to `inner: *const N` field
unsafe impl<T, N> Send for TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
    N: Send,
{
}

unsafe impl<T: Sync, N> Sync for TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
    N: Sync,
{
}

impl<T, N> std::ops::Deref for TreePtr<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
{
    type Target = N;
    fn deref(&self) -> &N {
        // We are a `TreePtr`, so underlying `SyntaxRoot` has an count at least
        // one, so it's safe to deref a node.
        unsafe { &*self.inner }
    }
}

impl<T: Types> SyntaxNode<T> {
    pub(crate) fn new_root(green: GreenNode<T>, data: T::RootData) -> TreePtr<T, SyntaxNode<T>> {
        let mut root = Arc::new(SyntaxRoot {
            root_node: SyntaxNode::new_impl(ptr::null(), None, green),
            data,
        });

        let red_node: *mut SyntaxNode<T> = &mut Arc::get_mut(&mut root).unwrap().root_node;
        // "forget" the `root` so that we have rc equal to one once exiting this
        // function.
        let root_ptr: *const SyntaxRoot<T> = Arc::into_raw(root);
        // set backreference
        unsafe {
            (*red_node).root = root_ptr;
        }
        TreePtr { inner: red_node }
    }

    fn new_child(
        &self,
        start_offset: TextUnit,
        index_in_parent: usize,
        green: GreenNode<T>,
    ) -> SyntaxNode<T> {
        let parent_data = ParentData {
            parent: ptr::NonNull::from(self),
            start_offset,
            index_in_parent: index_in_parent as u32,
        };
        SyntaxNode::new_impl(self.root, Some(parent_data), green)
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
        // If we (the rerence) not dangle, then root must be alive as well.
        unsafe { &*self.root }
    }

    pub(crate) fn parent_impl(&self) -> Option<&SyntaxNode<T>> {
        // If we (the rerence) not dangle, then parent must be alive as well.
        let ptr = self.parent.as_ref()?.parent;
        Some(unsafe { &*ptr.as_ptr() })
    }

    pub(crate) fn get_child(&self, idx: usize) -> Option<&SyntaxNode<T>> {
        if idx >= self.n_children() {
            return None;
        }
        let child = self.children[idx].get_or_init(|start_offset| {
            let green_children = self.green.children();
            self.new_child(start_offset, idx, green_children[idx].clone())
        });
        Some(child)
    }
}
