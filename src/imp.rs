use std::{mem, ptr, sync::Arc};

use crate::{swap_cell::SwapCell, GreenNode, TextUnit, Types};

#[derive(Debug)]
pub(crate) struct SyntaxRoot<T: Types> {
    pub(crate) root_node: SyntaxNode<T>,
    pub(crate) data: T::RootData,
}

#[derive(Debug)]
pub(crate) struct ParentData<T: Types> {
    parent: ptr::NonNull<SyntaxNode<T>>,
    pub(crate) start_offset: TextUnit,
    index_in_parent: u32,
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
    parent: Option<ParentData<T>>,
    pub(crate) green: GreenNode<T>,
    // replace with
    // `children: [(TextUnit, AtomSetOnce<SyntaxNode<T><T>>)]`
    // once we can have var-length structs.
    // Perhaps even fold `TextUnit` into ptr using a spare bit?
    children: Box<[SwapCell<TextUnit, SyntaxNode<T>>]>,
}

unsafe impl<T: Types> Send for SyntaxNode<T>
where
    T::RootData: Send,
    T::Kind: Send,
{
}

unsafe impl<T: Types> Sync for SyntaxNode<T>
where
    T::RootData: Sync,
    T::Kind: Sync,
{
}

/// Owned smart pointer for syntax Nodes.
/// It can be used with any type implementing `TransparentNewType<SyntaxNode>`.
pub struct TreePtr<T> {
    inner: *const T,
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
/// ```rust
/// unsafe impl TransparentNewType<Inner> for Wrapper {}
/// ```
///
/// Implementing this trait allows one to cast safely between the wrapper and
/// the underlying representation.
pub unsafe trait TransparentNewType<Repr>: Sized {
    fn from_repr(repr: &Repr) -> &Self {
        assert!(mem::size_of::<Self>() == mem::size_of::<Repr>());
        unsafe { mem::transmute(repr) }
    }
    fn into_repr(&self) -> &Repr {
        assert!(mem::size_of::<Self>() == mem::size_of::<Repr>());
        unsafe { mem::transmute(self) }
    }
}

unsafe impl<T> TransparentNewType<T> for T {
    fn from_repr(repr: &T) -> &T {
        repr
    }
    fn into_repr(&self) -> &T {
        self
    }
}

impl<T> TreePtr<T> {
    pub(crate) fn new<TY>(node: &T) -> TreePtr<T>
    where
        T: TransparentNewType<SyntaxNode<TY>>,
        TY: Types,
    {
        let node: &SyntaxNode<TY> = node.into_repr();
        let root: Arc<SyntaxRoot<TY>> = unsafe { Arc::from_raw(node.root) };
        std::mem::forget(Arc::clone(&root));
        std::mem::forget(root);
        TreePtr {
            inner: T::from_repr(node) as *const T,
        }
    }
}

unsafe impl<T: Send> Send for TreePtr<T> {}

unsafe impl<T: Sync> Sync for TreePtr<T> {}

impl<T> std::ops::Deref for TreePtr<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { &*self.inner }
    }
}

impl<T: Types> Clone for TreePtr<SyntaxNode<T>> {
    fn clone(&self) -> TreePtr<SyntaxNode<T>> {
        TreePtr::new(&*self)
    }
}

impl<T: Types> SyntaxNode<T> {
    pub(crate) fn new_root(green: GreenNode<T>, data: T::RootData) -> TreePtr<SyntaxNode<T>> {
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
    pub(crate) fn start_offset(&self) -> TextUnit {
        match &self.parent {
            None => 0.into(),
            Some(p) => p.start_offset,
        }
    }

    pub(crate) fn n_children(&self) -> usize {
        self.green.children().len()
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

    pub(crate) fn index_in_parent(&self) -> Option<usize> {
        Some(self.parent.as_ref()?.index_in_parent as usize)
    }
}
