//! All tricky and unsafe bits of implementation are here. If you get a
//! segfault, this the module to scrutinize in the first place :-)

use std::{
    mem,
    panic::{RefUnwindSafe, UnwindSafe},
    ptr,
    hash::{Hash, Hasher},
    sync::Arc,
    any::Any,
};

use crate::{swap_cell::SwapCell, GreenElement, GreenNode, GreenIndex, TextUnit};
use colosseum::sync::Arena;

type LazyNode = SwapCell<(TextUnit, GreenIndex), SyntaxNode>;

pub(crate) struct SyntaxRoot {
    arena: Arena<LazyNode>,
    pub(crate) data: Option<Box<dyn Any + Send + Sync>>,
}

#[derive(Debug)]
pub(crate) struct ParentData {
    parent: ptr::NonNull<SyntaxNode>,
    pub(crate) start_offset: TextUnit,
    pub(crate) index_in_parent: SyntaxIndex,
    pub(crate) index_in_green: GreenIndex,
}

/// An immutable lazy constructed syntax tree with offsets and parent pointers.
///
/// The design is close to
/// https://github.com/apple/swift/tree/bc3189a2d265bf7728ea0cfeb55f032bfe5beaf1/lib/Syntax
///
/// All nodes constituting a tree share the ownership by a tree. Internally, and
/// `Arc` is used, but outside world observes nodes as `&SyntaxNode` or
/// `TreeArc<SyntaxNode>`, where a `TreeArc` is an Arc-like smart pointer.
pub struct SyntaxNode {
    // Created from `Arc`. The ref-count on root is equal to the number of live
    // `TreeArc` which point into this tree.
    root: *const SyntaxRoot,
    parent_data: Option<ParentData>,
    pub(crate) green: GreenNode,
    // replace with
    // `children: [(TextUnit, AtomSetOnce<SyntaxNode>)]`
    // once we can have var-length structs.
    // Perhaps even fold `TextUnit` into ptr using a spare bit?
    children: ptr::NonNull<[LazyNode]>,
}

// Manually send/sync impls due to `root: *const SyntaxRoot` field.
unsafe impl Send for SyntaxNode {}
unsafe impl Sync for SyntaxNode {}
impl UnwindSafe for SyntaxNode {}
impl RefUnwindSafe for SyntaxNode {}

impl ToOwned for SyntaxNode {
    type Owned = TreeArc<SyntaxNode>;
    fn to_owned(&self) -> TreeArc<SyntaxNode> {
        TreeArc::new(self)
    }
}

/// Index into a syntax node.
/// Unlike GreenIndex, it can only refer to Nodes,
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct SyntaxIndex(pub(crate) u32);

impl SyntaxIndex {
    pub(crate) fn prev(self) -> SyntaxIndex {
        SyntaxIndex(self.0.wrapping_sub(1))
    }
    pub(crate) fn next(self) -> SyntaxIndex {
        SyntaxIndex(self.0 + 1)
    }
}

/// Owned smart pointer for syntax Nodes.
/// It can be used with any type implementing `TransparentNewType<SyntaxNode>`.
// Conceptually, it also plays an `Arc<TreeRoot>` role as well.
pub struct TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    inner: *const N,
}

impl<N> Clone for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    fn clone(&self) -> TreeArc<N> {
        let n: &N = &*self;
        TreeArc::new(n)
    }
}

impl<N> PartialEq<TreeArc<N>> for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    fn eq(&self, other: &TreeArc<N>) -> bool {
        ptr::eq(self.inner, other.inner)
    }
}

impl<N> Eq for TreeArc<N> where N: TransparentNewType<Repr = SyntaxNode> {}

impl<N> Hash for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner.hash(state)
    }
}

impl<N> TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    /// Creates a new owned node from a reference to a node, by bumping root's
    /// refcount.
    fn new(node: &N) -> TreeArc<N> {
        let node: &SyntaxNode = node.into_repr();
        let root: Arc<SyntaxRoot> = unsafe { Arc::from_raw(node.root) };
        std::mem::forget(Arc::clone(&root));
        std::mem::forget(root);
        TreeArc { inner: N::from_repr(node) as *const N }
    }

    /// Casts this ptr across equivalent reprs.
    pub fn cast<U>(this: TreeArc<N>) -> TreeArc<U>
    where
        U: TransparentNewType<Repr = SyntaxNode>,
    {
        // We can avoid an Arc bump if we want here, but lets leverage existing
        // `new`, to minimize unsafety.
        let r: &SyntaxNode = this.into_repr();
        let n = U::from_repr(r);
        TreeArc::new(n)
    }
}

impl<N> Drop for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    // Drop refcount
    fn drop(&mut self) {
        // Be careful not to leave dangling references to node.
        let root: *const SyntaxRoot = {
            let node: &N = &*self;
            let node = node.into_repr();
            node.root
        };
        drop(unsafe { Arc::from_raw(root) });
        // inner may be a dangling pointer at this point, but it is ok: it's a
        // raw pointer after all
    }
}

// Manual impls due to `inner: *const N` field
unsafe impl<N> Send for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
    N: Send,
{
}

unsafe impl<N> Sync for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
    N: Sync,
{
}

impl<N> std::ops::Deref for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    type Target = N;
    fn deref(&self) -> &N {
        // We are a `TreeArc`, so underlying `SyntaxRoot` has an count at least
        // one, so it's safe to deref a node.
        unsafe { &*self.inner }
    }
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
    /// Underlying representation of a newtype.
    type Repr;
    /// Cast the underlying repr into a wrapper.
    fn from_repr(repr: &Self::Repr) -> &Self {
        assert!(mem::size_of::<Self>() == mem::size_of::<Self::Repr>());
        unsafe { &*(repr as *const Self::Repr as *const Self) }
    }
    /// Cast wrapper to the underlying repr.
    fn into_repr(&self) -> &Self::Repr {
        assert!(mem::size_of::<Self>() == mem::size_of::<Self::Repr>());
        unsafe { &*(self as *const Self as *const Self::Repr) }
    }
}

unsafe impl TransparentNewType for SyntaxNode {
    type Repr = SyntaxNode;

    fn from_repr(repr: &Self::Repr) -> &Self {
        repr
    }

    fn into_repr(&self) -> &Self::Repr {
        self
    }
}

impl SyntaxNode {
    pub(crate) fn new_root(green: GreenNode, data: Option<Box<Any + Send + Sync>>) -> TreeArc<SyntaxNode> {
        let root = SyntaxRoot { arena: Arena::new(), data };
        let red_node: *mut SyntaxNode = {
            let red_node = root.arena.alloc(SwapCell::new((0.into(), GreenIndex(0))));
            red_node.get_or_init(|_| SyntaxNode::new_impl(&root, None, green));
            red_node.get_mut().unwrap()
        };

        // "forget" the `root` so that we have rc equal to one once exiting this
        // function.
        let root_ptr: *const SyntaxRoot = Arc::into_raw(Arc::new(root));
        // set backreference
        unsafe {
            (*red_node).root = root_ptr;
        }
        TreeArc { inner: red_node }
    }

    #[cold]
    fn new_child(
        &self,
        start_offset: TextUnit,
        index_in_parent: SyntaxIndex,
        index_in_green: GreenIndex,
        green: GreenNode,
    ) -> SyntaxNode {
        let parent_data = ParentData {
            parent: ptr::NonNull::from(self),
            start_offset,
            index_in_green,
            index_in_parent,
        };
        SyntaxNode::new_impl(self.root(), Some(parent_data), green)
    }

    fn new_impl(
        root: &SyntaxRoot,
        parent_data: Option<ParentData>,
        green: GreenNode,
    ) -> SyntaxNode {
        let mut start_offset = parent_data.as_ref().map(|it| it.start_offset).unwrap_or(0.into());

        let children = root.arena.alloc_extend(green.children().iter().enumerate().filter_map(
            |(index_in_green, element)| match element {
                GreenElement::Token(it) => {
                    start_offset += it.text_len();
                    None
                }
                GreenElement::Node(it) => {
                    let off = start_offset;
                    start_offset += it.text_len();
                    Some(SwapCell::new((off, GreenIndex(index_in_green as u32))))
                }
            },
        ));
        SyntaxNode { root, parent_data, green, children: children.into() }
    }

    pub(crate) fn root(&self) -> &SyntaxRoot {
        // If we (the reference) not dangle, then root must be alive as well.
        unsafe { &*self.root }
    }

    pub(crate) fn parent_data(&self) -> Option<&ParentData> {
        self.parent_data.as_ref()
    }

    pub(crate) fn parent_impl(&self) -> Option<&SyntaxNode> {
        // If we (the reference) not dangle, then parent must be alive as well.
        let ptr = self.parent_data()?.parent;
        Some(unsafe { &*ptr.as_ptr() })
    }

    pub(crate) fn children_len(&self) -> SyntaxIndex {
        SyntaxIndex(self.children_impl().len() as u32)
    }

    fn children_impl(&self) -> &[LazyNode] {
        // If we (the reference) not dangle, then SyntaxRoot is alive, and so is
        // the Arena where children are allocated.
        unsafe { &*self.children.as_ptr() }
    }

    pub(crate) fn get_child(&self, index_in_parent: SyntaxIndex) -> Option<&SyntaxNode> {
        let lazy_child = self.children_impl().get(index_in_parent.0 as usize)?;
        let child = lazy_child.get_or_init(|(start_offset, index_in_green)| {
            self.new_child(
                start_offset,
                index_in_parent,
                index_in_green,
                match self.green.get_child(index_in_green) {
                    Some(GreenElement::Node(it)) => it.clone(),
                    _ => unreachable!(),
                },
            )
        });
        Some(child)
    }

    /// Number of memory bytes of occupied by subtree rooted at `self`.
    pub(crate) fn memory_size_of_red_children(&self) -> usize {
        self.children_impl()
            .iter()
            .map(|it| {
                std::mem::size_of::<LazyNode>()
                    + it.get().map_or(0, |it| it.memory_size_of_red_children())
            })
            .sum()
    }
}
