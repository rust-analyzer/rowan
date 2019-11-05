use static_assertions::*;
use std::{
    alloc::{alloc, dealloc, handle_alloc_error, Layout},
    borrow::Borrow,
    convert::TryInto,
    fmt, hash, isize, mem,
    ops::Deref,
    ptr, slice,
    sync::atomic::{self, AtomicUsize, Ordering},
};

use super::*;
use crate::{cursor::SyntaxKind, TextUnit};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct GreenNodeHead {
    kind: SyntaxKind,
    text_len: TextUnit,
    child_len: u16,
}

assert_eq_size!(GreenNodeHead, u64);
assert_eq_align!(GreenNodeHead, u32);

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
///
/// # Safety
///
/// This struct is _not actually `Sized`_!
/// It is only safe as `&GreenNode`, `GreenElement`, or `ArcGreenNode`.
#[repr(C)]
pub struct GreenNode {
    head: GreenNodeHead,
    strong: AtomicUsize,
    children: [GreenElement; 0],
}

assert_eq_align!(GreenNode, usize);
assert_eq_size!(GreenNode, [u64; 2]);

impl Drop for GreenNode {
    fn drop(&mut self) {
        for child in self.children_mut() {
            unsafe { ptr::drop_in_place(child) }
        }
    }
}

impl fmt::Debug for GreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GreenNode")
            .field("head", &self.head)
            .field("strong", &self.strong)
            .field("children", &self.children())
            .finish()
    }
}

impl Eq for GreenNode {}
impl PartialEq for GreenNode {
    fn eq(&self, other: &GreenNode) -> bool {
        self.head == other.head && self.children().eq(other.children())
    }
}

impl hash::Hash for GreenNode {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.head.hash(state);
        self.children().iter().for_each(|it| it.hash(state));
    }
}

#[test]
#[cfg(not(miri))]
fn node_layout() {
    let node = unsafe { GreenNode::dummy() };
    assert_eq!(GreenNode::align(), mem::align_of_val(node));
    assert_eq!(GreenNode::size_for(0), mem::size_of_val(node));
    assert_eq!(GreenNode::layout_for(0), Layout::for_value(node));
    // FUTURE(ptr_offset_from: #41079): use `ptr::offset_from`
    assert_eq!(
        GreenNode::offset_of_strong(),
        &node.strong as *const _ as *const () as isize - node as *const _ as *const () as isize
    );
    assert_eq!(
        GreenNode::offset_of_head(),
        &node.head as *const _ as *const () as isize - node as *const _ as *const () as isize
    );
    assert_eq!(
        GreenNode::offset_of_children(),
        &node.children as *const _ as *const () as isize - node as *const _ as *const () as isize
    );
}

pub struct ArcGreenNode {
    raw: ptr::NonNull<GreenNode>,
}

unsafe impl Send for ArcGreenNode {}
unsafe impl Sync for ArcGreenNode {}

impl Drop for ArcGreenNode {
    // <https://github.com/rust-lang/rust/blob/2477e24/src/liballoc/sync.rs#L1195>
    #[inline]
    fn drop(&mut self) {
        unsafe {
            let raw = self.raw.as_mut();

            // Because `fetch_sub` is already atomic, we do not need to synchronize
            // with other threads unless we are going to delete the object.
            if raw.strong.fetch_sub(1, Ordering::Release) != 1 {
                return;
            }

            // This fence is needed to prevent reordering of use of the data and
            // deletion of the data.  Because it is marked `Release`, the decreasing
            // of the reference count synchronizes with this `Acquire` fence. This
            // means that use of the data happens before decreasing the reference
            // count, which happens before this fence, which happens before the
            // deletion of the data.
            //
            // As explained in the [Boost documentation][1],
            //
            // > It is important to enforce any possible access to the object in one
            // > thread (through an existing reference) to *happen before* deleting
            // > the object in a different thread. This is achieved by a "release"
            // > operation after dropping a reference (any access to the object
            // > through this reference must obviously happened before), and an
            // > "acquire" operation before deleting the object.
            //
            // In particular, while the contents of an Arc are usually immutable, it's
            // possible to have interior writes to something like a Mutex<T>. Since a
            // Mutex is not acquired when it is deleted, we can't rely on its
            // synchronization logic to make writes in thread A visible to a destructor
            // running in thread B.
            //
            // Also note that the Acquire fence here could probably be replaced with an
            // Acquire load, which could improve performance in highly-contended
            // situations. See [2].
            //
            // [1]: (www.boost.org/doc/libs/1_55_0/doc/html/atomic/usage_examples.html)
            // [2]: (https://github.com/rust-lang/rust/pull/41714)
            atomic::fence(Ordering::Acquire);

            // Non-inlined part of `drop`.
            unsafe fn drop_slow(raw: ptr::NonNull<GreenNode>) {
                let child_count = raw.as_ref().head.child_len as usize;
                ptr::drop_in_place(raw.as_ptr());
                atomic::fence(Ordering::Acquire);
                dealloc(raw.as_ptr().cast(), GreenNode::layout_for(child_count))
            }

            drop_slow(self.raw)
            // NB: as this is a copy of the stdlib code, we are also subject to rust-lang/rust#55005
        }
    }
}

impl fmt::Debug for ArcGreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl Eq for ArcGreenNode {}
impl PartialEq for ArcGreenNode {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl hash::Hash for ArcGreenNode {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl GreenNode {
    #[inline]
    fn alloc<I>(kind: SyntaxKind, text_len: TextUnit, children: I) -> ArcGreenNode
    where
        I: Iterator<Item = GreenElement> + ExactSizeIterator,
    {
        let child_len: u16 = children.len().try_into().expect("node children array too big");
        let strong = AtomicUsize::new(1);
        let head = GreenNodeHead { kind, text_len, child_len };
        let layout = GreenNode::layout_for(child_len as usize);

        unsafe {
            let ptr = ptr::NonNull::new(alloc(layout))
                .unwrap_or_else(|| handle_alloc_error(layout))
                .as_ptr();

            ptr::write(ptr.offset(GreenNode::offset_of_head()).cast(), head);
            ptr::write(ptr.offset(GreenNode::offset_of_strong()).cast(), strong);

            let mut fam = ptr.offset(GreenNode::offset_of_children()).cast::<GreenElement>();
            for child in children {
                ptr::write(fam, child);
                fam = fam.offset(1);
            }

            #[allow(clippy::cast_ptr_alignment)]
            ArcGreenNode::from_raw(ptr::NonNull::new_unchecked(ptr as *mut GreenNode))
        }
    }

    const fn align() -> usize {
        let head_align = mem::align_of::<GreenNodeHead>();
        let strong_align = mem::align_of::<AtomicUsize>();
        let tail_align = mem::align_of::<GreenElement>();
        // Get the max of [head_align, strong_align, tail_align] in a const-compatible way.
        [
            [head_align, strong_align][(head_align < strong_align) as usize], // true == 1usize
            [tail_align, strong_align][(tail_align < strong_align) as usize], // true == 1usize
        ][(head_align < tail_align) as usize] // true == 1usize
    }

    const fn size_for(child_count: usize) -> usize {
        let tail_offset = GreenNode::offset_of_children() as usize;
        let tail_size = mem::size_of::<GreenElement>() * child_count;
        tail_offset + tail_size + tail_size % GreenNode::align()
    }

    const fn layout_for(child_count: usize) -> Layout {
        unsafe {
            Layout::from_size_align_unchecked(GreenNode::size_for(child_count), GreenNode::align())
        }
    }

    const fn offset_of_head() -> isize {
        0
    }

    const fn offset_of_strong() -> isize {
        let head_size = mem::size_of::<GreenNodeHead>() as isize;
        let strong_align = mem::align_of::<AtomicUsize>() as isize;
        GreenNode::offset_of_head() + head_size + head_size % strong_align
    }

    const fn offset_of_children() -> isize {
        let strong_size = mem::size_of::<AtomicUsize>() as isize;
        let tail_align = mem::align_of::<GreenElement>() as isize;
        GreenNode::offset_of_strong() + strong_size + strong_size % tail_align
    }
}

impl GreenNode {
    /// A dummy `GreenNode` for use as a placeholder.
    ///
    /// # Safety
    ///
    /// You _can not_ upgrade this reference to `ArcGreenNode`.
    /// It is a fully valid `GreenNode` otherwise.
    #[cfg(test)]
    pub(crate) unsafe fn dummy() -> &'static GreenNode {
        static HEAD: [u64; 8] = [0; 8];
        // produce a zeroed GreenNodeHead with no children
        &*(&HEAD as *const u64 as *const GreenNode)
    }

    pub(crate) const fn dangling() -> ptr::NonNull<GreenNode> {
        unsafe { ptr::NonNull::new_unchecked(GreenNode::align() as *mut _) }
    }

    /// Creates new Node.
    #[inline]
    #[allow(clippy::new_ret_no_self)]
    pub fn new(kind: SyntaxKind, children: Vec<GreenElement>) -> ArcGreenNode {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        GreenNode::alloc(kind, text_len, children.into_iter())
    }

    /// Kind of this node.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.head.kind
    }

    /// Length of the text, covered by this node.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        self.head.text_len
    }

    /// Children of this node.
    #[inline]
    pub fn children(&self) -> &[GreenElement] {
        unsafe { slice::from_raw_parts(self.children.as_ptr(), self.head.child_len as usize) }
    }

    fn children_mut(&mut self) -> &mut [GreenElement] {
        unsafe {
            slice::from_raw_parts_mut(self.children.as_mut_ptr(), self.head.child_len as usize)
        }
    }
}

impl ArcGreenNode {
    pub(super) fn into_raw(self) -> ptr::NonNull<GreenNode> {
        let raw = self.raw;
        mem::forget(self);
        raw
    }

    pub(super) unsafe fn from_raw(raw: ptr::NonNull<GreenNode>) -> ArcGreenNode {
        ArcGreenNode { raw }
    }
}

impl Deref for ArcGreenNode {
    type Target = GreenNode;
    fn deref(&self) -> &GreenNode {
        unsafe { self.raw.as_ref() }
    }
}

impl Borrow<GreenNode> for ArcGreenNode {
    fn borrow(&self) -> &GreenNode {
        &**self
    }
}

impl AsRef<GreenNode> for ArcGreenNode {
    fn as_ref(&self) -> &GreenNode {
        &**self
    }
}

impl Clone for ArcGreenNode {
    // <https://github.com/rust-lang/rust/blob/2477e24/src/liballoc/sync.rs#L924>
    fn clone(&self) -> ArcGreenNode {
        let node = &**self;

        // Using a relaxed ordering is alright here, as knowledge of the
        // original reference prevents other threads from erroneously deleting
        // the object.
        //
        // As explained in the [Boost documentation][1], Increasing the
        // reference counter can always be done with memory_order_relaxed: New
        // references to an object can only be formed from an existing
        // reference, and passing an existing reference from one thread to
        // another must already provide any required synchronization.
        //
        // [1]: (www.boost.org/doc/libs/1_55_0/doc/html/atomic/usage_examples.html)
        let old_size = node.strong.fetch_add(1, Ordering::Relaxed);

        // However we need to guard against massive refcounts in case someone
        // is `mem::forget`ing Arcs. If we don't do this the count can overflow
        // and users will use-after free. We racily saturate to `isize::MAX` on
        // the assumption that there aren't ~2 billion threads incrementing
        // the reference count at once. This branch will never be taken in
        // any realistic program.
        //
        // We abort because such a program is incredibly degenerate, and we
        // don't care to support it.
        if old_size > isize::MAX as usize {
            std::process::abort();
        }

        unsafe { Self::from_raw(node.into()) }
    }
}

impl ToOwned for GreenNode {
    type Owned = ArcGreenNode;

    fn to_owned(&self) -> ArcGreenNode {
        unsafe {
            // This is safe because all `GreenNode`s live behind an `ArcGreenNode`.
            let arc = ArcGreenNode::from_raw(self.into());
            // NB: The real owning `ArcGreenNode` cannot be dropped here because it is borrowed.
            // Don't forget to increase the reference count!
            mem::forget(arc.clone());
            arc
        }
    }
}
