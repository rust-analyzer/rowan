use std::{
    alloc::{alloc, dealloc, handle_alloc_error, Layout},
    borrow::{Borrow, ToOwned},
    convert::TryInto,
    fmt,
    hash::{Hash, Hasher},
    isize,
    marker::PhantomData,
    mem,
    ops::Deref,
    ptr,
    sync::atomic::{self, AtomicUsize, Ordering},
};

use super::*;
use crate::{cursor::SyntaxKind, NodeOrToken, TextUnit};

// GreenNode uses a custom layout with align(u64).
//
// head: GreenNodeHead {
//   strong_count: AtomicUsize (u32 or u64)
//   text_len: TextUnit (u32)
//   kind: SyntaxKind (u16)
//   fam_len: u16
// }
// (padding to u64)
// -1u32
// [0u32 ArcGreenNode 0u32] OR [1u32 GreenToken 1u32]
// -1u32

const TAG_NODE: u32 = 0;
const TAG_TOKEN: u32 = 1;
const TAG_END: u32 = -1i32 as u32;

const FAM_NODE_U64_LEN: u16 = 2;
const FAM_TOKEN_U64_LEN: u16 = 5;
const FAM_NODE_BYTE_LEN: usize = FAM_NODE_U64_LEN as usize * mem::size_of::<u64>();
const FAM_TOKEN_BYTE_LEN: usize = FAM_TOKEN_U64_LEN as usize * mem::size_of::<u64>();

const OFFSET_OF_HEAD: isize = 0;
const OFFSET_OF_TAIL: isize = 16;

struct GreenNodeHead {
    strong_count: AtomicUsize,
    text_len: TextUnit,
    kind: SyntaxKind,
    fam_len: u16,
}

/// # Safety
///
/// This struct is not actually sized.
/// This struct is only sound behind a reference or `ArcGreenNode`.
#[repr(C)] // defined layout
pub struct GreenNode {
    head: GreenNodeHead,
    tail: FlexibleU64Array,
}

#[cfg(extern_types)]
extern "C" {
    #[repr(align(8))]
    type FlexibleU64Array;
}

#[cfg(not(extern_types))]
struct FlexibleU64Array([u64; 0]);

pub struct ArcGreenNode {
    raw: ptr::NonNull<GreenNode>,
}

unsafe impl Send for ArcGreenNode {}
unsafe impl Sync for ArcGreenNode {}

#[derive(Debug, Clone)]
struct RawGreenChildren {
    /// Pointer to the start tag of the next child to be iterated.
    next: ptr::NonNull<u32>,
    /// Pointer to the end tag of the last child to be iterated.
    last: ptr::NonNull<u32>,
}

#[derive(Debug, Clone)]
pub struct GreenChildren<'a> {
    raw: RawGreenChildren,
    marker: PhantomData<&'a GreenNode>,
}

#[derive(Debug, Clone)]
pub struct GreenChildrenMut<'a> {
    raw: RawGreenChildren,
    marker: PhantomData<&'a mut GreenNode>,
}

unsafe impl Send for GreenChildren<'_> {}
unsafe impl Sync for GreenChildren<'_> {}
unsafe impl Send for GreenChildrenMut<'_> {}
unsafe impl Sync for GreenChildrenMut<'_> {}

// Not #[cfg(test)] so const asserts apply always
mod layout_tests {
    use super::*;
    use static_assertions::*;

    #[cfg(not(any(target_pointer_width = "32", target_pointer_width = "64")))]
    compile_error!("`rowan::GreenNode` not known to work when `usize` is not `u32`/`u64`");

    assert_eq_align!(GreenNodeHead, usize);
    assert_eq_size!(GreenNodeHead, [u64; 2]);
    assert_eq_align!(GreenNode, u64);
    assert_eq_size!(*const GreenNode, usize);
    assert_eq_align!(ArcGreenNode, usize);
    assert_eq_size!(ArcGreenNode, usize);
    assert_eq_size!(GreenToken, [u64; 4]);

    #[test]
    #[cfg(not(extern_types))]
    fn test_align() {
        use memoffset::*;
        // NB: offset_of! macro does not work on ?Sized types.
        //     We will have to find a workaround to use `extern_types`.
        assert_eq!(offset_of!(GreenNode, head), OFFSET_OF_HEAD.into());
        assert_eq!(offset_of!(GreenNode, tail), OFFSET_OF_TAIL.into());
    }
}

impl fmt::Debug for GreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        struct DebugChildren<'a>(&'a GreenNode);

        impl fmt::Debug for DebugChildren<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let mut debug = f.debug_list();
                for child in self.0.children() {
                    debug.entry(&child);
                }
                debug.finish()
            }
        }

        f.debug_struct("GreenNode")
            .field("kind", &self.head.kind)
            .field("text_len", &self.head.text_len)
            .field("children", &DebugChildren(self))
            .finish()
    }
}

impl Eq for GreenNode {}
impl PartialEq for GreenNode {
    fn eq(&self, other: &Self) -> bool {
        self.kind() == other.kind()
            && self.text_len() == other.text_len()
            && self.children().eq(other.children())
    }
}

impl Hash for GreenNode {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.kind().hash(state);
        self.text_len().hash(state);
        self.children().for_each(|child| child.hash(state));
    }
}

impl Drop for GreenNode {
    fn drop(&mut self) {
        for child in self.children_mut() {
            match child {
                NodeOrToken::Token(token) => unsafe { ptr::drop_in_place(token) },
                NodeOrToken::Node(node) => unsafe { ptr::drop_in_place(node) },
            }
        }
    }
}

impl ToOwned for GreenNode {
    type Owned = ArcGreenNode;
    fn to_owned(&self) -> ArcGreenNode {
        let arc = ArcGreenNode { raw: ptr::NonNull::new(self as *const _ as *mut _).unwrap() };
        // Increase the ref count by one
        mem::forget(arc.clone());
        arc
    }
}

impl fmt::Debug for ArcGreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.deref().fmt(f)
    }
}

impl Clone for ArcGreenNode {
    fn clone(&self) -> Self {
        let raw = self.raw;
        // Relaxed is what the stdlib uses: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L925-L935>
        let old_count = unsafe { raw.as_ref() }.head.strong_count.fetch_add(1, Ordering::Relaxed);
        // But protect against runaway clone/forget: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L938-L946>
        if old_count > isize::MAX as usize {
            std::process::abort();
        }
        ArcGreenNode { raw }
    }
}

impl Eq for ArcGreenNode {}
impl PartialEq for ArcGreenNode {
    fn eq(&self, other: &Self) -> bool {
        self.deref() == other.deref()
    }
}

impl Hash for ArcGreenNode {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.deref().hash(state)
    }
}

impl Drop for ArcGreenNode {
    #[inline]
    fn drop(&mut self) {
        // Lower the reference count: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L1196-L1198>
        if unsafe { self.raw.as_ref() }.head.strong_count.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }

        // Synchronize with other threads: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L1203-L1230>
        atomic::fence(Ordering::Acquire);

        // Don't inline slow path:
        unsafe fn drop_real(this: &mut ArcGreenNode) {
            let layout = this.raw.as_ref().layout();
            ptr::drop_in_place(this.raw.as_mut());
            dealloc(this.raw.as_ptr().cast(), layout)
        }

        unsafe { drop_real(self) }
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
        self.deref()
    }
}

impl GreenNode {
    /// The length of the FAM in `u64`.
    ///
    /// Returns `None` if the FAM would not fit in a `u16` length.
    fn fam_length(node_count: u16, token_count: u16) -> Option<u16> {
        1u16.checked_add(node_count.checked_mul(FAM_NODE_U64_LEN)?)?
            .checked_add(token_count.checked_mul(FAM_TOKEN_U64_LEN)?)
    }

    /// The layout of a `GreenNode` with a FAM of `fam_len` `u64`.
    fn layout_with(fam_len: u16) -> Layout {
        let size = (OFFSET_OF_TAIL as usize) + (fam_len as usize * mem::size_of::<u64>());
        let align = mem::align_of::<GreenNode>();
        Layout::from_size_align(size, align).unwrap()
    }

    /// The layout of this `GreenNode`.
    fn layout(&self) -> Layout {
        GreenNode::layout_with(self.head.fam_len)
    }

    /// Creates new Node.
    #[allow(clippy::new_ret_no_self)]
    pub fn new(kind: SyntaxKind, children: Vec<GreenElement>) -> ArcGreenNode {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        let (node_count, token_count) =
            children.iter().fold((0, 0), |(nodes, tokens), el| match el {
                NodeOrToken::Node(_) => (nodes + 1, tokens),
                NodeOrToken::Token(_) => (nodes, tokens + 1),
            });
        let fam_len = GreenNode::fam_length(node_count, token_count).expect("too many children");
        let layout = GreenNode::layout_with(fam_len);
        let head = GreenNodeHead { strong_count: AtomicUsize::new(1), kind, text_len, fam_len };

        unsafe {
            let node = ptr::NonNull::new(alloc(layout))
                .unwrap_or_else(|| handle_alloc_error(layout))
                .cast::<GreenNode>();

            ptr::write(
                node.as_ptr().cast::<u8>().offset(OFFSET_OF_HEAD).cast::<GreenNodeHead>(),
                head,
            );

            let mut flex_array = node.as_ptr().cast::<u8>().offset(OFFSET_OF_TAIL).cast::<u32>();
            ptr::write(flex_array, TAG_END);
            flex_array = flex_array.offset(1);
            for el in children {
                match el {
                    NodeOrToken::Node(node) => {
                        ptr::write(flex_array, TAG_NODE);
                        let this_node = flex_array.offset(1).cast::<ArcGreenNode>();
                        ptr::write(this_node, node);
                        let tag_after = this_node.offset(1).cast::<u32>();
                        ptr::write(tag_after, TAG_NODE);
                        flex_array = tag_after.offset(1);
                    }
                    NodeOrToken::Token(token) => {
                        ptr::write(flex_array, TAG_TOKEN);
                        let this_token = flex_array.offset(1).cast::<GreenToken>();
                        ptr::write(this_token, token);
                        let tag_after = this_token.offset(1).cast::<u32>();
                        ptr::write(tag_after, TAG_TOKEN);
                        flex_array = tag_after.offset(1);
                    }
                }
            }
            ptr::write(flex_array, TAG_END);

            ArcGreenNode { raw: node }
        }
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

    #[inline]
    fn fam_len(&self) -> u16 {
        self.head.fam_len
    }

    /// # Safety
    ///
    /// `ptr` must point at `self.tail`.
    /// Separated to allow `children`/`children_mut`
    /// to provide pointers with different mutability provenance.
    unsafe fn children_raw(&self, ptr: *mut u64) -> RawGreenChildren {
        RawGreenChildren {
            next: ptr::NonNull::new(ptr.cast::<u32>().offset(1)).unwrap(),
            last: ptr::NonNull::new(ptr.offset(self.fam_len() as isize).cast::<u32>()).unwrap(),
        }
    }

    /// Children of this node.
    #[inline]
    pub fn children(&self) -> GreenChildren<'_> {
        let ptr = (&self.tail as *const FlexibleU64Array as *mut u64);
        unsafe { GreenChildren { raw: self.children_raw(ptr), marker: PhantomData } }
    }

    #[inline]
    fn children_mut(&mut self) -> GreenChildrenMut<'_> {
        let ptr = (&mut self.tail as *mut FlexibleU64Array as *mut u64);
        unsafe { GreenChildrenMut { raw: self.children_raw(ptr), marker: PhantomData } }
    }
}

impl RawGreenChildren {
    unsafe fn at(tag_before: ptr::NonNull<u32>) -> (GreenElementRaw, ptr::NonNull<u32>) {
        match *tag_before.as_ref() {
            TAG_NODE => {
                let node = tag_before.as_ptr().offset(1).cast::<ArcGreenNode>();
                let tag_after = node.offset(1).cast::<u32>();
                debug_assert_eq!(*tag_after, TAG_NODE);
                let next = ptr::NonNull::new_unchecked(tag_after.offset(1));
                (ptr::NonNull::new_unchecked(node).into(), next)
            }
            TAG_TOKEN => {
                let token = tag_before.as_ptr().offset(1).cast::<GreenToken>();
                let tag_after = token.offset(1).cast::<u32>();
                debug_assert_eq!(*tag_after, TAG_TOKEN);
                let next = ptr::NonNull::new_unchecked(tag_after.offset(1));
                (ptr::NonNull::new_unchecked(token).into(), next)
            }
            TAG_END => unreachable!("unexpected end of children FAM"),
            _ => unreachable!("unrecognized children FAM tag"),
        }
    }

    unsafe fn at_back(tag_after: ptr::NonNull<u32>) -> (GreenElementRaw, ptr::NonNull<u32>) {
        match *tag_after.as_ref() {
            TAG_NODE => {
                let next = tag_after.as_ptr().cast::<u8>().sub(FAM_NODE_BYTE_LEN).cast::<u32>();
                let tag_before = next.offset(1);
                debug_assert_eq!(*tag_before, TAG_NODE);
                let node = tag_before.offset(1).cast::<ArcGreenNode>();
                (ptr::NonNull::new_unchecked(node).into(), ptr::NonNull::new_unchecked(next))
            }
            TAG_TOKEN => {
                let next = tag_after.as_ptr().cast::<u8>().sub(FAM_TOKEN_BYTE_LEN).cast::<u32>();
                let tag_before = next.offset(1);
                debug_assert_eq!(*tag_before, TAG_TOKEN);
                let node = tag_before.offset(1).cast::<GreenToken>();
                (ptr::NonNull::new_unchecked(node).into(), ptr::NonNull::new_unchecked(next))
            }
            TAG_END => unreachable!("unexpected end of children FAM"),
            _ => unreachable!("unrecognized children FAM tag"),
        }
    }

    unsafe fn next(&mut self) -> Option<GreenElementRaw> {
        if self.next < self.last {
            let (el, next) = RawGreenChildren::at(self.next);
            self.next = next;
            Some(el)
        } else {
            None
        }
    }

    unsafe fn next_back(&mut self) -> Option<GreenElementRaw> {
        if self.next < self.last {
            let (el, last) = RawGreenChildren::at_back(self.last);
            self.last = last;
            Some(el)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let ptr_diff =
            unsafe { self.last.as_ptr().offset(1) as usize - self.next.as_ptr() as usize };
        let fam_size = ptr_diff / 8;
        (fam_size / FAM_TOKEN_BYTE_LEN, Some(fam_size / FAM_NODE_BYTE_LEN))
    }
}

impl<'a> Iterator for GreenChildren<'a> {
    type Item = GreenElementRef<'a>;

    fn next(&mut self) -> Option<GreenElementRef<'a>> {
        unsafe { self.raw.next().map(|el| el.as_ref()) }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.raw.size_hint()
    }
}

impl<'a> Iterator for GreenChildrenMut<'a> {
    type Item = GreenElementMut<'a>;

    fn next(&mut self) -> Option<GreenElementMut<'a>> {
        unsafe { self.raw.next().map(|el| el.as_mut()) }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.raw.size_hint()
    }
}

impl<'a> DoubleEndedIterator for GreenChildren<'a> {
    fn next_back(&mut self) -> Option<GreenElementRef<'a>> {
        unsafe { self.raw.next_back().map(|el| el.as_ref()) }
    }
}

impl<'a> DoubleEndedIterator for GreenChildrenMut<'a> {
    fn next_back(&mut self) -> Option<GreenElementMut<'a>> {
        unsafe { self.raw.next_back().map(|el| el.as_mut()) }
    }
}
