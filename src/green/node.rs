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
//   child_count: u16
// }
// (padding to u64)
// [0u64 ArcGreenNode OR 1u64 GreenToken]

const NODE_TAG: u64 = 0;
const TOKEN_TAG: u64 = 1;

const OFFSET_OF_HEAD: isize = 0;
const OFFSET_OF_TAIL: isize = 16;

struct GreenNodeHead {
    strong_count: AtomicUsize,
    text_len: TextUnit,
    kind: SyntaxKind,
    child_count: u16,
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
pub struct GreenChildren<'a> {
    next: ptr::NonNull<u64>,
    remaining: usize,
    marker: PhantomData<&'a GreenNode>,
}

#[derive(Debug, Clone)]
pub struct GreenChildrenMut<'a> {
    next: ptr::NonNull<u64>,
    remaining: usize,
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
    assert_eq_size!(GreenNodeHead, u128);
    assert_eq_align!(GreenNode, u64);
    assert_eq_size!(*const GreenNode, usize);
    assert_eq_align!(ArcGreenNode, usize);
    assert_eq_size!(ArcGreenNode, usize);

    #[test]
    #[cfg(not(extern_types))]
    fn test_align() {
        use memoffset::*;
        // NB: offset_of! macro does not work on ?Sized types.
        //     We will have to find a workaround to use `extern_types`.
        assert!(offset_of!(GreenNode, head).try_into().unwrap(), OFFSET_OF_HEAD);
        assert!(offset_of!(GreenNode, tail).try_into().unwrap(), OFFSET_OF_TAIL);
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
            && self.child_count() == other.child_count()
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
        let old_size = unsafe { raw.as_ref() }.head.strong_count.fetch_add(1, Ordering::Relaxed);
        // But protect against runaway clone/forget: <https://github.com/rust-lang/rust/blob/55e00631e5bc5b16d40232914e57deeea197a8e4/src/liballoc/sync.rs#L938-L946>
        if old_size > isize::MAX as usize {
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
    fn layout_with(node_count: usize, token_count: usize) -> Layout {
        let size = (OFFSET_OF_TAIL as usize)
            + (node_count * (mem::size_of::<u64>() + mem::size_of::<u64>()))
            + (token_count * (mem::size_of::<u64>() + mem::size_of::<GreenToken>()));
        let align = mem::align_of::<GreenNode>();
        Layout::from_size_align(size, align).unwrap()
    }

    fn layout(&self) -> Layout {
        let (node_count, token_count) =
            self.children().fold((0, 0), |(nodes, tokens), el| match el {
                NodeOrToken::Node(_) => (nodes + 1, tokens),
                NodeOrToken::Token(_) => (nodes, tokens + 1),
            });
        GreenNode::layout_with(node_count, token_count)
    }

    /// Creates new Node.
    #[allow(clippy::new_ret_no_self, clippy::boxed_local)]
    pub fn new(kind: SyntaxKind, children: Vec<GreenElement>) -> ArcGreenNode {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        let head = GreenNodeHead {
            strong_count: AtomicUsize::new(1),
            kind,
            text_len,
            child_count: children.len().try_into().unwrap(),
        };

        let (node_count, token_count) =
            children.iter().fold((0, 0), |(nodes, tokens), el| match el {
                NodeOrToken::Node(_) => (nodes + 1, tokens),
                NodeOrToken::Token(_) => (nodes, tokens + 1),
            });
        let layout = GreenNode::layout_with(node_count, token_count);

        unsafe {
            let node = ptr::NonNull::new(alloc(layout))
                .unwrap_or_else(|| handle_alloc_error(layout))
                .cast::<GreenNode>();

            ptr::write(
                node.as_ptr().cast::<u8>().offset(OFFSET_OF_HEAD).cast::<GreenNodeHead>(),
                head,
            );

            let mut flex_array = node.as_ptr().cast::<u8>().offset(OFFSET_OF_TAIL).cast::<u64>();
            for el in children {
                match el {
                    NodeOrToken::Node(node) => {
                        ptr::write(flex_array, NODE_TAG);
                        let this_node = flex_array.offset(1).cast::<ArcGreenNode>();
                        ptr::write(this_node, node);
                        flex_array = this_node.offset(1).cast::<u64>();
                    }
                    NodeOrToken::Token(token) => {
                        ptr::write(flex_array, TOKEN_TAG);
                        let this_token = flex_array.offset(1).cast::<GreenToken>();
                        ptr::write(this_token, token);
                        flex_array = this_token.offset(1).cast::<u64>();
                    }
                }
            }

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

    /// How many children does this node have?
    #[inline]
    pub fn child_count(&self) -> usize {
        self.head.child_count as usize
    }

    pub(crate) unsafe fn child_at(&self, idx: usize) -> GreenElementRef<'_> {
        let ptr = (&self.tail as *const _ as *mut u64).add(idx);
        let mut children = GreenChildren {
            next: ptr::NonNull::new(ptr).unwrap(),
            remaining: 1,
            marker: PhantomData,
        };
        match children.next() {
            Some(el) => el,
            None => unreachable!(),
        }
    }

    /// Children of this node.
    #[inline]
    pub fn children(&self) -> GreenChildren<'_> {
        GreenChildren {
            next: ptr::NonNull::new(&self.tail as *const _ as *mut _).unwrap(),
            remaining: self.child_count(),
            marker: PhantomData,
        }
    }

    #[inline]
    fn children_mut(&mut self) -> GreenChildrenMut<'_> {
        GreenChildrenMut {
            next: ptr::NonNull::new(&mut self.tail as *mut _ as *mut _).unwrap(),
            remaining: self.child_count(),
            marker: PhantomData,
        }
    }
}

impl<'a> Iterator for GreenChildren<'a> {
    type Item = GreenElementRef<'a>;

    fn next(&mut self) -> Option<GreenElementRef<'a>> {
        if self.remaining > 0 {
            self.remaining -= 1;
            unsafe {
                match *self.next.as_ref() {
                    NODE_TAG => {
                        let node = self.next.as_ptr().offset(1).cast::<ArcGreenNode>();
                        self.next = ptr::NonNull::new_unchecked(node.offset(1)).cast();
                        Some((&**node).into())
                    }
                    TOKEN_TAG => {
                        let token = self.next.as_ptr().offset(1).cast::<GreenToken>();
                        self.next = ptr::NonNull::new_unchecked(token.offset(1)).cast();
                        Some((&*token).into())
                    }
                    _ => unreachable!(),
                }
            }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'a> Iterator for GreenChildrenMut<'a> {
    type Item = GreenElementMut<'a>;

    fn next(&mut self) -> Option<GreenElementMut<'a>> {
        if self.remaining > 0 {
            self.remaining -= 1;
            unsafe {
                match *self.next.as_ref() {
                    NODE_TAG => {
                        let node = self.next.as_ptr().offset(1).cast::<ArcGreenNode>();
                        self.next = ptr::NonNull::new_unchecked(node.offset(1)).cast();
                        Some((&mut *node).into())
                    }
                    TOKEN_TAG => {
                        let token = self.next.as_ptr().offset(1).cast::<GreenToken>();
                        self.next = ptr::NonNull::new_unchecked(token.offset(1)).cast();
                        Some((&mut *token).into())
                    }
                    _ => unreachable!(),
                }
            }
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for GreenChildren<'_> {
    fn len(&self) -> usize {
        self.remaining
    }
}

impl ExactSizeIterator for GreenChildrenMut<'_> {
    fn len(&self) -> usize {
        self.remaining
    }
}
