use static_assertions::*;
use std::{
    alloc::{alloc, dealloc, Layout},
    mem, ptr, slice,
    sync::Arc,
};

use super::*;
use crate::{cursor::SyntaxKind, TextUnit};
use std::alloc::handle_alloc_error;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct GreenNodeHead {
    kind: SyntaxKind,
    text_len: TextUnit,
}

assert_eq_size!(GreenNodeHead, u64);
assert_eq_align!(GreenNodeHead, u32);

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
#[repr(C)]
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct GreenNode {
    head: GreenNodeHead,
    children: [GreenElement],
}

#[test]
fn node_layout() {
    let node = unsafe { GreenNode::dummy() };
    assert_eq!(GreenNode::align(), mem::align_of_val(node));
    assert_eq!(GreenNode::size_for(0), mem::size_of_val(node));
    assert_eq!(GreenNode::layout_for(0), Layout::for_value(node));
    assert_eq!(
        GreenNode::offset_of_head(),
        &node.head as *const _ as *const () as isize - node as *const _ as *const () as isize
    );
    assert_eq!(
        GreenNode::offset_of_children(),
        &node.children as *const _ as *const () as isize - node as *const _ as *const () as isize
    );
}

impl GreenNode {
    #[inline]
    fn alloc<I>(kind: SyntaxKind, text_len: TextUnit, children: I) -> Arc<GreenNode>
    where
        I: Iterator<Item = GreenElement> + ExactSizeIterator,
    {
        let len = children.len();
        let head = GreenNodeHead { kind, text_len };
        let layout = GreenNode::layout_for(len);

        let boxed = unsafe {
            let ptr = ptr::NonNull::new(alloc(layout))
                .unwrap_or_else(|| handle_alloc_error(layout))
                .as_ptr();

            ptr::write(ptr.offset(GreenNode::offset_of_head()).cast(), head);

            let mut fam = ptr.offset(GreenNode::offset_of_children()).cast::<GreenElement>();
            for child in children {
                ptr::write(fam, child);
                fam = fam.offset(1);
            }

            let ptr: *mut [u8] = slice::from_raw_parts_mut(ptr, len);
            Box::from_raw(ptr as *mut GreenNode)
        };

        // NB: this creates a new allocation for the Arc, moves into it, and drops the Box.
        // We cannot allocate an Arc directly, so it's this or implementing a custom Arc.
        boxed.into()
    }

    const fn align() -> usize {
        let head_align = mem::align_of::<GreenNodeHead>();
        let tail_align = mem::align_of::<GreenElement>();
        [head_align, tail_align][(head_align < tail_align) as usize] // true == 1usize
    }

    const fn size_for(child_count: usize) -> usize {
        let head_size = mem::size_of::<GreenNodeHead>();
        let tail_align = mem::align_of::<GreenElement>();
        let tail_size = mem::size_of::<GreenElement>() * child_count;
        head_size + head_size % tail_align + tail_size + tail_size % GreenNode::align()
        // head ^   ^^^^^^^ padding ^^^^^^   ^^ tail ^   ^^^^^^^^^^^ padding ^^^^^^^^^^
    }

    const fn layout_for(child_count: usize) -> Layout {
        unsafe {
            Layout::from_size_align_unchecked(GreenNode::size_for(child_count), GreenNode::align())
        }
    }

    const fn offset_of_head() -> isize {
        0
    }

    const fn offset_of_children() -> isize {
        let head_size = mem::size_of::<GreenNodeHead>() as isize;
        let tail_align = mem::align_of::<GreenElement>() as isize;
        GreenNode::offset_of_head() + head_size + head_size % tail_align
    }
}

impl GreenNode {
    /// A dummy `GreenNode` for use as a placeholder.
    ///
    /// # Safety
    ///
    /// You _can not_ upgrade this reference to `Arc`.
    /// It is a fully valid `GreenNode` otherwise.
    pub(crate) unsafe fn dummy() -> &'static GreenNode {
        static HEAD: u64 = 0;
        // produce a `mem::zeroed` GreenNodeHead with no children
        &*(slice::from_raw_parts(&HEAD, 0) as *const [u64] as *const GreenNode)
    }

    /// Creates new Node.
    #[inline]
    pub fn new(kind: SyntaxKind, children: Vec<GreenElement>) -> Arc<GreenNode> {
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
        &self.children
    }
}
