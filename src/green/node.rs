use {
    crate::{
        green::{GreenElement, GreenToken},
        helpers::repr_c_4,
        Kind, NodeOrToken, TextUnit,
    },
    erasable::{erase, Erasable, ErasedPtr},
    rc_borrow::ArcBorrow,
    rc_box::ArcBox,
    std::{
        alloc::{alloc, handle_alloc_error, Layout, LayoutErr},
        convert::TryInto,
        iter::TrustedLen,
        ptr,
        sync::Arc,
    },
};

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
#[repr(C)]
#[repr(align(2))] // NB: align >= 2
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct GreenNode {
    // NB: This is optimal layout, as the order is (u16, u16, u32, [ErasedPtr]).
    children_len: u16,
    /// The kind of this node.
    pub kind: Kind,
    /// The length of the text covered by this node.
    pub text_len: TextUnit,
    children: [GreenElement],
}

impl GreenNode {
    /// Heap layout for a `Box<GreenNode>` with the given text length.
    fn layout(children_len: u16) -> Result<(Layout, [usize; 4]), LayoutErr> {
        let children_len_layout = Layout::new::<u16>();
        let kind_layout = Layout::new::<Kind>();
        let text_len_layout = Layout::new::<TextUnit>();
        let children_layout = Layout::array::<ErasedPtr>(children_len.into())?;
        repr_c_4([children_len_layout, kind_layout, text_len_layout, children_layout])
    }

    /// Allocate a `Box<GreenNode>` with the given text length and layout.
    ///
    /// For the returned pointer to be usable, the layout and text length must agree.
    unsafe fn alloc_box(children_len: u16, layout: Layout) -> ptr::NonNull<Self> {
        let ptr = ptr::NonNull::new(alloc(layout)).unwrap_or_else(|| handle_alloc_error(layout));
        ptr::write(ptr.as_ptr().cast(), children_len);
        GreenNode::unerase(erase(ptr))
    }

    /// Create a new `GreenNode` with the given kind and text.
    ///
    /// The public entry point is `GreenBuilder::node`.
    pub(crate) fn new<I, E>(kind: Kind, children: I) -> ArcBox<Self>
    where
        E: Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>,
        I: IntoIterator<Item = E>,
        I::IntoIter: ExactSizeIterator + TrustedLen,
    {
        let mut text_len: TextUnit = 0.into();
        let children =
            children.into_iter().map(Into::into).inspect(|child| text_len += child.text_len());
        let children_len: u16 = children.len().try_into().unwrap();
        let (layout, [children_len_offset, kind_offset, text_len_offset, children_offset]) =
            Self::layout(children.len().try_into().unwrap()).unwrap();
        let boxed = unsafe {
            let ptr = Self::alloc_box(children_len, layout);
            ptr::write(ptr.cast::<u8>().as_ptr().add(children_len_offset).cast(), children_len);
            ptr::write(ptr.cast::<u8>().as_ptr().add(kind_offset).cast(), kind);
            // Don't write `text_len` yet, as we haven't inspected the children iterator yet!
            let mut child_ptr: *mut GreenElement =
                ptr.cast::<u8>().as_ptr().add(children_offset).cast();
            for child in children {
                ptr::write(child_ptr, GreenElement::from(child));
                child_ptr = child_ptr.offset(1);
            }
            // Now `text_len` is the sum of the children's text lengths.
            ptr::write(ptr.cast::<u8>().as_ptr().add(text_len_offset).cast(), text_len);
            Box::from_raw(ptr.as_ptr())
        };
        boxed.into()
    }

    /// Children of this node.
    pub fn children(
        &self,
    ) -> impl Iterator<Item = NodeOrToken<ArcBorrow<'_, GreenNode>, ArcBorrow<'_, GreenToken>>>
    {
        self.children.iter().map(Into::into)
    }
}

unsafe impl Erasable for GreenNode {
    unsafe fn unerase(this: ErasedPtr) -> ptr::NonNull<Self> {
        let children_len: u16 = *this.cast().as_ref();
        let ptr = ptr::slice_from_raw_parts_mut::<()>(this.as_ptr().cast(), children_len.into());
        #[allow(clippy::cast_ptr_alignment)]
        ptr::NonNull::new_unchecked(ptr as *mut Self)
    }
}

#[test]
fn miri_smoke() {
    use {crate::GreenToken, erasable::Thin, rc_borrow::ArcBorrow};
    let tok1 = GreenElement::from(GreenToken::new(Kind(0), "test"));
    let tok2 = tok1.clone();
    let tok3 = tok1.clone();
    let dangling = {
        let node: ArcBox<GreenNode> = GreenNode::new(Kind(0), vec![tok1, tok2, tok3]);
        let node: Arc<GreenNode> = node.into();
        let node: ArcBorrow<GreenNode> = (&node).into();
        let node: Thin<ArcBorrow<GreenNode>> = node.into();
        let node: Arc<GreenNode> = ArcBorrow::upgrade(Thin::into_inner(node));
        Arc::downgrade(&node)
    };
    assert_eq!(dangling.upgrade(), None);
}
