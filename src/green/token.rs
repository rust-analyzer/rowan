use {
    crate::{helpers::repr_c_3, Kind},
    erasable::{erase, Erasable, ErasedPtr},
    rc_box::ArcBox,
    std::{
        alloc::{alloc, handle_alloc_error, Layout, LayoutErr},
        ptr,
    },
    str_index::StrIndex,
};

/// Leaf node in the immutable tree.
///
/// Tokens are created using [`GreenBuilder::token`][super::GreenBuilder::token].
///
/// Note that while this struct is `#[repr(C)]`,
/// neither its layout nor field offsets are publicly stable.
#[repr(C)]
#[repr(align(2))] // NB: align >= 2
#[derive(Debug, Eq, PartialEq, Hash)]
pub struct GreenToken {
    // NB: This is optimal layout, as the order is (u32, u16, [u8]).
    /// The length of the trailing text.
    ///
    /// # Safety
    ///
    /// This field must be first (to be accessed by erased pointers),
    /// and must accurately represent the length of the trailing text.
    text_len: StrIndex,
    /// The kind of this token.
    pub kind: Kind,
    /// The text of this token.
    pub text: str,
}

impl GreenToken {
    /// Heap layout for a `Box<GreenToken>` with the given text length.
    fn layout(text_len: StrIndex) -> Result<(Layout, [usize; 3]), LayoutErr> {
        let text_len_layout = Layout::new::<StrIndex>();
        let kind_layout = Layout::new::<Kind>();
        let text_layout = Layout::array::<u8>(text_len.to_usize())?;
        repr_c_3([text_len_layout, kind_layout, text_layout])
    }

    /// Allocate a `Box<GreenToken>` with the given text length and layout.
    ///
    /// For the returned pointer to be usable, the layout and text length must agree.
    unsafe fn alloc_box(text_len: StrIndex, layout: Layout) -> ptr::NonNull<GreenToken> {
        let ptr = ptr::NonNull::new(alloc(layout)).unwrap_or_else(|| handle_alloc_error(layout));
        ptr::write(ptr.as_ptr().cast(), text_len);
        GreenToken::unerase(erase(ptr))
    }

    /// Create a new `GreenToken` with the given kind and text.
    ///
    /// The public entry point is `GreenBuilder::node`.
    pub(crate) fn new(kind: Kind, text: &str) -> ArcBox<GreenToken> {
        let text_len = StrIndex::from_str_len(text);
        let (layout, [text_len_offset, kind_offset, text_offset]) = Self::layout(text_len).unwrap();
        // FUTURE: allocate Arc directly rather than Box first?
        let boxed = unsafe {
            let ptr = Self::alloc_box(text_len, layout);
            ptr::write(ptr.cast::<u8>().as_ptr().add(text_len_offset).cast(), text_len);
            ptr::write(ptr.cast::<u8>().as_ptr().add(kind_offset).cast(), kind);
            ptr::copy_nonoverlapping(
                text.as_bytes().as_ptr(),
                ptr.cast::<u8>().as_ptr().add(text_offset),
                text_len.to_usize(),
            );
            Box::from_raw(ptr.as_ptr())
        };
        boxed.into()
    }
}

unsafe impl Erasable for GreenToken {
    unsafe fn unerase(this: ErasedPtr) -> ptr::NonNull<Self> {
        let text_len: StrIndex = *this.cast().as_ref();
        let ptr = ptr::slice_from_raw_parts_mut::<()>(this.as_ptr().cast(), text_len.to_usize());
        #[allow(clippy::cast_ptr_alignment)]
        ptr::NonNull::new_unchecked(ptr as *mut GreenToken)
    }
}

#[test]
fn miri_smoke() {
    use {erasable::Thin, rc_borrow::ArcBorrow, std::sync::Arc};
    let dangling = {
        let token: ArcBox<GreenToken> = GreenToken::new(Kind(0), "test");
        let token: Arc<GreenToken> = token.into();
        let token: ArcBorrow<GreenToken> = (&token).into();
        let token: Thin<ArcBorrow<GreenToken>> = token.into();
        let token: Arc<GreenToken> = ArcBorrow::upgrade(Thin::into_inner(token));
        Arc::downgrade(&token)
    };
    assert_eq!(dangling.upgrade(), None);
}
