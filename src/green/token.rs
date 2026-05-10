use std::{
    borrow::Borrow,
    fmt,
    mem::{self, ManuallyDrop},
    ops, ptr,
};

use countme::Count;

use crate::{
    TextSize,
    arc::{Arc, HeaderSlice, ThinArc, thin_to_thick},
    green::SyntaxKind,
};

#[derive(PartialEq, Eq, Hash)]
struct GreenTokenHead {
    kind: SyntaxKind,
    _c: Count<GreenToken>,
}

type Repr = HeaderSlice<GreenTokenHead, [u8]>;
type ReprThin = HeaderSlice<GreenTokenHead, [u8; 0]>;
#[repr(transparent)]
pub struct GreenTokenData {
    data: Repr, // unsized — provenance covers the full slice
}

impl PartialEq for GreenTokenData {
    fn eq(&self, other: &Self) -> bool {
        self.kind() == other.kind() && self.text() == other.text()
    }
}

/// Leaf node in the immutable tree.
#[derive(PartialEq, Eq, Hash, Clone)]
#[repr(transparent)]
pub struct GreenToken {
    ptr: ThinArc<GreenTokenHead, u8>,
}

impl ToOwned for GreenTokenData {
    type Owned = GreenToken;

    #[inline]
    fn to_owned(&self) -> GreenToken {
        let green = unsafe { GreenToken::from_raw(ptr::NonNull::from(self)) };
        let green = ManuallyDrop::new(green);
        GreenToken::clone(&green)
    }
}

impl Borrow<GreenTokenData> for GreenToken {
    #[inline]
    fn borrow(&self) -> &GreenTokenData {
        self
    }
}

impl fmt::Debug for GreenTokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GreenToken")
            .field("kind", &self.kind())
            .field("text", &self.text())
            .finish()
    }
}

impl fmt::Debug for GreenToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let data: &GreenTokenData = self;
        fmt::Debug::fmt(data, f)
    }
}

impl fmt::Display for GreenToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let data: &GreenTokenData = self;
        fmt::Display::fmt(data, f)
    }
}

impl fmt::Display for GreenTokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text())
    }
}

impl GreenTokenData {
    /// Kind of this Token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.data.header.kind
    }

    /// Text of this Token.
    #[inline]
    pub fn text(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.data.slice()) }
    }

    /// Returns the length of the text covered by this token.
    #[inline]
    pub fn text_len(&self) -> TextSize {
        TextSize::of(self.text())
    }
}

impl GreenToken {
    /// Creates new Token.
    #[inline]
    pub fn new(kind: SyntaxKind, text: &str) -> GreenToken {
        let head = GreenTokenHead { kind, _c: Count::new() };
        let ptr = ThinArc::from_header_and_iter(head, text.bytes());
        GreenToken { ptr }
    }
    #[inline]
    pub(crate) fn into_raw(this: GreenToken) -> ptr::NonNull<GreenTokenData> {
        let green = ManuallyDrop::new(this);
        // Extract a fat pointer directly from the ThinArc to preserve full
        // allocation provenance (the slice tail extends past the thin
        // header, and a narrow `&GreenTokenData` would not cover it).
        let thin_ptr = green.ptr.ptr.as_ptr();
        let thick = thin_to_thick(thin_ptr);
        unsafe {
            ptr::NonNull::new_unchecked(
                ptr::addr_of!((*thick).data) as *mut Repr as *mut GreenTokenData
            )
        }
    }

    /// # Safety
    ///
    /// This function uses `unsafe` code to create an `Arc` from a raw pointer and then transmutes it into a `ThinArc`.
    ///
    /// - The raw pointer must be valid and correctly aligned for the type `ReprThin`.
    /// - The lifetime of the raw pointer must outlive the lifetime of the `Arc` created from it.
    /// - The transmute operation must be safe, meaning that the memory layout of `Arc<ReprThin>` must be compatible with `ThinArc<GreenTokenHead, u8>`.
    ///
    /// Failure to uphold these invariants can lead to undefined behavior.
    #[inline]
    pub(crate) unsafe fn from_raw(ptr: ptr::NonNull<GreenTokenData>) -> GreenToken {
        unsafe {
            // Reinterpret the (fat) pointer to GreenTokenData as a thin
            // pointer to ReprThin: drop the slice metadata, since
            // ThinArc carries length in the allocation header.
            let thin_ptr = ptr.as_ptr() as *const Repr as *const ReprThin;
            let arc = Arc::from_raw(thin_ptr);
            let arc = mem::transmute::<Arc<ReprThin>, ThinArc<GreenTokenHead, u8>>(arc);
            GreenToken { ptr: arc }
        }
    }
}

impl ops::Deref for GreenToken {
    type Target = GreenTokenData;

    #[inline]
    fn deref(&self) -> &GreenTokenData {
        // SAFETY: GreenTokenData is #[repr(transparent)] over Repr (fat HeaderSlice).
        // ThinArc::deref() returns &HeaderSlice<H, [T]> with full allocation
        // provenance via thin_to_thick(). We transmute to &GreenTokenData,
        // which has the same layout.
        let repr: &Repr = &self.ptr;
        unsafe { mem::transmute::<&Repr, &GreenTokenData>(repr) }
    }
}
