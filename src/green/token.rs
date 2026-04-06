use std::{
    borrow::Borrow,
    fmt,
    mem::{self, ManuallyDrop},
    ops, ptr,
};

use countme::Count;

use crate::{
    TextSize,
    arc::{Arc, HeaderSlice, ThinArc},
    green::SyntaxKind,
};

#[derive(PartialEq, Eq, Hash)]
struct GreenTokenHead {
    kind: SyntaxKind,
    _c: Count<GreenToken>,
}

type ReprThin = HeaderSlice<GreenTokenHead, [u8; 0]>;
#[repr(transparent)]
pub struct GreenTokenData {
    data: ReprThin,
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
        // Access the byte slice via raw pointer arithmetic to avoid going through
        // Deref on HeaderSlice<H, [u8; 0]> which creates a reference with provenance
        // limited to the thin type.
        unsafe {
            let len = self.data.length;
            let slice_start = ptr::addr_of!(self.data.slice) as *const u8;
            let bytes = std::slice::from_raw_parts(slice_start, len);
            std::str::from_utf8_unchecked(bytes)
        }
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
        // Extract pointer directly from ThinArc to preserve full allocation provenance.
        let inner = green.ptr.ptr.as_ptr();
        unsafe {
            let data = ptr::addr_of!((*inner).data);
            ptr::NonNull::new_unchecked(data as *mut ReprThin as *mut GreenTokenData)
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
        let arc = unsafe {
            let arc = Arc::from_raw(&ptr.as_ref().data as *const ReprThin);
            mem::transmute::<Arc<ReprThin>, ThinArc<GreenTokenHead, u8>>(arc)
        };
        GreenToken { ptr: arc }
    }
}

impl ops::Deref for GreenToken {
    type Target = GreenTokenData;

    #[inline]
    fn deref(&self) -> &GreenTokenData {
        unsafe {
            let inner = self.ptr.ptr.as_ptr();
            &*(ptr::addr_of!((*inner).data) as *const ReprThin as *const GreenTokenData)
        }
    }
}
