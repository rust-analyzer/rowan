use std::{
    borrow::Borrow,
    fmt,
    mem::{self, ManuallyDrop},
    ops, ptr,
};

use countme::Count;

use crate::{
    arc::{Arc, HeaderSlice, ThinArc},
    green::SyntaxKind,
    TextRange, TextSize,
};

#[derive(PartialEq, Eq, Hash)]
struct GreenTokenHead {
    kind: SyntaxKind,
    leading_len: TextSize,
    trailing_len: TextSize,
    leading: Box<[GreenToken]>,
    trailing: Box<[GreenToken]>,
    _c: Count<GreenToken>,
}

type Repr = HeaderSlice<GreenTokenHead, [u8]>;
type ReprThin = HeaderSlice<GreenTokenHead, [u8; 0]>;
#[repr(transparent)]
pub struct GreenTokenData {
    data: ReprThin,
}

impl PartialEq for GreenTokenData {
    fn eq(&self, other: &Self) -> bool {
        self.kind() == other.kind()
            && self.text() == other.text()
            && self.leading_trivia() == other.leading_trivia()
            && self.trailing_trivia() == other.trailing_trivia()
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
        unsafe {
            let green = GreenToken::from_raw(ptr::NonNull::from(self));
            let green = ManuallyDrop::new(green);
            GreenToken::clone(&green)
        }
    }
}

impl Borrow<GreenTokenData> for GreenToken {
    #[inline]
    fn borrow(&self) -> &GreenTokenData {
        &*self
    }
}

impl fmt::Debug for GreenTokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GreenToken")
            .field("kind", &self.kind())
            .field("text", &self.text())
            .field("leading", &self.leading_trivia())
            .field("trailing", &self.trailing_trivia())
            .finish()
    }
}

impl fmt::Debug for GreenToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let data: &GreenTokenData = &*self;
        fmt::Debug::fmt(data, f)
    }
}

impl fmt::Display for GreenToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let data: &GreenTokenData = &*self;
        fmt::Display::fmt(data, f)
    }
}

impl fmt::Display for GreenTokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for token in self.leading_trivia() {
            write!(f, "{}", token.text())?;
        }
        write!(f, "{}", self.text())?;
        for token in self.trailing_trivia() {
            write!(f, "{}", token.text())?;
        }
        Ok(())
    }
}

impl GreenTokenData {
    /// Kind of this Token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.data.header.kind
    }

    /// Text of this Token, excluding its trivia.
    #[inline]
    pub fn text(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(self.data.slice()) }
    }

    /// Returns the length of the text covered by this token, excluding its trivia.
    #[inline]
    pub fn text_len(&self) -> TextSize {
        TextSize::of(self.text())
    }

    #[inline]
    pub(crate) fn text_range(&self) -> TextRange {
        TextRange::at(self.leading_trivia_len(), self.text_len())
    }

    #[inline]
    pub fn leading_trivia(&self) -> &[GreenToken] {
        &self.data.header.leading
    }

    #[inline]
    pub fn trailing_trivia(&self) -> &[GreenToken] {
        &self.data.header.trailing
    }

    #[inline]
    pub(crate) fn leading_trivia_len(&self) -> TextSize {
        self.data.header.leading_len
    }

    #[inline]
    pub(crate) fn trailing_trivia_len(&self) -> TextSize {
        self.data.header.trailing_len
    }

    #[inline]
    pub(crate) fn text_len_including_trivia(&self) -> TextSize {
        self.leading_trivia_len() + self.text_len() + self.trailing_trivia_len()
    }
}

impl GreenToken {
    /// Creates new Token.
    #[inline]
    pub fn new(kind: SyntaxKind, text: &str) -> GreenToken {
        Self::with_trivia(kind, text, Vec::new(), Vec::new())
    }

    pub fn with_trivia(
        kind: SyntaxKind,
        text: &str,
        leading: Vec<GreenToken>,
        trailing: Vec<GreenToken>,
    ) -> Self {
        assert!(
            leading
                .iter()
                .chain(&trailing)
                .all(|it| { it.leading_trivia().is_empty() && it.trailing_trivia().is_empty() }),
            "trivia tokens cannot themselves carry trivia"
        );
        let head = GreenTokenHead {
            kind,
            leading_len: leading.iter().map(|it| it.text_len()).sum(),
            trailing_len: trailing.iter().map(|it| it.text_len()).sum(),
            leading: leading.into_boxed_slice(),
            trailing: trailing.into_boxed_slice(),
            _c: Count::new(),
        };
        let ptr = ThinArc::from_header_and_iter(head, text.bytes());
        GreenToken { ptr }
    }

    #[inline]
    pub(crate) unsafe fn from_raw(ptr: ptr::NonNull<GreenTokenData>) -> GreenToken {
        let arc = Arc::from_raw(&ptr.as_ref().data as *const ReprThin);
        let arc = mem::transmute::<Arc<ReprThin>, ThinArc<GreenTokenHead, u8>>(arc);
        GreenToken { ptr: arc }
    }
}

impl ops::Deref for GreenToken {
    type Target = GreenTokenData;

    #[inline]
    fn deref(&self) -> &GreenTokenData {
        unsafe {
            let repr: &Repr = &self.ptr;
            let repr: &ReprThin = &*(repr as *const Repr as *const ReprThin);
            mem::transmute::<&ReprThin, &GreenTokenData>(repr)
        }
    }
}
