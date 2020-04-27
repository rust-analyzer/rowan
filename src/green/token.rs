use {
    crate::{
        green::{NodeCache, SyntaxKind},
        TextSize,
    },
    std::{
        fmt,
        sync::Arc,
    },
};

/// Leaf node in the immutable tree.
#[repr(transparent)]
#[derive(Eq, PartialEq, Hash)]
pub struct GreenToken {
    imp: sorbus::green::Token,
}

impl GreenToken {
    /// Creates a new token, without any caching.
    #[inline]
    #[deprecated(note = "use the builder API to deduplicate tokens")]
    pub fn new(kind: SyntaxKind, text: &str) -> Arc<GreenToken> {
        NodeCache::new().token(kind, text)
    }

    /// The kind of this token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        SyntaxKind(self.imp.kind().0)
    }

    /// The text of this token.
    #[inline]
    pub fn text(&self) -> &str {
        self.imp.text()
    }

    /// The length of the text covered by this token.
    #[inline]
    pub fn text_len(&self) -> TextSize {
        self.imp.len()
    }
}

impl fmt::Debug for GreenToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GreenToken")
            .field("kind", &self.kind())
            .field("text", &self.text())
            .finish()
    }
}
