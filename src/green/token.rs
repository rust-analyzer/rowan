use crate::{cursor::SyntaxKind, SmolStr, TextUnit};

/// Leaf node in the immutable tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    kind: SyntaxKind,
    text: SmolStr,
}

impl GreenToken {
    /// Creates new Token.
    #[inline]
    pub fn new(kind: SyntaxKind, text: SmolStr) -> GreenToken {
        GreenToken { kind, text }
    }

    /// Kind of this Token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Text of this Token.
    #[inline]
    pub fn text(&self) -> &SmolStr {
        &self.text
    }

    /// Text of this Token.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        TextUnit::from_usize(self.text.len())
    }
}
