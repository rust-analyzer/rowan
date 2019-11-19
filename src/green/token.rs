use std::sync::Arc;

use crate::{cursor::SyntaxKind, SmolStr, TextUnit};

#[derive(Debug, PartialEq, Eq, Hash)]
struct GreenTokenData {
    kind: SyntaxKind,
    text: SmolStr,
}

/// Leaf node in the immutable tree.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenToken {
    data: Arc<GreenTokenData>,
}

impl GreenToken {
    /// Creates new Token.
    #[inline]
    pub fn new(kind: SyntaxKind, text: SmolStr) -> GreenToken {
        GreenToken { data: Arc::new(GreenTokenData { kind, text }) }
    }

    /// Kind of this Token.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.data.kind
    }

    /// Text of this Token.
    #[inline]
    pub fn text(&self) -> &SmolStr {
        &self.data.text
    }

    /// Text of this Token.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        TextUnit::from_usize(self.text().len())
    }
}
