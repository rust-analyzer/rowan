use std::fmt;

use crate::{arc::Arc, green::SyntaxKind, SmolStr, TextSize};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct GreenTokenData {
    kind: SyntaxKind,
    text: SmolStr,
}

/// Leaf node in the immutable tree.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct GreenToken {
    data: Arc<GreenTokenData>,
}

impl GreenToken {
    fn data(&self) -> &GreenTokenData {
        &*self.data
    }

    /// Creates new Token.
    #[inline]
    pub fn new(kind: SyntaxKind, text: SmolStr) -> GreenToken {
        let data = Arc::new(GreenTokenData { kind, text });
        GreenToken { data }
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

    /// Returns the length of the text covered by this token.
    #[inline]
    pub fn text_len(&self) -> TextSize {
        TextSize::of(self.text().as_str())
    }
}

impl fmt::Debug for GreenToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let data = self.data();
        f.debug_struct("GreenToken").field("kind", &data.kind).field("text", &data.text).finish()
    }
}
