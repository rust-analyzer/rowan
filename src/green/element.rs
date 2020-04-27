use {
    crate::{
        green::{GreenNode, GreenToken, SyntaxKind},
        NodeOrToken, TextSize,
    },
    std::sync::Arc,
    sorbus::ArcBorrow,
};

pub(crate) type GreenElement = NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>;
pub(crate) type GreenElementRef<'a> = NodeOrToken<ArcBorrow<'a, GreenNode>, ArcBorrow<'a, GreenToken>>;

impl From<Arc<GreenNode>> for GreenElement {
    #[inline]
    fn from(node: Arc<GreenNode>) -> GreenElement {
        NodeOrToken::Node(node)
    }
}

impl<'a> From<&'a Arc<GreenNode>> for GreenElementRef<'a> {
    #[inline]
    fn from(node: &'a Arc<GreenNode>) -> GreenElementRef<'a> {
        NodeOrToken::Node(node.into())
    }
}

impl<'a> From<ArcBorrow<'a, GreenNode>> for GreenElementRef<'a> {
    #[inline]
    fn from(node: ArcBorrow<'a, GreenNode>) -> GreenElementRef<'a> {
        NodeOrToken::Node(node)
    }
}

impl From<Arc<GreenToken>> for GreenElement {
    #[inline]
    fn from(token: Arc<GreenToken>) -> GreenElement {
        NodeOrToken::Token(token)
    }
}

impl<'a> From<&'a Arc<GreenToken>> for GreenElementRef<'a> {
    #[inline]
    fn from(token: &'a Arc<GreenToken>) -> GreenElementRef<'a> {
        NodeOrToken::Token(token.into())
    }
}

impl<'a> From<ArcBorrow<'a, GreenToken>> for GreenElementRef<'a> {
    #[inline]
    fn from(token: ArcBorrow<'a, GreenToken>) -> GreenElementRef<'a> {
        NodeOrToken::Token(token)
    }
}

impl GreenElement {
    /// Returns kind of this element.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.borrowed().kind()
    }

    /// Returns the length of the text covered by this element.
    #[inline]
    pub fn text_len(&self) -> TextSize {
        self.borrowed().text_len()
    }
}

impl GreenElementRef<'_> {
    /// Returns kind of this element.
    #[inline]
    pub fn kind(self) -> SyntaxKind {
        match self {
            NodeOrToken::Node(it) => it.kind(),
            NodeOrToken::Token(it) => it.kind(),
        }
    }

    /// Returns the length of the text covered by this element.
    #[inline]
    pub fn text_len(self) -> TextSize {
        match self {
            NodeOrToken::Node(it) => it.text_len(),
            NodeOrToken::Token(it) => it.text_len(),
        }
    }
}
