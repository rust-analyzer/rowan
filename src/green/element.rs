use super::*;
use crate::{cursor::SyntaxKind, NodeOrToken, TextUnit};

pub(crate) type GreenElement = NodeOrToken<ArcGreenNode, GreenToken>;
pub(crate) type GreenElementRef<'a> = NodeOrToken<&'a GreenNode, &'a GreenToken>;
pub(crate) type GreenElementMut<'a> = NodeOrToken<&'a mut ArcGreenNode, &'a mut GreenToken>;

impl From<ArcGreenNode> for GreenElement {
    #[inline]
    fn from(node: ArcGreenNode) -> GreenElement {
        NodeOrToken::Node(node)
    }
}

impl<'a> From<&'a GreenNode> for GreenElementRef<'a> {
    #[inline]
    fn from(node: &'a GreenNode) -> GreenElementRef<'a> {
        NodeOrToken::Node(node)
    }
}

impl<'a> From<&'a mut ArcGreenNode> for GreenElementMut<'a> {
    #[inline]
    fn from(node: &'a mut ArcGreenNode) -> GreenElementMut<'a> {
        NodeOrToken::Node(node)
    }
}

impl From<GreenToken> for GreenElement {
    #[inline]
    fn from(token: GreenToken) -> GreenElement {
        NodeOrToken::Token(token)
    }
}

impl<'a> From<&'a GreenToken> for GreenElementRef<'a> {
    #[inline]
    fn from(token: &'a GreenToken) -> GreenElementRef<'a> {
        NodeOrToken::Token(token)
    }
}

impl<'a> From<&'a mut GreenToken> for GreenElementMut<'a> {
    #[inline]
    fn from(token: &'a mut GreenToken) -> GreenElementMut<'a> {
        NodeOrToken::Token(token)
    }
}

impl GreenElementRef<'_> {
    #[inline]
    pub(crate) fn to_owned(self) -> GreenElement {
        match self {
            NodeOrToken::Node(node) => NodeOrToken::Node(node.to_owned()),
            NodeOrToken::Token(token) => NodeOrToken::Token(token.clone()),
        }
    }
}

macro_rules! green_element_methods {
    () => {
        /// Returns kind of this element.
        #[inline]
        pub fn kind(&self) -> SyntaxKind {
            match self {
                NodeOrToken::Node(it) => it.kind(),
                NodeOrToken::Token(it) => it.kind(),
            }
        }

        /// Returns length of the text covered by this element.
        #[inline]
        pub fn text_len(&self) -> TextUnit {
            match self {
                NodeOrToken::Node(it) => it.text_len(),
                NodeOrToken::Token(it) => it.text_len(),
            }
        }
    };
}

impl GreenElement {
    green_element_methods!();
}

impl GreenElementRef<'_> {
    green_element_methods!();
}

impl GreenElementMut<'_> {
    green_element_methods!();
}
