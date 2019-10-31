use std::ptr;

use super::*;
use crate::{cursor::SyntaxKind, NodeOrToken, TextUnit};

// TODO: reduce repetition with a macro?

pub(crate) type GreenElement = NodeOrToken<ArcGreenNode, GreenToken>;
pub(crate) type GreenElementRef<'a> = NodeOrToken<&'a GreenNode, &'a GreenToken>;
pub(crate) type GreenElementMut<'a> = NodeOrToken<&'a mut ArcGreenNode, &'a mut GreenToken>;
pub(crate) type GreenElementRaw = NodeOrToken<ptr::NonNull<ArcGreenNode>, ptr::NonNull<GreenToken>>;

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

impl From<ptr::NonNull<ArcGreenNode>> for GreenElementRaw {
    #[inline]
    fn from(node: ptr::NonNull<ArcGreenNode>) -> GreenElementRaw {
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

impl From<ptr::NonNull<GreenToken>> for GreenElementRaw {
    #[inline]
    fn from(token: ptr::NonNull<GreenToken>) -> GreenElementRaw {
        NodeOrToken::Token(token)
    }
}

impl GreenElementRef<'_> {
    #[inline]
    pub fn to_owned(self) -> GreenElement {
        match self {
            NodeOrToken::Node(node) => NodeOrToken::Node(node.to_owned()),
            NodeOrToken::Token(token) => NodeOrToken::Token(token.clone()),
        }
    }
}

#[allow(unsafe_code)]
impl GreenElementRaw {
    #[inline]
    pub(crate) unsafe fn as_ref<'a>(self) -> GreenElementRef<'a> {
        match self {
            NodeOrToken::Node(raw) => (&**raw.as_ptr()).into(),
            NodeOrToken::Token(raw) => (&*raw.as_ptr()).into(),
        }
    }

    #[inline]
    pub(crate) unsafe fn as_mut<'a>(self) -> GreenElementMut<'a> {
        match self {
            NodeOrToken::Node(raw) => (&mut *raw.as_ptr()).into(),
            NodeOrToken::Token(raw) => (&mut *raw.as_ptr()).into(),
        }
    }
}

impl Into<ptr::NonNull<u64>> for GreenElementRef<'_> {
    fn into(self) -> ptr::NonNull<u64> {
        match self {
            NodeOrToken::Node(node) => ptr::NonNull::from(node).cast(),
            NodeOrToken::Token(token) => ptr::NonNull::from(token).cast(),
        }
    }
}

impl GreenElementRef<'_> {
    pub(crate) fn fam_len(&self) -> u16 {
        match self {
            NodeOrToken::Node(_) => node::FAM_NODE_U64_LEN,
            NodeOrToken::Token(_) => node::FAM_TOKEN_U64_LEN,
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
