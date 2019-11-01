use std::sync::Arc;

use super::*;
use crate::{cursor::SyntaxKind, NodeOrToken, TextUnit};

pub type GreenElement = NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>;

impl From<Arc<GreenNode>> for GreenElement {
    #[inline]
    fn from(node: Arc<GreenNode>) -> GreenElement {
        NodeOrToken::Node(node)
    }
}

impl From<Arc<GreenToken>> for GreenElement {
    #[inline]
    fn from(token: Arc<GreenToken>) -> GreenElement {
        NodeOrToken::Token(token)
    }
}

impl GreenElement {
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
}
