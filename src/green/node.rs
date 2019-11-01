use std::sync::Arc;

use super::*;
use crate::{cursor::SyntaxKind, TextUnit};

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GreenNode {
    kind: SyntaxKind,
    text_len: TextUnit,
    //TODO: implement llvm::trailing_objects trick
    children: Arc<[GreenElement]>,
}

impl GreenNode {
    /// Creates new Node.
    #[inline]
    pub fn new(kind: SyntaxKind, children: Box<[GreenElement]>) -> Arc<GreenNode> {
        let text_len = children.iter().map(|x| x.text_len()).sum::<TextUnit>();
        Arc::new(GreenNode { kind, text_len, children: children.into() })
    }

    /// Kind of this node.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Length of the text, covered by this node.
    #[inline]
    pub fn text_len(&self) -> TextUnit {
        self.text_len
    }
    /// Children of this node.
    #[inline]
    pub fn children(&self) -> &[GreenElement] {
        &self.children
    }
}
