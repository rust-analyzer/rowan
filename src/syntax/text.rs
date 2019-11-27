use {
    crate::{
        syntax::untyped::{Node, NodeKind},
        GreenToken,
    },
    rc_borrow::ArcBorrow,
    std::mem,
    text_unit::{TextRange, TextUnit},
};

/// Text of a node in the syntax tree.
#[derive(Debug, Clone)]
pub struct Text {
    pub(super) node: Node,
    pub(super) range: TextRange,
}

impl Text {
    pub(crate) fn new(node: Node) -> Text {
        let offset = match node.inner.kind {
            NodeKind::Child { offset, .. } => offset,
            _ => 0.into(),
        };
        let range = TextRange::offset_len(offset, node.green().text_len());
        Text { node, range }
    }

    /// This node.
    pub fn node(&self) -> &Node {
        &self.node
    }

    /// Range of this text.
    pub fn range(&self) -> TextRange {
        self.range
    }

    /// Length of this text.
    pub fn len(&self) -> TextUnit {
        self.range.len()
    }

    #[allow(missing_docs)]
    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }
}

#[allow(missing_docs)]
impl Text {
    pub fn chunks(&self) -> impl Iterator<Item = (ArcBorrow<'_, GreenToken>, TextRange)> {
        let range = self.range();
        self.node.clone().preorder().filter(|el| el.green().is_token()).filter_map(move |token| {
            let token_range = token.text().range();
            let range = range.intersection(&token_range)?;
            unsafe {
                Some((
                    erase_ab_lt(token.green().into_token().unwrap()),
                    range - token_range.start(),
                ))
            }
        })
    }
}

unsafe fn erase_ab_lt<'a, T: ?Sized>(ab: ArcBorrow<'_, T>) -> ArcBorrow<'a, T> {
    mem::transmute(ab)
}
