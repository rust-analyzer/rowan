use {
    crate::syntax::untyped::{Node, NodeKind},
    std::mem,
    str_index::{StrIndex, StrRange},
};

/// Text of a node in the syntax tree.
#[derive(Debug, Clone)]
pub struct Text {
    pub(super) node: Node,
    pub(super) range: StrRange,
}

impl Text {
    pub(crate) fn new(node: Node) -> Text {
        let offset = match node.inner.kind {
            NodeKind::Child { offset, .. } => offset,
            _ => 0.into(),
        };
        let range = offset.range_for(node.green().text_len());
        Text { node, range }
    }

    /// This node.
    pub fn node(&self) -> &Node {
        &self.node
    }

    /// Range of this text.
    pub fn range(&self) -> StrRange {
        self.range
    }

    /// Length of this text.
    pub fn len(&self) -> StrIndex {
        self.range.len()
    }

    /// Is this text empty?
    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }
}

impl Text {
    /// Iterate all string fragments that make up this text.
    pub fn chunks(&self) -> impl Iterator<Item = (Node, &str)> {
        let range = self.range();
        self.node.clone().preorder().filter(|el| el.green().is_token()).filter_map(move |token| {
            if range.contains(token.text().range()) {
                let text = unsafe { erase_ref_lt(&token.green().into_token().unwrap().text) };
                Some((token, text))
            } else {
                None
            }
        })
    }

    /// Slice a subset of this text.
    pub fn slice(&self, range: impl Into<StrRange>) -> Text {
        let range = range.into();
        assert!(self.range().contains(range), "invalid slice `{:?}[{:?}]`", self.range(), range);
        Text { node: self.node.clone(), range }
    }
}

impl PartialEq<str> for Text {
    fn eq(&self, mut rhs: &str) -> bool {
        for (_, chunk) in self.chunks() {
            if !rhs.starts_with(chunk) {
                return false;
            }
            rhs = &rhs[chunk.len()..];
        }
        rhs.is_empty()
    }
}

unsafe fn erase_ref_lt<'a, T: ?Sized>(this: &T) -> &'a T {
    mem::transmute(this)
}
