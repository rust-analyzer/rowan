use {
    crate::syntax::{Generic, Language, Node},
    std::mem,
    str_index::{StrIndex, StrRange},
};

/// Text of a node in the syntax tree.
#[derive(Debug, Clone)]
pub struct Text {
    pub(super) node: Node<Generic>,
    pub(super) range: StrRange,
}

impl Text {
    pub(crate) fn new(node: Node<Generic>) -> Text {
        let range = node.text_range();
        Text { node, range }
    }

    /// This node.
    pub fn node(&self) -> &Node<Generic> {
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
    /// The leaf nodes that make up this text.
    pub fn leaves<Lang: Language>(&self, lang: Lang) -> impl Iterator<Item = (Node<Lang>, &str)> {
        let range = self.range();
        self.clone().node.with_lang(lang).preorder().filter(|el| el.is_leaf()).filter_map(
            move |token| {
                if range.contains(token.clone().text().range()) {
                    let text = unsafe { erase_ref_lt(&token.green().into_token().unwrap().text) };
                    Some((token, text))
                } else {
                    None
                }
            },
        )
    }

    /// The text fragments that make up this text.
    pub fn chunks(&self) -> impl Iterator<Item = &str> {
        self.leaves(Generic).map(|it| it.1)
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
        for chunk in self.chunks() {
            if !rhs.starts_with(chunk) {
                return false;
            }
            rhs = &rhs[chunk.len()..];
        }
        rhs.is_empty()
    }
}

impl ToString for Text {
    fn to_string(&self) -> String {
        self.chunks().collect()
    }
}

unsafe fn erase_ref_lt<'a, T: ?Sized>(this: &T) -> &'a T {
    mem::transmute(this)
}
