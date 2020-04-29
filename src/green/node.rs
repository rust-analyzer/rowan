use {
    crate::{
        green::{GreenElement, GreenElementRef, NodeCache, SyntaxKind},
        ArcBorrow, NodeOrToken, TextSize,
    },
    std::{fmt, iter::FusedIterator, mem, sync::Arc},
};

/// Internal node in the immutable tree.
/// It has other nodes and tokens as children.
#[repr(transparent)]
#[derive(Eq, PartialEq, Hash)]
pub struct GreenNode {
    imp: sorbus::green::Node,
}

impl fmt::Debug for GreenNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("GreenNode")
            .field("kind", &self.kind())
            .field("text_len", &self.text_len())
            .field("children", &self.children())
            .finish()
    }
}

impl GreenNode {
    /// Creates a new node, without any caching.
    #[inline]
    #[deprecated(note = "use the builder API to deduplicate nodes")]
    pub fn new<I>(kind: SyntaxKind, children: I) -> Arc<GreenNode>
    where
        I: IntoIterator<Item = GreenElement>,
        I::IntoIter: ExactSizeIterator,
    {
        NodeCache::new().node(kind, children)
    }

    /// The kind of this node.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        SyntaxKind(self.imp.kind().0)
    }

    /// The length of the text covered by this node.
    #[inline]
    pub fn text_len(&self) -> TextSize {
        self.imp.len()
    }

    /// The children of this node.
    #[inline]
    pub fn children(&self) -> Children<'_> {
        Children { imp: self.imp.children() }
    }

    pub(crate) fn child_by_offset(
        &self,
        offset: TextSize,
    ) -> Option<(usize, TextSize, ArcBorrow<'_, GreenNode>)> {
        if offset > self.text_len() {
            return None;
        }
        let index = self.imp.index_of_offset(offset);
        let (offset, node) = self.imp.children().with_offsets().get(index)?;
        let node = *node.as_node()?;
        unsafe { Some((index, offset, mem::transmute(node))) }
    }
}

#[derive(Clone)]
pub struct Children<'a> {
    imp: sorbus::green::Children<'a>,
}

impl fmt::Debug for Children<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.clone()).finish()
    }
}

// NB: forward everything that sorbus::green::Children specializes
fn sorbus_child_to_rowan<'a>(
    el: sorbus::NodeOrToken<
        ArcBorrow<'a, sorbus::green::Node>,
        ArcBorrow<'a, sorbus::green::Token>,
    >,
) -> GreenElementRef<'a> {
    match el {
        sorbus::NodeOrToken::Node(node) => unsafe {
            // transmute from sorbus to rowan nominal type
            NodeOrToken::Node(mem::transmute(node))
        },
        sorbus::NodeOrToken::Token(token) => unsafe {
            // transmute from sorbus to rowan nominal type
            NodeOrToken::Token(mem::transmute(token))
        },
    }
}

impl<'a> Iterator for Children<'a> {
    type Item = GreenElementRef<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.imp.next().map(sorbus_child_to_rowan)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.imp.size_hint()
    }

    fn count(self) -> usize {
        self.imp.count()
    }

    fn last(mut self) -> Option<Self::Item> {
        self.next_back()
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.imp.nth(n).map(sorbus_child_to_rowan)
    }

    fn fold<B, F>(self, init: B, mut f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        self.imp.fold(init, |acc, el| f(acc, sorbus_child_to_rowan(el)))
    }
}

impl ExactSizeIterator for Children<'_> {
    #[inline(always)]
    fn len(&self) -> usize {
        self.imp.len()
    }
}

impl DoubleEndedIterator for Children<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.imp.next_back().map(sorbus_child_to_rowan)
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.imp.nth_back(n).map(sorbus_child_to_rowan)
    }

    fn rfold<B, F>(self, init: B, mut f: F) -> B
    where
        F: FnMut(B, Self::Item) -> B,
    {
        self.imp.rfold(init, |acc, el| f(acc, sorbus_child_to_rowan(el)))
    }
}

impl FusedIterator for Children<'_> {}
