use std::{
    ptr,
    fmt,
    hash::{Hash, Hasher},
    any::Any,
};

use crate::{
    TreeArc, WalkEvent, SyntaxKind,
    SyntaxNode, SyntaxToken, SyntaxElement, SyntaxIndex,
    GreenNode, GreenElement, GreenIndex,
    TextRange,
};

// SyntaxNodes have identity equality semantics
impl PartialEq<SyntaxNode> for SyntaxNode {
    #[inline]
    fn eq(&self, other: &SyntaxNode) -> bool {
        ptr::eq(self, other)
    }
}
impl Eq for SyntaxNode {}
impl Hash for SyntaxNode {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const SyntaxNode).hash(state)
    }
}

impl fmt::Debug for SyntaxNode {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}@{:?}", self.kind(), self.range())
    }
}
impl fmt::Display for SyntaxNode {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.preorder_with_tokens()
            .filter_map(|event| match event {
                WalkEvent::Enter(SyntaxElement::Token(token)) => Some(token.text()),
                _ => None,
            })
            .try_for_each(|it| write!(fmt, "{}", it))
    }
}

impl SyntaxNode {
    /// Creates a new `SyntaxNode`, which becomes the root of the tree.
    #[inline]
    pub fn new(green: GreenNode, data: Option<Box<Any + Send + Sync>>) -> TreeArc<SyntaxNode> {
        Self::new_root(green, data)
    }

    /// Get root data.
    #[inline]
    pub fn root_data(&self) -> Option<&(dyn Any + Send + Sync)> {
        self.root().data.as_ref().map(|it| &**it)
    }

    /// Get the green node for this node
    #[inline]
    pub fn green(&self) -> &GreenNode {
        &self.green
    }

    /// Get kind of this node.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.green.kind()
    }

    /// Get text range, covered by this node.
    #[inline]
    pub fn range(&self) -> TextRange {
        let start_offset = self.parent_data().map_or(0.into(), |it| it.start_offset);
        TextRange::offset_len(start_offset, self.green.text_len())
    }

    /// Get the parent node.
    #[inline]
    pub fn parent(&self) -> Option<&SyntaxNode> {
        self.parent_impl()
    }

    /// Get first child, excluding tokens.
    #[inline]
    pub fn first_child(&self) -> Option<&SyntaxNode> {
        self.get_child(SyntaxIndex(0))
    }

    /// Get the first, including tokens.
    #[inline]
    pub fn first_child_or_token(&self) -> Option<SyntaxElement> {
        let res = match self.green().children().first()? {
            GreenElement::Node(_) => self.first_child()?.into(),
            GreenElement::Token(_) => SyntaxToken {
                parent: self,
                start_offset: self.range().start(),
                index_in_green: GreenIndex(0),
                index_in_parent: SyntaxIndex(0),
            }
            .into(),
        };
        Some(res)
    }

    /// Get last child, excluding tokens.
    #[inline]
    pub fn last_child(&self) -> Option<&SyntaxNode> {
        let idx = self.children_len().prev();
        self.get_child(idx)
    }

    /// Get last child, including tokens.
    pub fn last_child_or_token(&self) -> Option<SyntaxElement> {
        let res = match self.green().children().last()? {
            GreenElement::Node(_) => self.last_child()?.into(),
            GreenElement::Token(t) => SyntaxToken {
                parent: self,
                start_offset: self.range().end() - t.text_len(),
                index_in_green: GreenIndex(self.green().children().len() as u32 - 1),
                index_in_parent: self.children_len(),
            }
            .into(),
        };
        Some(res)
    }

    /// Get next sibling, excluding tokens.
    #[inline]
    pub fn next_sibling(&self) -> Option<&SyntaxNode> {
        let parent = self.parent()?;
        let next_sibling_idx = self.parent_data()?.index_in_parent.next();
        parent.get_child(next_sibling_idx)
    }

    /// Get next sibling, including tokens.
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.parent()?;
        let parent_data = self.parent_data()?;
        let index_in_green = parent_data.index_in_green.next();
        match parent.green().get_child(index_in_green)? {
            GreenElement::Node(_) => self.next_sibling().map(SyntaxElement::from),
            GreenElement::Token(_) => {
                let token = SyntaxToken {
                    parent,
                    start_offset: self.range().end(),
                    index_in_green,
                    index_in_parent: parent_data.index_in_parent.next(),
                };
                Some(token.into())
            }
        }
    }

    /// Get previous sibling, excluding tokens.
    #[inline]
    pub fn prev_sibling(&self) -> Option<&SyntaxNode> {
        let parent = self.parent()?;
        let prev_sibling_idx = self.parent_data()?.index_in_parent.prev();
        parent.get_child(prev_sibling_idx)
    }

    /// Get previous sibling, including tokens.
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.parent()?;
        let parent_data = self.parent_data()?;
        let index_in_green = parent_data.index_in_green.prev();
        match parent.green().get_child(index_in_green)? {
            GreenElement::Node(_) => self.prev_sibling().map(SyntaxElement::from),
            GreenElement::Token(it) => {
                let token = SyntaxToken {
                    parent,
                    start_offset: self.range().start() - it.text_len(),
                    index_in_green,
                    index_in_parent: parent_data.index_in_parent,
                };
                Some(token.into())
            }
        }
    }

    /// Return the leftmost token in the subtree of this node
    #[inline]
    pub fn first_token(&self) -> Option<SyntaxToken> {
        self.first_child_or_token()?.first_token()
    }

    /// Return the rightmost token in the subtree of this node
    #[inline]
    pub fn last_token(&self) -> Option<SyntaxToken> {
        self.last_child_or_token()?.last_token()
    }

    /// Returns a green tree, equal to the green tree this node
    /// belongs two, except with this node substitute. The complexity
    /// of operation is proportional to the depth of the tree
    pub fn replace_with(&self, replacement: GreenNode) -> GreenNode {
        assert_eq!(self.kind(), replacement.kind());
        match self.parent() {
            None => replacement,
            Some(parent) => {
                let me = self.parent_data().unwrap().index_in_green;
                let mut replacement = Some(replacement);
                let children: Box<[_]> = parent
                    .green()
                    .children()
                    .iter()
                    .enumerate()
                    .map(|(i, child)| {
                        if i as u32 == me.0 {
                            replacement.take().unwrap().into()
                        } else {
                            child.clone()
                        }
                    })
                    .collect();
                assert!(replacement.is_none());
                let new_parent = GreenNode::new(parent.kind(), children);
                parent.replace_with(new_parent)
            }
        }
    }
}
