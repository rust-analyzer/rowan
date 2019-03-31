use std::{
    ptr,
    fmt,
    hash::{Hash, Hasher},
};

use crate::{
    TreeArc, Types, WalkEvent,
    SyntaxNode, SyntaxToken, SyntaxElement, SyntaxIndex,
    GreenNode, GreenElement, GreenIndex,
    TextRange,
};

// SyntaxNodes have identity equality semantics
impl<T: Types> PartialEq<SyntaxNode<T>> for SyntaxNode<T> {
    fn eq(&self, other: &SyntaxNode<T>) -> bool {
        ptr::eq(self, other)
    }
}
impl<T: Types> Eq for SyntaxNode<T> {}
impl<T: Types> Hash for SyntaxNode<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const SyntaxNode<T>).hash(state)
    }
}

impl<T: Types> fmt::Debug for SyntaxNode<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}@{:?}", self.kind(), self.range())
    }
}
impl<T: Types> fmt::Display for SyntaxNode<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.preorder_with_tokens()
            .filter_map(|event| match event {
                WalkEvent::Enter(SyntaxElement::Token(token)) => Some(token.text()),
                _ => None,
            })
            .try_for_each(|it| write!(fmt, "{}", it))
    }
}

impl<T: Types> SyntaxNode<T> {
    /// Creates a new `SyntaxNode`, which becomes the root of the tree.
    pub fn new(green: GreenNode<T>, data: T::RootData) -> TreeArc<T, SyntaxNode<T>> {
        Self::new_root(green, data)
    }

    /// Get root data.
    pub fn root_data(&self) -> &T::RootData {
        &self.root().data
    }

    /// Get the green node for this node
    pub fn green(&self) -> &GreenNode<T> {
        &self.green
    }

    /// Get kind of this node.
    pub fn kind(&self) -> T::Kind {
        self.green.kind()
    }

    /// Get text range, covered by this node.
    pub fn range(&self) -> TextRange {
        let start_offset = self.parent_data().map_or(0.into(), |it| it.start_offset);
        TextRange::offset_len(start_offset, self.green.text_len())
    }

    /// Get the parent node.
    pub fn parent(&self) -> Option<&SyntaxNode<T>> {
        self.parent_impl()
    }

    /// Get first child, excluding tokens.
    pub fn first_child(&self) -> Option<&SyntaxNode<T>> {
        self.get_child(SyntaxIndex(0))
    }

    /// Get the first, including tokens.
    pub fn first_child_or_token(&self) -> Option<SyntaxElement<T>> {
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
    pub fn last_child(&self) -> Option<&SyntaxNode<T>> {
        let idx = self.children_len().prev();
        self.get_child(idx)
    }

    /// Get last child, including tokens.
    pub fn last_child_or_token(&self) -> Option<SyntaxElement<T>> {
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
    pub fn next_sibling(&self) -> Option<&SyntaxNode<T>> {
        let parent = self.parent()?;
        let next_sibling_idx = self.parent_data()?.index_in_parent.next();
        parent.get_child(next_sibling_idx)
    }

    /// Get next sibling, including tokens.
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<T>> {
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
    pub fn prev_sibling(&self) -> Option<&SyntaxNode<T>> {
        let parent = self.parent()?;
        let prev_sibling_idx = self.parent_data()?.index_in_parent.prev();
        parent.get_child(prev_sibling_idx)
    }

    /// Get previous sibling, including tokens.
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<T>> {
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
    pub fn first_token(&self) -> Option<SyntaxToken<T>> {
        self.first_child_or_token()?.first_token()
    }

    /// Return the rightmost token in the subtree of this node
    pub fn last_token(&self) -> Option<SyntaxToken<T>> {
        self.last_child_or_token()?.last_token()
    }

    /// Returns a green tree, equal to the green tree this node
    /// belongs two, except with this node substitute. The complexity
    /// of operation is proportional to the depth of the tree
    pub fn replace_with(&self, replacement: GreenNode<T>) -> GreenNode<T> {
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
