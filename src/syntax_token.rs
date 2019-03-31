use std::{fmt, hash::{Hasher, Hash}};

use crate::{
    Types, TextUnit, SmolStr, TextRange,
    SyntaxNode, SyntaxElement, SyntaxIndex,
    GreenToken, GreenNode, GreenElement, GreenIndex,
};

/// A token (leaf node) in a syntax tree.
///
/// A token can't exist in isolation, it is always attached to a parent Node.
pub struct SyntaxToken<'a, T: Types> {
    pub(crate) parent: &'a SyntaxNode<T>,
    pub(crate) start_offset: TextUnit,
    /// Index of this token in the green node
    pub(crate) index_in_green: GreenIndex,
    /// Index of the following SyntaxNode in the parent. 0 for tokens which come
    /// before the first non-token child.
    pub(crate) index_in_parent: SyntaxIndex,
}

impl<'a, T: Types> Clone for SyntaxToken<'a, T> {
    fn clone(&self) -> SyntaxToken<'a, T> {
        *self
    }
}

impl<'a, T: Types> Copy for SyntaxToken<'a, T> {}

impl<'a, 'b, T: Types> PartialEq<SyntaxToken<'a, T>> for SyntaxToken<'b, T> {
    fn eq(&self, other: &SyntaxToken<T>) -> bool {
        (self.parent(), self.index_in_green) == (other.parent(), other.index_in_green)
    }
}
impl<'a, T: Types> Eq for SyntaxToken<'a, T> {}
impl<'a, T: Types> Hash for SyntaxToken<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.parent(), self.index_in_green).hash(state)
    }
}

impl<'a, T: Types> fmt::Debug for SyntaxToken<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}@{:?}", self.kind(), self.range())
    }
}
impl<'a, T: Types> fmt::Display for SyntaxToken<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.write_str(self.text())
    }
}

impl<'a, T: Types> SyntaxToken<'a, T> {
    /// Kind of this token.
    pub fn kind(&self) -> T::Kind {
        self.green().kind()
    }
    /// Text of this token.
    pub fn text(&self) -> &'a SmolStr {
        self.green().text()
    }
    /// Text range, covered by this token.
    pub fn range(&self) -> TextRange {
        TextRange::offset_len(self.start_offset, self.green().text_len())
    }
    /// Parent node, containing this token.
    pub fn parent(&self) -> &'a SyntaxNode<T> {
        self.parent
    }
    /// Next sibling of this tokens, including both nodes and tokens.
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<'a, T>> {
        let index_in_green = self.index_in_green.next();
        let green = self.parent().green.get_child(index_in_green)?;
        let element = match green {
            GreenElement::Token(_) => {
                let token = SyntaxToken {
                    parent: self.parent(),
                    start_offset: self.start_offset + self.green().text_len(),
                    index_in_green,
                    index_in_parent: self.index_in_parent,
                };
                token.into()
            }
            GreenElement::Node(_) => self.parent().get_child(self.index_in_parent).unwrap().into(),
        };
        Some(element)
    }
    /// Previous sibling of this tokens, including both nodes and tokens.
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<'a, T>> {
        let index_in_green = self.index_in_green.prev();
        let green = self.parent().green.get_child(index_in_green)?;
        let element = match green {
            GreenElement::Token(it) => {
                let token = SyntaxToken {
                    parent: self.parent(),
                    start_offset: self.start_offset - it.text_len(),
                    index_in_green,
                    index_in_parent: self.index_in_parent,
                };
                token.into()
            }
            GreenElement::Node(_) => {
                self.parent().get_child(self.index_in_parent.prev()).unwrap().into()
            }
        };
        Some(element)
    }
    /// Next token in the file (i.e, not necessary a sibling)
    pub fn next_token(&self) -> Option<SyntaxToken<'a, T>> {
        match self.next_sibling_or_token() {
            Some(element) => element.first_token(),
            None => self.parent().ancestors().find_map(|it| it.next_sibling_or_token()).and_then(|element| element.first_token())
        }
    }
    /// Previous token in the file (i.e, not necessary a sibling)
    pub fn prev_token(&self) -> Option<SyntaxToken<'a, T>> {
        match self.prev_sibling_or_token() {
            Some(element) => element.last_token(),
            None => self.parent().ancestors().find_map(|it| it.prev_sibling_or_token()).and_then(|element| element.last_token())
        }
    }

    /// Returns a green tree, equal to the green tree this token
    /// belongs two, except with this token substitute. The complexity
    /// of operation is proportional to the depth of the tree
    pub fn replace_with(&self, replacement: GreenToken<T>) -> GreenNode<T> {
        assert_eq!(self.kind(), replacement.kind());
        let mut replacement = Some(replacement);
        let parent = self.parent();
        let me = self.index_in_green;

        let children: Box<[_]> =
            parent
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

    fn green(&self) -> &'a GreenToken<T> {
        match self.parent.green.get_child(self.index_in_green) {
            Some(GreenElement::Token(it)) => it,
            _ => unreachable!(),
        }
    }
}
