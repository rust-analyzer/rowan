use std::hash::{Hash, Hasher};

use crate::{Types, SyntaxNode, SyntaxToken, TextRange};

/// Either a SyntaxToken or SyntaxNode.
#[derive(Debug)]
pub enum SyntaxElement<'a, T: Types> {
    Node(&'a SyntaxNode<T>),
    Token(SyntaxToken<'a, T>),
}

impl<'a, T: Types> Clone for SyntaxElement<'a, T> {
    fn clone(&self) -> SyntaxElement<'a, T> {
        *self
    }
}
impl<'a, T: Types> Copy for SyntaxElement<'a, T> {}

impl<'a, T: Types> PartialEq<SyntaxElement<'a, T>> for SyntaxElement<'a, T> {
    fn eq(&self, other: &SyntaxElement<T>) -> bool {
        match (self, other) {
            (SyntaxElement::Node(n1), SyntaxElement::Node(n2)) => n1 == n2,
            (SyntaxElement::Token(t1), SyntaxElement::Token(t2)) => t1 == t2,
            _ => false,
        }
    }
}
impl<'a, T: Types> Eq for SyntaxElement<'a, T> {}
impl<'a, T: Types> Hash for SyntaxElement<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SyntaxElement::Node(it) => it.hash(state),
            SyntaxElement::Token(it) => it.hash(state),
        }
    }
}

impl<'a, T: Types> From<&'a SyntaxNode<T>> for SyntaxElement<'a, T> {
    fn from(node: &'a SyntaxNode<T>) -> SyntaxElement<'a, T> {
        SyntaxElement::Node(node)
    }
}
impl<'a, T: Types> From<SyntaxToken<'a, T>> for SyntaxElement<'a, T> {
    fn from(token: SyntaxToken<'a, T>) -> SyntaxElement<'a, T> {
        SyntaxElement::Token(token)
    }
}

impl<'a, T: Types> SyntaxElement<'a, T> {
    /// Kind of this element.
    pub fn kind(&self) -> T::Kind {
        match self {
            SyntaxElement::Node(it) => it.kind(),
            SyntaxElement::Token(it) => it.kind(),
        }
    }
    /// Text range, covered by this element.
    pub fn range(&self) -> TextRange {
        match self {
            SyntaxElement::Node(it) => it.range(),
            SyntaxElement::Token(it) => it.range(),
        }
    }
    /// Parent node, containing this element.
    pub fn parent(&self) -> Option<&'a SyntaxNode<T>> {
        match self {
            SyntaxElement::Node(it) => it.parent(),
            SyntaxElement::Token(it) => Some(it.parent()),
        }
    }
    /// Next sibling, including tokens.
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<'a, T>> {
        match self {
            SyntaxElement::Node(it) => it.next_sibling_or_token(),
            SyntaxElement::Token(it) => it.next_sibling_or_token(),
        }
    }
    /// Next sibling, including tokens.
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<'a, T>> {
        match self {
            SyntaxElement::Node(it) => it.prev_sibling_or_token(),
            SyntaxElement::Token(it) => it.prev_sibling_or_token(),
        }
    }
    /// Return the leftmost token in the subtree of this element.
    pub(crate) fn first_token(&self) -> Option<SyntaxToken<'a, T>> {
        match self {
            SyntaxElement::Node(node) => node.first_token(),
            SyntaxElement::Token(token) => Some(*token),
        }
    }
    /// Return the rightmost token in the subtree of this element.
    pub(crate) fn last_token(&self) -> Option<SyntaxToken<'a, T>> {
        match self {
            SyntaxElement::Node(node) => node.last_token(),
            SyntaxElement::Token(token) => Some(*token),
        }
    }
}
