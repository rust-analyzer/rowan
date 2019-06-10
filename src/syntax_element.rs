use crate::{SyntaxNode, SyntaxToken, SyntaxKind, TextRange};

/// Either a SyntaxToken or SyntaxNode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElement<'a> {
    /// SyntaxNode
    Node(&'a SyntaxNode),
    /// SyntaxToken
    Token(SyntaxToken<'a>),
}
impl<'a> From<&'a SyntaxNode> for SyntaxElement<'a> {
    fn from(node: &'a SyntaxNode) -> SyntaxElement<'a> {
        SyntaxElement::Node(node)
    }
}
impl<'a> From<SyntaxToken<'a>> for SyntaxElement<'a> {
    fn from(token: SyntaxToken<'a>) -> SyntaxElement<'a> {
        SyntaxElement::Token(token)
    }
}

impl<'a> SyntaxElement<'a> {
    /// If this element is a token, return it.
    #[inline]
    pub fn as_token(&self) -> Option<SyntaxToken<'a>> {
        match *self {
            SyntaxElement::Token(token) => Some(token),
            SyntaxElement::Node(_) => None,
        }
    }
    /// If this element is a node, return it.
    #[inline]
    pub fn as_node(&self) -> Option<&'a SyntaxNode> {
        match self {
            SyntaxElement::Node(node) => Some(node),
            SyntaxElement::Token(_) => None,
        }
    }
    /// Kind of this element.
    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxElement::Node(it) => it.kind(),
            SyntaxElement::Token(it) => it.kind(),
        }
    }
    /// Text range, covered by this element.
    #[inline]
    pub fn range(&self) -> TextRange {
        match self {
            SyntaxElement::Node(it) => it.range(),
            SyntaxElement::Token(it) => it.range(),
        }
    }
    /// Parent node, containing this element.
    #[inline]
    pub fn parent(&self) -> Option<&'a SyntaxNode> {
        match self {
            SyntaxElement::Node(it) => it.parent(),
            SyntaxElement::Token(it) => Some(it.parent()),
        }
    }
    /// Next sibling, including tokens.
    #[inline]
    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<'a>> {
        match self {
            SyntaxElement::Node(it) => it.next_sibling_or_token(),
            SyntaxElement::Token(it) => it.next_sibling_or_token(),
        }
    }
    /// Next sibling, including tokens.
    #[inline]
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<'a>> {
        match self {
            SyntaxElement::Node(it) => it.prev_sibling_or_token(),
            SyntaxElement::Token(it) => it.prev_sibling_or_token(),
        }
    }
    /// Return the leftmost token in the subtree of this element.
    #[inline]
    pub(crate) fn first_token(&self) -> Option<SyntaxToken<'a>> {
        match self {
            SyntaxElement::Node(node) => node.first_token(),
            SyntaxElement::Token(token) => Some(*token),
        }
    }
    /// Return the rightmost token in the subtree of this element.
    #[inline]
    pub(crate) fn last_token(&self) -> Option<SyntaxToken<'a>> {
        match self {
            SyntaxElement::Node(node) => node.last_token(),
            SyntaxElement::Token(token) => Some(*token),
        }
    }
}
