use std::{fmt, marker::PhantomData};

use crate::{
    cursor, GreenNode, GreenToken, SmolStr, SyntaxText, TextRange, TextUnit, TokenAtOffset,
    WalkEvent,
};

pub trait Props: Sized + Clone + Copy + fmt::Debug + Eq + Ord + std::hash::Hash {
    fn debug_kind(kind: &SyntaxKind<Self>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&kind.raw, f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxKind<P> {
    raw: cursor::SyntaxKind,
    _p: PhantomData<P>,
}

impl<P: Props> From<cursor::SyntaxKind> for SyntaxKind<P> {
    fn from(raw: cursor::SyntaxKind) -> SyntaxKind<P> {
        SyntaxKind { raw, _p: PhantomData }
    }
}

impl<P: Props> From<SyntaxKind<P>> for cursor::SyntaxKind {
    fn from(kind: SyntaxKind<P>) -> cursor::SyntaxKind {
        kind.raw
    }
}

impl<P: Props> fmt::Debug for SyntaxKind<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Props::debug_kind(self, f)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNode<P: Props> {
    raw: cursor::SyntaxNode,
    _p: PhantomData<P>,
}

impl<P: Props> From<cursor::SyntaxNode> for SyntaxNode<P> {
    fn from(raw: cursor::SyntaxNode) -> SyntaxNode<P> {
        SyntaxNode { raw, _p: PhantomData }
    }
}

impl<P: Props> From<SyntaxNode<P>> for cursor::SyntaxNode {
    fn from(node: SyntaxNode<P>) -> cursor::SyntaxNode {
        node.raw
    }
}

impl<P: Props> fmt::Debug for SyntaxNode<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind(), self.text_range())
    }
}

impl<P: Props> fmt::Display for SyntaxNode<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SyntaxToken<P: Props> {
    raw: cursor::SyntaxToken,
    _p: PhantomData<P>,
}

impl<P: Props> From<cursor::SyntaxToken> for SyntaxToken<P> {
    fn from(raw: cursor::SyntaxToken) -> SyntaxToken<P> {
        SyntaxToken { raw, _p: PhantomData }
    }
}

impl<P: Props> From<SyntaxToken<P>> for cursor::SyntaxToken {
    fn from(token: SyntaxToken<P>) -> cursor::SyntaxToken {
        token.raw
    }
}

impl<P: Props> fmt::Debug for SyntaxToken<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind(), self.text_range())?;
        if self.text().len() < 25 {
            return write!(f, " {:?}", self.text());
        }
        let text = self.text().as_str();
        for idx in 21..25 {
            if text.is_char_boundary(idx) {
                let text = format!("{} ...", &text[..idx]);
                return write!(f, " {:?}", text);
            }
        }
        unreachable!()
    }
}

impl<P: Props> fmt::Display for SyntaxToken<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SyntaxElement<P: Props> {
    Node(SyntaxNode<P>),
    Token(SyntaxToken<P>),
}

impl<P: Props> From<cursor::SyntaxElement> for SyntaxElement<P> {
    fn from(raw: cursor::SyntaxElement) -> SyntaxElement<P> {
        match raw {
            cursor::SyntaxElement::Node(it) => SyntaxElement::Node(it.into()),
            cursor::SyntaxElement::Token(it) => SyntaxElement::Token(it.into()),
        }
    }
}

impl<P: Props> From<SyntaxElement<P>> for cursor::SyntaxElement {
    fn from(element: SyntaxElement<P>) -> cursor::SyntaxElement {
        match element {
            SyntaxElement::Node(it) => cursor::SyntaxElement::Node(it.into()),
            SyntaxElement::Token(it) => cursor::SyntaxElement::Token(it.into()),
        }
    }
}

impl<P: Props> From<SyntaxNode<P>> for SyntaxElement<P> {
    fn from(node: SyntaxNode<P>) -> SyntaxElement<P> {
        SyntaxElement::Node(node)
    }
}

impl<P: Props> From<SyntaxToken<P>> for SyntaxElement<P> {
    fn from(token: SyntaxToken<P>) -> SyntaxElement<P> {
        SyntaxElement::Token(token)
    }
}

impl<P: Props> fmt::Debug for SyntaxElement<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxElement::Node(it) => fmt::Debug::fmt(it, f),
            SyntaxElement::Token(it) => fmt::Debug::fmt(it, f),
        }
    }
}

impl<P: Props> fmt::Display for SyntaxElement<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxElement::Node(it) => fmt::Display::fmt(it, f),
            SyntaxElement::Token(it) => fmt::Display::fmt(it, f),
        }
    }
}

impl<P> SyntaxKind<P> {
    pub const fn new(kind: u16) -> SyntaxKind<P> {
        SyntaxKind { raw: cursor::SyntaxKind(kind), _p: PhantomData }
    }
}

impl<P: Props> SyntaxNode<P> {
    pub fn new_root(green: GreenNode) -> SyntaxNode<P> {
        SyntaxNode::from(cursor::SyntaxNode::new_root(green))
    }
    pub fn replace_with(&self, replacement: GreenNode) -> GreenNode {
        self.raw.replace_with(replacement)
    }

    pub fn kind(&self) -> SyntaxKind<P> {
        SyntaxKind::from(self.raw.kind())
    }

    pub fn text_range(&self) -> TextRange {
        self.raw.text_range()
    }

    pub fn text(&self) -> SyntaxText {
        self.raw.text()
    }

    pub fn green(&self) -> &GreenNode {
        self.raw.green()
    }

    pub fn parent(&self) -> Option<SyntaxNode<P>> {
        self.raw.parent().map(Self::from)
    }

    pub fn children(&self) -> SyntaxNodeChildren<P> {
        SyntaxNodeChildren { raw: self.raw.children(), _p: PhantomData }
    }

    pub fn children_with_tokens(&self) -> SyntaxElementChildren<P> {
        SyntaxElementChildren { raw: self.raw.children_with_tokens(), _p: PhantomData }
    }

    pub fn first_child(&self) -> Option<SyntaxNode<P>> {
        self.raw.first_child().map(Self::from)
    }

    pub fn first_child_or_token(&self) -> Option<SyntaxElement<P>> {
        self.raw.first_child_or_token().map(SyntaxElement::from)
    }

    pub fn last_child(&self) -> Option<SyntaxNode<P>> {
        self.raw.last_child().map(Self::from)
    }

    pub fn last_child_or_token(&self) -> Option<SyntaxElement<P>> {
        self.raw.last_child_or_token().map(SyntaxElement::from)
    }

    pub fn next_sibling(&self) -> Option<SyntaxNode<P>> {
        self.raw.next_sibling().map(Self::from)
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<P>> {
        self.raw.next_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn prev_sibling(&self) -> Option<SyntaxNode<P>> {
        self.raw.prev_sibling().map(Self::from)
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<P>> {
        self.raw.prev_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn first_token(&self) -> Option<SyntaxToken<P>> {
        self.raw.first_token().map(SyntaxToken::from)
    }

    pub fn last_token(&self) -> Option<SyntaxToken<P>> {
        self.raw.last_token().map(SyntaxToken::from)
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<P>> {
        self.raw.ancestors().map(SyntaxNode::from)
    }

    pub fn descendants(&self) -> impl Iterator<Item = SyntaxNode<P>> {
        self.raw.descendants().map(SyntaxNode::from)
    }

    pub fn descendants_with_tokens(&self) -> impl Iterator<Item = SyntaxElement<P>> {
        self.raw.descendants_with_tokens().map(SyntaxElement::from)
    }

    pub fn preorder(&self) -> impl Iterator<Item = WalkEvent<SyntaxNode<P>>> {
        self.raw.preorder().map(|event| event.map(SyntaxNode::from))
    }

    pub fn preorder_with_tokens(&self) -> impl Iterator<Item = WalkEvent<SyntaxElement<P>>> {
        self.raw.preorder_with_tokens().map(|event| event.map(SyntaxElement::from))
    }

    pub fn token_at_offset(&self, offset: TextUnit) -> TokenAtOffset<SyntaxToken<P>> {
        self.raw.token_at_offset(offset).map(SyntaxToken::from)
    }

    pub fn covering_element(&self, range: TextRange) -> SyntaxElement<P> {
        SyntaxElement::from(self.raw.covering_element(range))
    }
}

impl<P: Props> SyntaxToken<P> {
    pub fn replace_with(&self, new_token: GreenToken) -> GreenNode {
        self.raw.replace_with(new_token)
    }

    pub fn kind(&self) -> SyntaxKind<P> {
        SyntaxKind::from(self.raw.kind())
    }

    pub fn text_range(&self) -> TextRange {
        self.raw.text_range()
    }

    pub fn text(&self) -> &SmolStr {
        self.raw.text()
    }

    pub fn green(&self) -> &GreenToken {
        self.raw.green()
    }

    pub fn parent(&self) -> SyntaxNode<P> {
        SyntaxNode::from(self.raw.parent())
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<P>> {
        self.raw.next_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<P>> {
        self.raw.prev_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn next_token(&self) -> Option<SyntaxToken<P>> {
        self.raw.next_token().map(SyntaxToken::from)
    }

    pub fn prev_token(&self) -> Option<SyntaxToken<P>> {
        self.raw.prev_token().map(SyntaxToken::from)
    }
}

impl<P: Props> SyntaxElement<P> {
    pub fn text_range(&self) -> TextRange {
        match self {
            SyntaxElement::Node(it) => it.text_range(),
            SyntaxElement::Token(it) => it.text_range(),
        }
    }

    pub fn kind(&self) -> SyntaxKind<P> {
        match self {
            SyntaxElement::Node(it) => it.kind(),
            SyntaxElement::Token(it) => it.kind(),
        }
    }

    pub fn parent(&self) -> Option<SyntaxNode<P>> {
        match self {
            SyntaxElement::Node(it) => it.parent(),
            SyntaxElement::Token(it) => Some(it.parent()),
        }
    }

    pub fn as_node(&self) -> Option<&SyntaxNode<P>> {
        match self {
            SyntaxElement::Node(it) => Some(it),
            SyntaxElement::Token(_) => None,
        }
    }

    pub fn into_node(self) -> Option<SyntaxNode<P>> {
        match self {
            SyntaxElement::Node(it) => Some(it),
            SyntaxElement::Token(_) => None,
        }
    }

    pub fn as_token(&self) -> Option<&SyntaxToken<P>> {
        match self {
            SyntaxElement::Node(_) => None,
            SyntaxElement::Token(it) => Some(it),
        }
    }

    pub fn into_token(self) -> Option<SyntaxToken<P>> {
        match self {
            SyntaxElement::Node(_) => None,
            SyntaxElement::Token(it) => Some(it),
        }
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<P>> {
        match self {
            SyntaxElement::Node(it) => it.next_sibling_or_token(),
            SyntaxElement::Token(it) => it.next_sibling_or_token(),
        }
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<P>> {
        match self {
            SyntaxElement::Node(it) => it.prev_sibling_or_token(),
            SyntaxElement::Token(it) => it.prev_sibling_or_token(),
        }
    }
}

pub struct SyntaxNodeChildren<P: Props> {
    raw: cursor::SyntaxNodeChildren,
    _p: PhantomData<P>,
}

impl<P: Props> Iterator for SyntaxNodeChildren<P> {
    type Item = SyntaxNode<P>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(SyntaxNode::from)
    }
}

pub struct SyntaxElementChildren<P: Props> {
    raw: cursor::SyntaxElementChildren,
    _p: PhantomData<P>,
}

impl<P: Props> Iterator for SyntaxElementChildren<P> {
    type Item = SyntaxElement<P>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(SyntaxElement::from)
    }
}
