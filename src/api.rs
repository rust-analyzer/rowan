use std::{fmt, marker::PhantomData};

use crate::{
    cursor, GreenNode, GreenToken, SmolStr, SyntaxText, TextRange, TextUnit, TokenAtOffset,
    WalkEvent,
};

pub trait Language: Sized + Clone + Copy + fmt::Debug + Eq + Ord + std::hash::Hash {
    fn debug_kind(kind: &SyntaxKind<Self>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&kind.raw, f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxKind<L> {
    raw: cursor::SyntaxKind,
    _p: PhantomData<L>,
}

impl<L: Language> From<cursor::SyntaxKind> for SyntaxKind<L> {
    fn from(raw: cursor::SyntaxKind) -> SyntaxKind<L> {
        SyntaxKind { raw, _p: PhantomData }
    }
}

impl<L: Language> From<SyntaxKind<L>> for cursor::SyntaxKind {
    fn from(kind: SyntaxKind<L>) -> cursor::SyntaxKind {
        kind.raw
    }
}

impl<L: Language> fmt::Debug for SyntaxKind<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Language::debug_kind(self, f)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SyntaxNode<L: Language> {
    raw: cursor::SyntaxNode,
    _p: PhantomData<L>,
}

impl<L: Language> From<cursor::SyntaxNode> for SyntaxNode<L> {
    fn from(raw: cursor::SyntaxNode) -> SyntaxNode<L> {
        SyntaxNode { raw, _p: PhantomData }
    }
}

impl<L: Language> From<SyntaxNode<L>> for cursor::SyntaxNode {
    fn from(node: SyntaxNode<L>) -> cursor::SyntaxNode {
        node.raw
    }
}

impl<L: Language> fmt::Debug for SyntaxNode<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind(), self.text_range())
    }
}

impl<L: Language> fmt::Display for SyntaxNode<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SyntaxToken<L: Language> {
    raw: cursor::SyntaxToken,
    _p: PhantomData<L>,
}

impl<L: Language> From<cursor::SyntaxToken> for SyntaxToken<L> {
    fn from(raw: cursor::SyntaxToken) -> SyntaxToken<L> {
        SyntaxToken { raw, _p: PhantomData }
    }
}

impl<L: Language> From<SyntaxToken<L>> for cursor::SyntaxToken {
    fn from(token: SyntaxToken<L>) -> cursor::SyntaxToken {
        token.raw
    }
}

impl<L: Language> fmt::Debug for SyntaxToken<L> {
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

impl<L: Language> fmt::Display for SyntaxToken<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SyntaxElement<L: Language> {
    Node(SyntaxNode<L>),
    Token(SyntaxToken<L>),
}

impl<L: Language> From<cursor::SyntaxElement> for SyntaxElement<L> {
    fn from(raw: cursor::SyntaxElement) -> SyntaxElement<L> {
        match raw {
            cursor::SyntaxElement::Node(it) => SyntaxElement::Node(it.into()),
            cursor::SyntaxElement::Token(it) => SyntaxElement::Token(it.into()),
        }
    }
}

impl<L: Language> From<SyntaxElement<L>> for cursor::SyntaxElement {
    fn from(element: SyntaxElement<L>) -> cursor::SyntaxElement {
        match element {
            SyntaxElement::Node(it) => cursor::SyntaxElement::Node(it.into()),
            SyntaxElement::Token(it) => cursor::SyntaxElement::Token(it.into()),
        }
    }
}

impl<L: Language> From<SyntaxNode<L>> for SyntaxElement<L> {
    fn from(node: SyntaxNode<L>) -> SyntaxElement<L> {
        SyntaxElement::Node(node)
    }
}

impl<L: Language> From<SyntaxToken<L>> for SyntaxElement<L> {
    fn from(token: SyntaxToken<L>) -> SyntaxElement<L> {
        SyntaxElement::Token(token)
    }
}

impl<L: Language> fmt::Debug for SyntaxElement<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxElement::Node(it) => fmt::Debug::fmt(it, f),
            SyntaxElement::Token(it) => fmt::Debug::fmt(it, f),
        }
    }
}

impl<L: Language> fmt::Display for SyntaxElement<L> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxElement::Node(it) => fmt::Display::fmt(it, f),
            SyntaxElement::Token(it) => fmt::Display::fmt(it, f),
        }
    }
}

impl<L> SyntaxKind<L> {
    pub const fn new(kind: u16) -> SyntaxKind<L> {
        SyntaxKind { raw: cursor::SyntaxKind(kind), _p: PhantomData }
    }
}

impl<L: Language> SyntaxNode<L> {
    pub fn new_root(green: GreenNode) -> SyntaxNode<L> {
        SyntaxNode::from(cursor::SyntaxNode::new_root(green))
    }
    pub fn replace_with(&self, replacement: GreenNode) -> GreenNode {
        self.raw.replace_with(replacement)
    }

    pub fn kind(&self) -> SyntaxKind<L> {
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

    pub fn parent(&self) -> Option<SyntaxNode<L>> {
        self.raw.parent().map(Self::from)
    }

    pub fn children(&self) -> SyntaxNodeChildren<L> {
        SyntaxNodeChildren { raw: self.raw.children(), _p: PhantomData }
    }

    pub fn children_with_tokens(&self) -> SyntaxElementChildren<L> {
        SyntaxElementChildren { raw: self.raw.children_with_tokens(), _p: PhantomData }
    }

    pub fn first_child(&self) -> Option<SyntaxNode<L>> {
        self.raw.first_child().map(Self::from)
    }

    pub fn first_child_or_token(&self) -> Option<SyntaxElement<L>> {
        self.raw.first_child_or_token().map(SyntaxElement::from)
    }

    pub fn last_child(&self) -> Option<SyntaxNode<L>> {
        self.raw.last_child().map(Self::from)
    }

    pub fn last_child_or_token(&self) -> Option<SyntaxElement<L>> {
        self.raw.last_child_or_token().map(SyntaxElement::from)
    }

    pub fn next_sibling(&self) -> Option<SyntaxNode<L>> {
        self.raw.next_sibling().map(Self::from)
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<L>> {
        self.raw.next_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn prev_sibling(&self) -> Option<SyntaxNode<L>> {
        self.raw.prev_sibling().map(Self::from)
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<L>> {
        self.raw.prev_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn first_token(&self) -> Option<SyntaxToken<L>> {
        self.raw.first_token().map(SyntaxToken::from)
    }

    pub fn last_token(&self) -> Option<SyntaxToken<L>> {
        self.raw.last_token().map(SyntaxToken::from)
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode<L>> {
        self.raw.ancestors().map(SyntaxNode::from)
    }

    pub fn descendants(&self) -> impl Iterator<Item = SyntaxNode<L>> {
        self.raw.descendants().map(SyntaxNode::from)
    }

    pub fn descendants_with_tokens(&self) -> impl Iterator<Item = SyntaxElement<L>> {
        self.raw.descendants_with_tokens().map(SyntaxElement::from)
    }

    pub fn preorder(&self) -> impl Iterator<Item = WalkEvent<SyntaxNode<L>>> {
        self.raw.preorder().map(|event| event.map(SyntaxNode::from))
    }

    pub fn preorder_with_tokens(&self) -> impl Iterator<Item = WalkEvent<SyntaxElement<L>>> {
        self.raw.preorder_with_tokens().map(|event| event.map(SyntaxElement::from))
    }

    pub fn token_at_offset(&self, offset: TextUnit) -> TokenAtOffset<SyntaxToken<L>> {
        self.raw.token_at_offset(offset).map(SyntaxToken::from)
    }

    pub fn covering_element(&self, range: TextRange) -> SyntaxElement<L> {
        SyntaxElement::from(self.raw.covering_element(range))
    }
}

impl<L: Language> SyntaxToken<L> {
    pub fn replace_with(&self, new_token: GreenToken) -> GreenNode {
        self.raw.replace_with(new_token)
    }

    pub fn kind(&self) -> SyntaxKind<L> {
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

    pub fn parent(&self) -> SyntaxNode<L> {
        SyntaxNode::from(self.raw.parent())
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<L>> {
        self.raw.next_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<L>> {
        self.raw.prev_sibling_or_token().map(SyntaxElement::from)
    }

    pub fn next_token(&self) -> Option<SyntaxToken<L>> {
        self.raw.next_token().map(SyntaxToken::from)
    }

    pub fn prev_token(&self) -> Option<SyntaxToken<L>> {
        self.raw.prev_token().map(SyntaxToken::from)
    }
}

impl<L: Language> SyntaxElement<L> {
    pub fn text_range(&self) -> TextRange {
        match self {
            SyntaxElement::Node(it) => it.text_range(),
            SyntaxElement::Token(it) => it.text_range(),
        }
    }

    pub fn kind(&self) -> SyntaxKind<L> {
        match self {
            SyntaxElement::Node(it) => it.kind(),
            SyntaxElement::Token(it) => it.kind(),
        }
    }

    pub fn parent(&self) -> Option<SyntaxNode<L>> {
        match self {
            SyntaxElement::Node(it) => it.parent(),
            SyntaxElement::Token(it) => Some(it.parent()),
        }
    }

    pub fn as_node(&self) -> Option<&SyntaxNode<L>> {
        match self {
            SyntaxElement::Node(it) => Some(it),
            SyntaxElement::Token(_) => None,
        }
    }

    pub fn into_node(self) -> Option<SyntaxNode<L>> {
        match self {
            SyntaxElement::Node(it) => Some(it),
            SyntaxElement::Token(_) => None,
        }
    }

    pub fn as_token(&self) -> Option<&SyntaxToken<L>> {
        match self {
            SyntaxElement::Node(_) => None,
            SyntaxElement::Token(it) => Some(it),
        }
    }

    pub fn into_token(self) -> Option<SyntaxToken<L>> {
        match self {
            SyntaxElement::Node(_) => None,
            SyntaxElement::Token(it) => Some(it),
        }
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement<L>> {
        match self {
            SyntaxElement::Node(it) => it.next_sibling_or_token(),
            SyntaxElement::Token(it) => it.next_sibling_or_token(),
        }
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement<L>> {
        match self {
            SyntaxElement::Node(it) => it.prev_sibling_or_token(),
            SyntaxElement::Token(it) => it.prev_sibling_or_token(),
        }
    }
}

pub struct SyntaxNodeChildren<L: Language> {
    raw: cursor::SyntaxNodeChildren,
    _p: PhantomData<L>,
}

impl<L: Language> Iterator for SyntaxNodeChildren<L> {
    type Item = SyntaxNode<L>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(SyntaxNode::from)
    }
}

pub struct SyntaxElementChildren<L: Language> {
    raw: cursor::SyntaxElementChildren,
    _p: PhantomData<L>,
}

impl<L: Language> Iterator for SyntaxElementChildren<L> {
    type Item = SyntaxElement<L>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(SyntaxElement::from)
    }
}
