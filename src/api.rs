use std::{borrow::Cow, fmt, iter, marker::PhantomData, ops::Range};

use crate::{
    cursor, green::GreenTokenData, Direction, GreenNode, GreenNodeData, GreenToken, NodeOrToken,
    SyntaxKind, SyntaxText, TextRange, TextSize, TokenAtOffset, WalkEvent,
};

pub trait Language: Sized + Clone + Copy + fmt::Debug + Eq + Ord + std::hash::Hash {
    type Kind: fmt::Debug;

    fn kind_from_raw(raw: SyntaxKind) -> Self::Kind;
    fn kind_to_raw(kind: Self::Kind) -> SyntaxKind;
}

pub trait MutToken: Sized + Clone + Copy + fmt::Debug + Eq + Ord + std::hash::Hash {
    const IS_MUT: bool;

    #[inline(always)]
    fn is_mut() -> bool {
        Self::IS_MUT
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum ImMut {}

impl MutToken for ImMut {
    const IS_MUT: bool = false;
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Mut {}

impl MutToken for Mut {
    const IS_MUT: bool = true;
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AnySyntaxNode<L: Language, M: MutToken> {
    raw: cursor::SyntaxNode,
    _p: (PhantomData<L>, PhantomData<M>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AnySyntaxToken<L: Language, M: MutToken> {
    raw: cursor::SyntaxToken,
    _p: (PhantomData<L>, PhantomData<M>),
}

pub type AnySyntaxElement<L, M> = NodeOrToken<AnySyntaxNode<L, M>, AnySyntaxToken<L, M>>;

pub type SyntaxNode<L> = AnySyntaxNode<L, ImMut>;

pub type SyntaxToken<L> = AnySyntaxToken<L, ImMut>;

pub type SyntaxElement<L> = AnySyntaxElement<L, ImMut>;

pub type SyntaxNodeMut<L> = AnySyntaxNode<L, Mut>;

pub type SyntaxTokenMut<L> = AnySyntaxToken<L, Mut>;

pub type SyntaxElementMut<L> = AnySyntaxElement<L, Mut>;

impl<L: Language, M: MutToken> fmt::Debug for AnySyntaxNode<L, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            let mut level = 0;
            for event in self.preorder_with_tokens() {
                match event {
                    WalkEvent::Enter(element) => {
                        for _ in 0..level {
                            write!(f, "  ")?;
                        }
                        match element {
                            NodeOrToken::Node(node) => writeln!(f, "{:?}", node)?,
                            NodeOrToken::Token(token) => writeln!(f, "{:?}", token)?,
                        }
                        level += 1;
                    }
                    WalkEvent::Leave(_) => level -= 1,
                }
            }
            assert_eq!(level, 0);
            Ok(())
        } else {
            write!(f, "{:?}@{:?}", self.kind(), self.text_range())
        }
    }
}

impl<L: Language, M: MutToken> fmt::Display for AnySyntaxNode<L, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

impl<L: Language, M: MutToken> fmt::Debug for AnySyntaxToken<L, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{:?}", self.kind(), self.text_range())?;
        if self.text().len() < 25 {
            return write!(f, " {:?}", self.text());
        }
        let text = self.text();
        for idx in 21..25 {
            if text.is_char_boundary(idx) {
                let text = format!("{} ...", &text[..idx]);
                return write!(f, " {:?}", text);
            }
        }
        unreachable!()
    }
}

impl<L: Language, M: MutToken> fmt::Display for AnySyntaxToken<L, M> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

impl<L: Language, M: MutToken> From<AnySyntaxNode<L, M>> for AnySyntaxElement<L, M> {
    fn from(node: AnySyntaxNode<L, M>) -> AnySyntaxElement<L, M> {
        NodeOrToken::Node(node)
    }
}

impl<L: Language, M: MutToken> From<AnySyntaxToken<L, M>> for AnySyntaxElement<L, M> {
    fn from(token: AnySyntaxToken<L, M>) -> AnySyntaxElement<L, M> {
        NodeOrToken::Token(token)
    }
}

impl<L: Language, M: MutToken> AnySyntaxNode<L, M> {
    pub fn new_root(green: GreenNode) -> SyntaxNode<L> {
        AnySyntaxNode::from_raw_unchecked(cursor::SyntaxNode::new_root(green))
    }
    /// Returns a green tree, equal to the green tree this node
    /// belongs two, except with this node substitute. The complexity
    /// of operation is proportional to the depth of the tree
    pub fn replace_with(&self, replacement: GreenNode) -> GreenNode {
        self.raw.replace_with(replacement)
    }

    pub fn kind(&self) -> L::Kind {
        L::kind_from_raw(self.raw.kind())
    }

    pub fn text_range(&self) -> TextRange {
        self.raw.text_range()
    }

    pub fn index(&self) -> usize {
        self.raw.index()
    }

    pub fn text(&self) -> SyntaxText {
        self.raw.text()
    }

    pub fn green(&self) -> Cow<'_, GreenNodeData> {
        self.raw.green()
    }

    pub fn parent(&self) -> Option<AnySyntaxNode<L, M>> {
        self.raw.parent().map(Self::from_raw_unchecked)
    }

    pub fn ancestors(&self) -> impl Iterator<Item = AnySyntaxNode<L, M>> {
        self.raw.ancestors().map(AnySyntaxNode::from_raw_unchecked)
    }

    pub fn children(&self) -> SyntaxNodeChildren<L, M> {
        SyntaxNodeChildren { raw: self.raw.children(), _p: (PhantomData, PhantomData) }
    }

    pub fn children_with_tokens(&self) -> SyntaxElementChildren<L, M> {
        SyntaxElementChildren {
            raw: self.raw.children_with_tokens(),
            _p: (PhantomData, PhantomData),
        }
    }

    pub fn first_child(&self) -> Option<AnySyntaxNode<L, M>> {
        self.raw.first_child().map(Self::from_raw_unchecked)
    }
    pub fn last_child(&self) -> Option<AnySyntaxNode<L, M>> {
        self.raw.last_child().map(Self::from_raw_unchecked)
    }

    pub fn first_child_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        self.raw.first_child_or_token().map(NodeOrToken::from_raw_unchecked)
    }
    pub fn last_child_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        self.raw.last_child_or_token().map(NodeOrToken::from_raw_unchecked)
    }

    pub fn next_sibling(&self) -> Option<AnySyntaxNode<L, M>> {
        self.raw.next_sibling().map(Self::from_raw_unchecked)
    }
    pub fn prev_sibling(&self) -> Option<AnySyntaxNode<L, M>> {
        self.raw.prev_sibling().map(Self::from_raw_unchecked)
    }

    pub fn next_sibling_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        self.raw.next_sibling_or_token().map(NodeOrToken::from_raw_unchecked)
    }
    pub fn prev_sibling_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        self.raw.prev_sibling_or_token().map(NodeOrToken::from_raw_unchecked)
    }

    /// Return the leftmost token in the subtree of this node.
    pub fn first_token(&self) -> Option<AnySyntaxToken<L, M>> {
        self.raw.first_token().map(AnySyntaxToken::from_raw_unchecked)
    }
    /// Return the rightmost token in the subtree of this node.
    pub fn last_token(&self) -> Option<AnySyntaxToken<L, M>> {
        self.raw.last_token().map(AnySyntaxToken::from_raw_unchecked)
    }

    pub fn siblings(&self, direction: Direction) -> impl Iterator<Item = AnySyntaxNode<L, M>> {
        self.raw.siblings(direction).map(AnySyntaxNode::from_raw_unchecked)
    }

    pub fn siblings_with_tokens(
        &self,
        direction: Direction,
    ) -> impl Iterator<Item = AnySyntaxElement<L, M>> {
        self.raw.siblings_with_tokens(direction).map(AnySyntaxElement::from_raw_unchecked)
    }

    pub fn descendants(&self) -> impl Iterator<Item = AnySyntaxNode<L, M>> {
        self.raw.descendants().map(AnySyntaxNode::from_raw_unchecked)
    }

    pub fn descendants_with_tokens(&self) -> impl Iterator<Item = AnySyntaxElement<L, M>> {
        self.raw.descendants_with_tokens().map(NodeOrToken::from_raw_unchecked)
    }

    /// Traverse the subtree rooted at the current node (including the current
    /// node) in preorder, excluding tokens.
    pub fn preorder(&self) -> Preorder<L, M> {
        Preorder { raw: self.raw.preorder(), _p: (PhantomData, PhantomData) }
    }

    /// Traverse the subtree rooted at the current node (including the current
    /// node) in preorder, including tokens.
    pub fn preorder_with_tokens(&self) -> PreorderWithTokens<L, M> {
        PreorderWithTokens { raw: self.raw.preorder_with_tokens(), _p: (PhantomData, PhantomData) }
    }

    /// Find a token in the subtree corresponding to this node, which covers the offset.
    /// Precondition: offset must be withing node's range.
    pub fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<AnySyntaxToken<L, M>> {
        self.raw.token_at_offset(offset).map(AnySyntaxToken::from_raw_unchecked)
    }

    /// Return the deepest node or token in the current subtree that fully
    /// contains the range. If the range is empty and is contained in two leaf
    /// nodes, either one can be returned. Precondition: range must be contained
    /// withing the current node
    pub fn covering_element(&self, range: TextRange) -> AnySyntaxElement<L, M> {
        NodeOrToken::from_raw_unchecked(self.raw.covering_element(range))
    }

    /// Finds a [`SyntaxElement`] which intersects with a given `range`. If
    /// there are several intersecting elements, any one can be returned.
    ///
    /// The method uses binary search internally, so it's complexity is
    /// `O(log(N))` where `N = self.children_with_tokens().count()`.
    pub fn child_or_token_at_range(&self, range: TextRange) -> Option<AnySyntaxElement<L, M>> {
        self.raw.child_or_token_at_range(range).map(AnySyntaxElement::from_raw_unchecked)
    }

    /// Returns an independent copy of the subtree rooted at this node.
    ///
    /// The parent of the returned node will be `None`, the start offset will be
    /// zero, but, otherwise, it'll be equivalent to the source node.
    pub fn clone_subtree(&self) -> AnySyntaxNode<L, M> {
        AnySyntaxNode::from_raw_unchecked(self.raw.clone_subtree())
    }
}

impl<L: Language> SyntaxNode<L> {
    pub fn clone_for_update(&self) -> AnySyntaxNode<L, Mut> {
        AnySyntaxNode::from_raw_unchecked(self.raw.clone_for_update())
    }
}

impl<L: Language> SyntaxNodeMut<L> {
    pub fn splice_children(&self, to_delete: Range<usize>, to_insert: Vec<SyntaxElementMut<L>>) {
        let to_insert = to_insert.into_iter().map(cursor::SyntaxElement::from).collect::<Vec<_>>();
        self.raw.splice_children(to_delete, to_insert)
    }

    pub fn detach(&self) {
        self.raw.detach()
    }
}

impl<L: Language, M: MutToken> AnySyntaxToken<L, M> {
    /// Returns a green tree, equal to the green tree this token
    /// belongs two, except with this token substitute. The complexity
    /// of operation is proportional to the depth of the tree
    pub fn replace_with(&self, new_token: GreenToken) -> GreenNode {
        self.raw.replace_with(new_token)
    }

    pub fn kind(&self) -> L::Kind {
        L::kind_from_raw(self.raw.kind())
    }

    pub fn text_range(&self) -> TextRange {
        self.raw.text_range()
    }

    pub fn index(&self) -> usize {
        self.raw.index()
    }

    pub fn text(&self) -> &str {
        self.raw.text()
    }

    pub fn green(&self) -> &GreenTokenData {
        self.raw.green()
    }

    pub fn parent(&self) -> Option<AnySyntaxNode<L, M>> {
        self.raw.parent().map(AnySyntaxNode::from_raw_unchecked)
    }

    pub fn ancestors(&self) -> impl Iterator<Item = AnySyntaxNode<L, M>> {
        self.raw.ancestors().map(AnySyntaxNode::from_raw_unchecked)
    }

    pub fn next_sibling_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        self.raw.next_sibling_or_token().map(NodeOrToken::from_raw_unchecked)
    }
    pub fn prev_sibling_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        self.raw.prev_sibling_or_token().map(NodeOrToken::from_raw_unchecked)
    }

    pub fn siblings_with_tokens(
        &self,
        direction: Direction,
    ) -> impl Iterator<Item = AnySyntaxElement<L, M>> {
        self.raw.siblings_with_tokens(direction).map(AnySyntaxElement::from_raw_unchecked)
    }

    /// Next token in the tree (i.e, not necessary a sibling).
    pub fn next_token(&self) -> Option<AnySyntaxToken<L, M>> {
        self.raw.next_token().map(AnySyntaxToken::from_raw_unchecked)
    }
    /// Previous token in the tree (i.e, not necessary a sibling).
    pub fn prev_token(&self) -> Option<AnySyntaxToken<L, M>> {
        self.raw.prev_token().map(AnySyntaxToken::from_raw_unchecked)
    }

    pub fn detach(&self) {
        self.raw.detach()
    }
}

impl<L: Language, M: MutToken> AnySyntaxElement<L, M> {
    pub fn text_range(&self) -> TextRange {
        match self {
            NodeOrToken::Node(it) => it.text_range(),
            NodeOrToken::Token(it) => it.text_range(),
        }
    }

    pub fn index(&self) -> usize {
        match self {
            NodeOrToken::Node(it) => it.index(),
            NodeOrToken::Token(it) => it.index(),
        }
    }

    pub fn kind(&self) -> L::Kind {
        match self {
            NodeOrToken::Node(it) => it.kind(),
            NodeOrToken::Token(it) => it.kind(),
        }
    }

    pub fn parent(&self) -> Option<AnySyntaxNode<L, M>> {
        match self {
            NodeOrToken::Node(it) => it.parent(),
            NodeOrToken::Token(it) => it.parent(),
        }
    }

    pub fn ancestors(&self) -> impl Iterator<Item = AnySyntaxNode<L, M>> {
        let first = match self {
            NodeOrToken::Node(it) => Some(it.clone()),
            NodeOrToken::Token(it) => it.parent(),
        };
        iter::successors(first, AnySyntaxNode::parent)
    }

    pub fn next_sibling_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        match self {
            NodeOrToken::Node(it) => it.next_sibling_or_token(),
            NodeOrToken::Token(it) => it.next_sibling_or_token(),
        }
    }
    pub fn prev_sibling_or_token(&self) -> Option<AnySyntaxElement<L, M>> {
        match self {
            NodeOrToken::Node(it) => it.prev_sibling_or_token(),
            NodeOrToken::Token(it) => it.prev_sibling_or_token(),
        }
    }
}

impl<L: Language> SyntaxElementMut<L> {
    pub fn detach(&self) {
        match self {
            NodeOrToken::Node(it) => it.detach(),
            NodeOrToken::Token(it) => it.detach(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxNodeChildren<L: Language, M: MutToken> {
    raw: cursor::SyntaxNodeChildren,
    _p: (PhantomData<L>, PhantomData<M>),
}

impl<L: Language, M: MutToken> Iterator for SyntaxNodeChildren<L, M> {
    type Item = AnySyntaxNode<L, M>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(AnySyntaxNode::from_raw_unchecked)
    }
}

#[derive(Debug, Clone)]
pub struct SyntaxElementChildren<L: Language, M: MutToken> {
    raw: cursor::SyntaxElementChildren,
    _p: (PhantomData<L>, PhantomData<M>),
}

impl<L: Language, M: MutToken> Iterator for SyntaxElementChildren<L, M> {
    type Item = AnySyntaxElement<L, M>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(AnySyntaxElement::from_raw_unchecked)
    }
}

pub struct Preorder<L: Language, M: MutToken> {
    raw: cursor::Preorder,
    _p: (PhantomData<L>, PhantomData<M>),
}

impl<L: Language, M: MutToken> Preorder<L, M> {
    pub fn skip_subtree(&mut self) {
        self.raw.skip_subtree()
    }
}

impl<L: Language, M: MutToken> Iterator for Preorder<L, M> {
    type Item = WalkEvent<AnySyntaxNode<L, M>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(|it| it.map(AnySyntaxNode::from_raw_unchecked))
    }
}

pub struct PreorderWithTokens<L: Language, M: MutToken> {
    raw: cursor::PreorderWithTokens,
    _p: (PhantomData<L>, PhantomData<M>),
}

impl<L: Language, M: MutToken> PreorderWithTokens<L, M> {
    pub fn skip_subtree(&mut self) {
        self.raw.skip_subtree()
    }
}

impl<L: Language, M: MutToken> Iterator for PreorderWithTokens<L, M> {
    type Item = WalkEvent<AnySyntaxElement<L, M>>;
    fn next(&mut self) -> Option<Self::Item> {
        self.raw.next().map(|it| it.map(AnySyntaxElement::from_raw_unchecked))
    }
}

impl<L: Language, M: MutToken> AnySyntaxNode<L, M> {
    fn from_raw_unchecked(raw: cursor::SyntaxNode) -> AnySyntaxNode<L, M> {
        AnySyntaxNode { raw, _p: (PhantomData, PhantomData) }
    }

    fn from_raw(raw: cursor::SyntaxNode) -> AnySyntaxNode<L, M> {
        assert_eq!(M::is_mut(), raw.is_mut());
        Self::from_raw_unchecked(raw)
    }
}

impl<L: Language, M: MutToken> From<cursor::SyntaxNode> for AnySyntaxNode<L, M> {
    fn from(raw: cursor::SyntaxNode) -> AnySyntaxNode<L, M> {
        Self::from_raw(raw)
    }
}

impl<L: Language, M: MutToken> From<AnySyntaxNode<L, M>> for cursor::SyntaxNode {
    fn from(node: AnySyntaxNode<L, M>) -> cursor::SyntaxNode {
        node.raw
    }
}

impl<L: Language, M: MutToken> AnySyntaxToken<L, M> {
    fn from_raw_unchecked(raw: cursor::SyntaxToken) -> AnySyntaxToken<L, M> {
        AnySyntaxToken { raw, _p: (PhantomData, PhantomData) }
    }

    fn from_raw(raw: cursor::SyntaxToken) -> AnySyntaxToken<L, M> {
        assert_eq!(M::IS_MUT, raw.is_mut());
        Self::from_raw_unchecked(raw)
    }
}

impl<L: Language, M: MutToken> From<cursor::SyntaxToken> for AnySyntaxToken<L, M> {
    fn from(raw: cursor::SyntaxToken) -> AnySyntaxToken<L, M> {
        Self::from_raw(raw)
    }
}

impl<L: Language, M: MutToken> From<AnySyntaxToken<L, M>> for cursor::SyntaxToken {
    fn from(token: AnySyntaxToken<L, M>) -> cursor::SyntaxToken {
        token.raw
    }
}

impl<L: Language, M: MutToken> AnySyntaxElement<L, M> {
    fn from_raw_unchecked(raw: cursor::SyntaxElement) -> AnySyntaxElement<L, M> {
        match raw {
            NodeOrToken::Node(it) => NodeOrToken::Node(AnySyntaxNode::from_raw_unchecked(it)),
            NodeOrToken::Token(it) => NodeOrToken::Token(AnySyntaxToken::from_raw_unchecked(it)),
        }
    }

    fn from_raw(raw: cursor::SyntaxElement) -> AnySyntaxElement<L, M> {
        assert_eq!(M::is_mut(), raw.is_mut());
        Self::from_raw_unchecked(raw)
    }
}

impl<L: Language, M: MutToken> From<cursor::SyntaxElement> for AnySyntaxElement<L, M> {
    fn from(raw: cursor::SyntaxElement) -> AnySyntaxElement<L, M> {
        Self::from_raw(raw)
    }
}

impl<L: Language, M: MutToken> From<AnySyntaxElement<L, M>> for cursor::SyntaxElement {
    fn from(element: AnySyntaxElement<L, M>) -> cursor::SyntaxElement {
        match element {
            NodeOrToken::Node(it) => NodeOrToken::Node(it.into()),
            NodeOrToken::Token(it) => NodeOrToken::Token(it.into()),
        }
    }
}
