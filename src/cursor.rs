//! Implementation of the cursors -- API for convenient access to syntax trees.
//!
//! Functional programmers will recognize that this module implements a zipper
//! for a purely functional (green) tree.
//!
//! A cursor node (`SyntaxNode`) points to a `GreenNode` and a parent
//! `SyntaxNode`. This allows cursor to provide iteration over both ancestors
//! and descendants, as well as a cheep access to absolute offset of the node in
//! file.

// Implementation notes:
//
// The implementation is utterly and horribly unsafe. This whole module is an
// unsafety boundary. It is believed that the API here is, in principle, sound,
// but the implementation might have bugs.
//
// The core type is `NodeData` -- a heap-allocated reference counted object,
// which points to a green node or a green token, and to the parent `NodeData`.
// Publicly-exposed `SyntaxNode` and `SyntaxToken` own a reference to
// `NodeData`.
//
// `NodeData`s are transient, and are created and destroyed during tree
// traversals. In general, only currently referenced nodes and their ancestors
// are alive at any given moment.
//
// More specifically, `NodeData`'s ref count is equal to the number of
// outstanding `SyntaxNode` and `SyntaxToken` plus the number of children with
// non-zero ref counts. For example, if the user has only a single `SyntaxNode`
// pointing somewhere in the middle of the tree, then all `NodeData` on the path
// from that point towards the root have ref count equal to one.
//
// A root `NodeData` owns its green node and is responsible for freeing it.

use std::{
    cell::Cell,
    fmt,
    hash::{Hash, Hasher},
    iter,
    mem::ManuallyDrop,
    ptr, slice,
};

use countme::Count;

use crate::{
    green::{GreenChild, GreenElementRef, GreenNodeData, GreenTokenData, SyntaxKind},
    Direction, GreenNode, GreenToken, NodeOrToken, SyntaxText, TextRange, TextSize, TokenAtOffset,
    WalkEvent,
};

enum Green {
    Node { ptr: ptr::NonNull<GreenNodeData> },
    Token { ptr: ptr::NonNull<GreenTokenData> },
}

struct _SyntaxElement;

struct NodeData {
    _c: Count<_SyntaxElement>,

    rc: Cell<u32>,
    parent: Option<ptr::NonNull<NodeData>>,
    index: u32,
    green: Green,
    offset: TextSize,
}

pub type SyntaxElement = NodeOrToken<SyntaxNode, SyntaxToken>;

pub struct SyntaxNode {
    ptr: ptr::NonNull<NodeData>,
}

impl Clone for SyntaxNode {
    #[inline]
    fn clone(&self) -> Self {
        self.data().inc_rc();
        SyntaxNode { ptr: self.ptr }
    }
}

impl Drop for SyntaxNode {
    #[inline]
    fn drop(&mut self) {
        if self.data().dec_rc() {
            unsafe { free(self.ptr) }
        }
    }
}

#[derive(Debug)]
pub struct SyntaxToken {
    ptr: ptr::NonNull<NodeData>,
}

impl Clone for SyntaxToken {
    #[inline]
    fn clone(&self) -> Self {
        self.data().inc_rc();
        SyntaxToken { ptr: self.ptr }
    }
}

impl Drop for SyntaxToken {
    #[inline]
    fn drop(&mut self) {
        if self.data().dec_rc() {
            unsafe { free(self.ptr) }
        }
    }
}

#[inline(never)]
unsafe fn free(mut data: ptr::NonNull<NodeData>) {
    loop {
        debug_assert_eq!(data.as_ref().rc.get(), 0);
        let node = Box::from_raw(data.as_ptr());
        match node.parent {
            Some(parent) => {
                debug_assert!(parent.as_ref().rc.get() > 0);
                if parent.as_ref().dec_rc() {
                    data = parent;
                } else {
                    break;
                }
            }
            None => {
                if let Green::Node { ptr } = &node.green {
                    let _ = GreenNode::from_raw(*ptr);
                } else {
                    unreachable!("a token cannot be a root");
                }
                break;
            }
        }
    }
}

impl NodeData {
    #[inline]
    fn new(
        parent: Option<ptr::NonNull<NodeData>>,
        index: u32,
        offset: TextSize,
        green: Green,
    ) -> ptr::NonNull<NodeData> {
        let res = NodeData { _c: Count::new(), rc: Cell::new(1), parent, index, green, offset };
        unsafe { ptr::NonNull::new_unchecked(Box::into_raw(Box::new(res))) }
    }

    #[inline]
    fn inc_rc(&self) {
        let rc = match self.rc.get().checked_add(1) {
            Some(it) => it,
            None => std::process::abort(),
        };
        self.rc.set(rc)
    }

    #[inline]
    fn dec_rc(&self) -> bool {
        let rc = self.rc.get() - 1;
        self.rc.set(rc);
        rc == 0
    }

    #[inline]
    fn key(&self) -> (ptr::NonNull<()>, TextSize) {
        let ptr = match &self.green {
            Green::Node { ptr } => ptr.cast(),
            Green::Token { ptr } => ptr.cast(),
        };
        (ptr, self.offset())
    }

    #[inline]
    fn parent_node(&self) -> Option<SyntaxNode> {
        let mut parent = self.parent()?;
        while !matches!(parent.green, Green::Node { .. }) {
            parent = parent.parent()?;
        }
        parent.inc_rc();
        Some(SyntaxNode { ptr: ptr::NonNull::from(parent) })
    }

    #[inline]
    fn parent_token(&self) -> Option<SyntaxToken> {
        let parent = self.parent()?;
        match parent.green {
            Green::Token { .. } => {
                parent.inc_rc();
                Some(SyntaxToken { ptr: ptr::NonNull::from(parent) })
            }
            Green::Node { .. } => None,
        }
    }

    #[inline]
    fn parent(&self) -> Option<&NodeData> {
        self.parent.map(|it| unsafe { &*it.as_ptr() })
    }

    #[inline]
    fn green(&self) -> GreenElementRef<'_> {
        match &self.green {
            Green::Node { ptr } => GreenElementRef::Node(unsafe { &*ptr.as_ptr() }),
            Green::Token { ptr } => GreenElementRef::Token(unsafe { ptr.as_ref() }),
        }
    }
    #[inline]
    fn green_siblings(&self) -> slice::Iter<'_, GreenChild> {
        match &self.parent().map(|it| &it.green) {
            Some(Green::Node { ptr }) => unsafe { &*ptr.as_ptr() }.children().raw,
            Some(Green::Token { .. }) => [].iter(),
            None => [].iter(),
        }
    }
    #[inline]
    fn index(&self) -> u32 {
        self.index
    }

    #[inline]
    fn offset(&self) -> TextSize {
        self.offset
    }

    #[inline]
    fn text_range(&self) -> TextRange {
        let offset = self.offset();
        let len = self.green().text_len();
        TextRange::at(offset, len)
    }

    #[inline]
    fn kind(&self) -> SyntaxKind {
        self.green().kind()
    }

    fn next_sibling(&self) -> Option<SyntaxNode> {
        let mut siblings = self.green_siblings().enumerate();
        let index = self.index() as usize;

        siblings.nth(index);
        siblings.find_map(|(index, child)| {
            child.as_ref().into_node().and_then(|green| {
                let parent = self.parent_node()?;
                let offset = parent.offset() + child.rel_offset();
                Some(SyntaxNode::new_child(green, parent, index as u32, offset))
            })
        })
    }
    fn prev_sibling(&self) -> Option<SyntaxNode> {
        let mut rev_siblings = self.green_siblings().enumerate().rev();
        let index = rev_siblings.len().checked_sub(self.index() as usize + 1)?;

        rev_siblings.nth(index);
        rev_siblings.find_map(|(index, child)| {
            child.as_ref().into_node().and_then(|green| {
                let parent = self.parent_node()?;
                let offset = parent.offset() + child.rel_offset();
                Some(SyntaxNode::new_child(green, parent, index as u32, offset))
            })
        })
    }

    fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        let mut siblings = self.green_siblings().enumerate();
        let index = self.index() as usize + 1;

        siblings.nth(index).and_then(|(index, child)| {
            let parent = self.parent_node()?;
            let offset = parent.offset() + child.rel_offset();
            Some(SyntaxElement::new(child.as_ref(), parent, index as u32, offset))
        })
    }
    fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        let mut siblings = self.green_siblings().enumerate();
        let index = self.index().checked_sub(1)? as usize;

        siblings.nth(index).and_then(|(index, child)| {
            let parent = self.parent_node()?;
            let offset = parent.offset() + child.rel_offset();
            Some(SyntaxElement::new(child.as_ref(), parent, index as u32, offset))
        })
    }
}

impl SyntaxNode {
    pub fn new_root(green: GreenNode) -> SyntaxNode {
        let green = GreenNode::into_raw(green);
        let green = Green::Node { ptr: green };
        SyntaxNode { ptr: NodeData::new(None, 0, 0.into(), green) }
    }

    fn new_child(
        green: &GreenNodeData,
        parent: SyntaxNode,
        index: u32,
        offset: TextSize,
    ) -> SyntaxNode {
        let parent = ManuallyDrop::new(parent);
        let green = Green::Node { ptr: green.into() };
        SyntaxNode { ptr: NodeData::new(Some(parent.ptr), index, offset, green) }
    }

    pub fn clone_subtree(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green().to_owned())
    }

    #[inline]
    fn data(&self) -> &NodeData {
        unsafe { self.ptr.as_ref() }
    }

    pub fn replace_with(&self, replacement: GreenNode) -> GreenNode {
        assert_eq!(self.kind(), replacement.kind());
        match &self.parent() {
            None => replacement,
            Some(parent) => {
                let new_parent = parent
                    .green_ref()
                    .replace_child(self.data().index() as usize, replacement.into());
                parent.replace_with(new_parent)
            }
        }
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.data().kind()
    }

    #[inline]
    fn offset(&self) -> TextSize {
        self.data().offset()
    }

    #[inline]
    pub fn text_range(&self) -> TextRange {
        self.data().text_range()
    }

    #[inline]
    pub fn index(&self) -> usize {
        self.data().index() as usize
    }

    #[inline]
    pub fn text(&self) -> SyntaxText {
        SyntaxText::new(self.clone())
    }

    #[inline]
    pub fn green(&self) -> &GreenNodeData {
        self.green_ref()
    }
    #[inline]
    fn green_ref(&self) -> &GreenNodeData {
        self.data().green().into_node().unwrap()
    }

    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.data().parent_node()
    }

    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.clone()), SyntaxNode::parent)
    }

    #[inline]
    pub fn tree_top(&self) -> SyntaxNode {
        self.ancestors().last().unwrap()
    }

    #[inline]
    pub fn children(&self) -> SyntaxNodeChildren {
        SyntaxNodeChildren::new(self.clone())
    }

    #[inline]
    pub fn children_with_tokens(&self) -> SyntaxElementChildren {
        SyntaxElementChildren::new(self.clone())
    }

    pub fn first_child(&self) -> Option<SyntaxNode> {
        self.green_ref().children().raw.enumerate().find_map(|(index, child)| {
            child.as_ref().into_node().map(|green| {
                SyntaxNode::new_child(
                    green,
                    self.clone(),
                    index as u32,
                    self.offset() + child.rel_offset(),
                )
            })
        })
    }
    pub fn last_child(&self) -> Option<SyntaxNode> {
        self.green_ref().children().raw.enumerate().rev().find_map(|(index, child)| {
            child.as_ref().into_node().map(|green| {
                SyntaxNode::new_child(
                    green,
                    self.clone(),
                    index as u32,
                    self.offset() + child.rel_offset(),
                )
            })
        })
    }

    pub fn first_child_or_token(&self) -> Option<SyntaxElement> {
        self.green_ref().children().raw.next().map(|child| {
            SyntaxElement::new(child.as_ref(), self.clone(), 0, self.offset() + child.rel_offset())
        })
    }
    pub fn last_child_or_token(&self) -> Option<SyntaxElement> {
        self.green_ref().children().raw.enumerate().next_back().map(|(index, child)| {
            SyntaxElement::new(
                child.as_ref(),
                self.clone(),
                index as u32,
                self.offset() + child.rel_offset(),
            )
        })
    }

    pub fn next_sibling(&self) -> Option<SyntaxNode> {
        self.data().next_sibling()
    }
    pub fn prev_sibling(&self) -> Option<SyntaxNode> {
        self.data().prev_sibling()
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.data().next_sibling_or_token()
    }
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.data().prev_sibling_or_token()
    }

    pub fn first_token(&self) -> Option<SyntaxToken> {
        self.first_child_or_token()?.first_token()
    }
    pub fn last_token(&self) -> Option<SyntaxToken> {
        self.last_child_or_token()?.last_token()
    }

    #[inline]
    pub fn siblings(&self, direction: Direction) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.clone()), move |node| match direction {
            Direction::Next => node.next_sibling(),
            Direction::Prev => node.prev_sibling(),
        })
    }

    #[inline]
    pub fn siblings_with_tokens(
        &self,
        direction: Direction,
    ) -> impl Iterator<Item = SyntaxElement> {
        let me: SyntaxElement = self.clone().into();
        iter::successors(Some(me), move |el| match direction {
            Direction::Next => el.next_sibling_or_token(),
            Direction::Prev => el.prev_sibling_or_token(),
        })
    }

    #[inline]
    pub fn descendants(&self) -> impl Iterator<Item = SyntaxNode> {
        self.preorder().filter_map(|event| match event {
            WalkEvent::Enter(node) => Some(node),
            WalkEvent::Leave(_) => None,
        })
    }

    #[inline]
    pub fn descendants_with_tokens(&self) -> impl Iterator<Item = SyntaxElement> {
        self.preorder_with_tokens().filter_map(|event| match event {
            WalkEvent::Enter(it) => Some(it),
            WalkEvent::Leave(_) => None,
        })
    }

    #[inline]
    pub fn preorder(&self) -> Preorder {
        Preorder::new(self.clone())
    }

    #[inline]
    pub fn preorder_with_tokens(&self) -> PreorderWithTokens {
        PreorderWithTokens::new(self.clone())
    }

    pub fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken> {
        // TODO: this could be faster if we first drill-down to node, and only
        // then switch to token search. We should also replace explicit
        // recursion with a loop.
        let range = self.text_range();
        assert!(
            range.start() <= offset && offset <= range.end(),
            "Bad offset: range {:?} offset {:?}",
            range,
            offset
        );
        if range.is_empty() {
            return TokenAtOffset::None;
        }

        let mut children = self.children_with_tokens().filter(|child| {
            let child_range = child.text_range_including_trivia();
            !child_range.is_empty()
                && (child_range.start() <= offset && offset <= child_range.end())
        });

        let left = children.next().unwrap();
        let right = children.next();
        assert!(children.next().is_none());

        if let Some(right) = right {
            match (left.token_at_offset(offset), right.token_at_offset(offset)) {
                (TokenAtOffset::Single(left), TokenAtOffset::Single(right)) => {
                    TokenAtOffset::Between(left, right)
                }
                _ => unreachable!(),
            }
        } else {
            left.token_at_offset(offset)
        }
    }

    pub fn covering_element(&self, range: TextRange) -> SyntaxElement {
        let mut res: SyntaxElement = self.clone().into();
        loop {
            assert!(
                res.text_range().contains_range(range),
                "Bad range: node range {:?}, range {:?}",
                res.text_range(),
                range,
            );
            res = match &res {
                NodeOrToken::Token(_) => return res,
                NodeOrToken::Node(node) => match node.child_or_token_at_range(range) {
                    Some(it) => it,
                    None => return res,
                },
            };
        }
    }

    pub fn child_or_token_at_range(&self, range: TextRange) -> Option<SyntaxElement> {
        let rel_range = range - self.offset();
        let (index, rel_offset, green) = self.green_ref().child_at_range(rel_range)?;
        let child =
            SyntaxElement::new(green, self.clone(), index as u32, self.offset() + rel_offset);
        match child {
            NodeOrToken::Token(token) if !token.text_range().contains_range(range) => token
                .leading_trivia()
                .chain(token.trailing_trivia())
                .find(|it| it.text_range().contains_range(range))
                .map(SyntaxElement::from),
            child => Some(child),
        }
    }
}

impl SyntaxToken {
    fn new(
        green: &GreenTokenData,
        parent: SyntaxNode,
        index: u32,
        offset: TextSize,
    ) -> SyntaxToken {
        let parent = ManuallyDrop::new(parent);
        let green = Green::Token { ptr: green.into() };
        SyntaxToken { ptr: NodeData::new(Some(parent.ptr), index, offset, green) }
    }

    fn new_trivia(
        green: &GreenTokenData,
        parent: SyntaxToken,
        index: u32,
        offset: TextSize,
    ) -> SyntaxToken {
        let parent = ManuallyDrop::new(parent);
        let green = Green::Token { ptr: green.into() };
        SyntaxToken { ptr: NodeData::new(Some(parent.ptr), index, offset, green) }
    }

    #[inline]
    fn data(&self) -> &NodeData {
        unsafe { self.ptr.as_ref() }
    }

    pub fn replace_with(&self, replacement: GreenToken) -> GreenNode {
        assert_eq!(self.kind(), replacement.kind());
        if let Some(owner) = self.data().parent_token() {
            let index = self.data().index() as usize;
            let green = owner.green();
            let mut leading = green.leading_trivia().to_vec();
            let mut trailing = green.trailing_trivia().to_vec();
            if self.is_leading_trivia(&owner) {
                leading[index] = replacement;
            } else {
                trailing[index] = replacement;
            }
            let new_owner = GreenToken::with_trivia(green.kind(), green.text(), leading, trailing);
            return owner.replace_with(new_owner);
        }
        let parent = self.parent().unwrap();
        let me: u32 = self.data().index();

        let new_parent = parent.green_ref().replace_child(me as usize, replacement.into());
        parent.replace_with(new_parent)
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.data().kind()
    }

    #[inline]
    pub fn text_range(&self) -> TextRange {
        self.green().text_range() + self.data().offset()
    }

    #[inline]
    pub fn text_range_including_trivia(&self) -> TextRange {
        TextRange::at(self.data().offset(), self.green().text_len_including_trivia())
    }

    #[inline]
    pub fn index(&self) -> usize {
        self.data().index() as usize
    }

    #[inline]
    pub fn text(&self) -> &str {
        match self.data().green().as_token() {
            Some(it) => it.text(),
            None => {
                debug_assert!(
                    false,
                    "corrupted tree: a node thinks it is a token: {:?}",
                    self.data().green().as_node().unwrap().to_string()
                );
                ""
            }
        }
    }

    pub fn text_including_trivia(&self) -> String {
        self.with_trivia().map(|token| token.text().to_owned()).collect()
    }

    #[inline]
    pub fn green(&self) -> &GreenTokenData {
        self.data().green().into_token().unwrap()
    }

    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.data().parent_node()
    }

    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        std::iter::successors(self.parent(), SyntaxNode::parent)
    }

    #[inline]
    pub fn tree_top(&self) -> SyntaxNode {
        self.ancestors().last().unwrap()
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.data().next_sibling_or_token()
    }
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.data().prev_sibling_or_token()
    }

    #[inline]
    pub fn siblings_with_tokens(
        &self,
        direction: Direction,
    ) -> impl Iterator<Item = SyntaxElement> {
        let me: SyntaxElement = self.clone().into();
        iter::successors(Some(me), move |el| match direction {
            Direction::Next => el.next_sibling_or_token(),
            Direction::Prev => el.prev_sibling_or_token(),
        })
    }

    pub fn next_token(&self) -> Option<SyntaxToken> {
        if let Some(parent) = self.data().parent_token() {
            let index = self.index() + 1;
            return if self.is_leading_trivia(&parent) {
                parent.leading_trivia().nth(index).or(Some(parent))
            } else {
                parent.trailing_trivia().nth(index).or_else(|| {
                    parent.next_non_trivia_token().map(SyntaxToken::first_token_including_trivia)
                })
            };
        }
        self.trailing_trivia()
            .next()
            .or_else(|| self.next_non_trivia_token().map(SyntaxToken::first_token_including_trivia))
    }
    pub fn prev_token(&self) -> Option<SyntaxToken> {
        if let Some(parent) = self.data().parent_token() {
            let index = self.index().checked_sub(1);
            return if self.is_leading_trivia(&parent) {
                index.and_then(|it| parent.leading_trivia().nth(it)).or_else(|| {
                    parent.prev_non_trivia_token().map(SyntaxToken::last_token_including_trivia)
                })
            } else {
                index.and_then(|it| parent.trailing_trivia().nth(it)).or(Some(parent))
            };
        }
        self.leading_trivia()
            .next_back()
            .or_else(|| self.prev_non_trivia_token().map(SyntaxToken::last_token_including_trivia))
    }

    fn next_non_trivia_token(&self) -> Option<SyntaxToken> {
        match self.next_sibling_or_token() {
            Some(element) => element.first_token(),
            None => self
                .ancestors()
                .find_map(|it| it.next_sibling_or_token())
                .and_then(|element| element.first_token()),
        }
    }
    fn prev_non_trivia_token(&self) -> Option<SyntaxToken> {
        match self.prev_sibling_or_token() {
            Some(element) => element.last_token(),
            None => self
                .ancestors()
                .find_map(|it| it.prev_sibling_or_token())
                .and_then(|element| element.last_token()),
        }
    }

    fn first_token_including_trivia(self) -> SyntaxToken {
        let first = self.leading_trivia().next();
        first.unwrap_or(self)
    }
    fn last_token_including_trivia(self) -> SyntaxToken {
        let last = self.trailing_trivia().next_back();
        last.unwrap_or(self)
    }

    fn is_leading_trivia(&self, parent: &SyntaxToken) -> bool {
        self.data().offset() < parent.text_range().start()
    }

    pub(crate) fn with_trivia(&self) -> impl Iterator<Item = SyntaxToken> {
        self.leading_trivia().chain(iter::once(self.clone())).chain(self.trailing_trivia())
    }

    fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken> {
        let mut tokens = self.with_trivia().filter(|token| {
            let range = token.text_range();
            !range.is_empty() && range.start() <= offset && offset <= range.end()
        });
        let Some(left) = tokens.next() else {
            return TokenAtOffset::None;
        };
        match tokens.next() {
            Some(right) => TokenAtOffset::Between(left, right),
            None => TokenAtOffset::Single(left),
        }
    }

    pub fn leading_trivia(
        &self,
    ) -> impl DoubleEndedIterator<Item = SyntaxToken> + ExactSizeIterator {
        self.trivia(true)
    }

    pub fn trailing_trivia(
        &self,
    ) -> impl DoubleEndedIterator<Item = SyntaxToken> + ExactSizeIterator {
        self.trivia(false)
    }

    fn trivia(
        &self,
        leading: bool,
    ) -> impl DoubleEndedIterator<Item = SyntaxToken> + ExactSizeIterator {
        let green = self.green();
        let (start, len) = if leading {
            (self.data().offset(), green.leading_trivia().len())
        } else {
            (self.text_range().end(), green.trailing_trivia().len())
        };
        let token = self.clone();
        (0..len).map(move |index| {
            let trivia = if leading {
                token.green().leading_trivia()
            } else {
                token.green().trailing_trivia()
            };
            let offset = start + trivia[..index].iter().map(|it| it.text_len()).sum::<TextSize>();
            SyntaxToken::new_trivia(&trivia[index], token.clone(), index as u32, offset)
        })
    }
}

impl SyntaxElement {
    fn new(
        element: GreenElementRef<'_>,
        parent: SyntaxNode,
        index: u32,
        offset: TextSize,
    ) -> SyntaxElement {
        match element {
            NodeOrToken::Node(node) => {
                SyntaxNode::new_child(node, parent, index as u32, offset).into()
            }
            NodeOrToken::Token(token) => {
                SyntaxToken::new(token, parent, index as u32, offset).into()
            }
        }
    }

    #[inline]
    pub fn text_range(&self) -> TextRange {
        match self {
            NodeOrToken::Node(it) => it.text_range(),
            NodeOrToken::Token(it) => it.text_range(),
        }
    }

    #[inline]
    fn text_range_including_trivia(&self) -> TextRange {
        match self {
            NodeOrToken::Node(it) => it.text_range(),
            NodeOrToken::Token(it) => it.text_range_including_trivia(),
        }
    }

    #[inline]
    pub fn index(&self) -> usize {
        match self {
            NodeOrToken::Node(it) => it.index(),
            NodeOrToken::Token(it) => it.index(),
        }
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        match self {
            NodeOrToken::Node(it) => it.kind(),
            NodeOrToken::Token(it) => it.kind(),
        }
    }

    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode> {
        match self {
            NodeOrToken::Node(it) => it.parent(),
            NodeOrToken::Token(it) => it.parent(),
        }
    }

    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        let first = match self {
            NodeOrToken::Node(it) => Some(it.clone()),
            NodeOrToken::Token(it) => it.parent(),
        };
        iter::successors(first, SyntaxNode::parent)
    }

    #[inline]
    pub fn tree_top(&self) -> SyntaxNode {
        match self {
            NodeOrToken::Node(it) => it.tree_top(),
            NodeOrToken::Token(it) => it.tree_top(),
        }
    }

    pub fn first_token(&self) -> Option<SyntaxToken> {
        match self {
            NodeOrToken::Node(it) => it.first_token(),
            NodeOrToken::Token(it) => Some(it.clone()),
        }
    }
    pub fn last_token(&self) -> Option<SyntaxToken> {
        match self {
            NodeOrToken::Node(it) => it.last_token(),
            NodeOrToken::Token(it) => Some(it.clone()),
        }
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        match self {
            NodeOrToken::Node(it) => it.next_sibling_or_token(),
            NodeOrToken::Token(it) => it.next_sibling_or_token(),
        }
    }
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        match self {
            NodeOrToken::Node(it) => it.prev_sibling_or_token(),
            NodeOrToken::Token(it) => it.prev_sibling_or_token(),
        }
    }

    fn token_at_offset(&self, offset: TextSize) -> TokenAtOffset<SyntaxToken> {
        let range = self.text_range_including_trivia();
        assert!(range.start() <= offset && offset <= range.end());
        match self {
            NodeOrToken::Token(token) => token.token_at_offset(offset),
            NodeOrToken::Node(node) => node.token_at_offset(offset),
        }
    }
}

// region: impls

// Identity semantics for hash & eq
impl PartialEq for SyntaxNode {
    #[inline]
    fn eq(&self, other: &SyntaxNode) -> bool {
        self.data().key() == other.data().key()
    }
}

impl Eq for SyntaxNode {}

impl Hash for SyntaxNode {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data().key().hash(state);
    }
}

impl fmt::Debug for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SyntaxNode")
            .field("kind", &self.kind())
            .field("text_range", &self.text_range())
            .finish()
    }
}

impl fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.text().fmt(f)
    }
}

// Identity semantics for hash & eq
impl PartialEq for SyntaxToken {
    #[inline]
    fn eq(&self, other: &SyntaxToken) -> bool {
        self.data().key() == other.data().key()
    }
}

impl Eq for SyntaxToken {}

impl Hash for SyntaxToken {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data().key().hash(state);
    }
}

impl fmt::Display for SyntaxToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.text_including_trivia(), f)
    }
}

impl From<SyntaxNode> for SyntaxElement {
    #[inline]
    fn from(node: SyntaxNode) -> SyntaxElement {
        NodeOrToken::Node(node)
    }
}

impl From<SyntaxToken> for SyntaxElement {
    #[inline]
    fn from(token: SyntaxToken) -> SyntaxElement {
        NodeOrToken::Token(token)
    }
}

// endregion

// region: iterators

#[derive(Clone, Debug)]
pub struct SyntaxNodeChildren {
    next: Option<SyntaxNode>,
}

impl SyntaxNodeChildren {
    fn new(parent: SyntaxNode) -> SyntaxNodeChildren {
        SyntaxNodeChildren { next: parent.first_child() }
    }
}

impl Iterator for SyntaxNodeChildren {
    type Item = SyntaxNode;
    fn next(&mut self) -> Option<SyntaxNode> {
        self.next.take().map(|next| {
            self.next = next.next_sibling();
            next
        })
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxElementChildren {
    next: Option<SyntaxElement>,
}

impl SyntaxElementChildren {
    fn new(parent: SyntaxNode) -> SyntaxElementChildren {
        SyntaxElementChildren { next: parent.first_child_or_token() }
    }
}

impl Iterator for SyntaxElementChildren {
    type Item = SyntaxElement;
    fn next(&mut self) -> Option<SyntaxElement> {
        self.next.take().map(|next| {
            self.next = next.next_sibling_or_token();
            next
        })
    }
}

#[derive(Debug, Clone)]
pub struct Preorder {
    start: SyntaxNode,
    next: Option<WalkEvent<SyntaxNode>>,
    skip_subtree: bool,
}

impl Preorder {
    fn new(start: SyntaxNode) -> Preorder {
        let next = Some(WalkEvent::Enter(start.clone()));
        Preorder { start, next, skip_subtree: false }
    }

    pub fn skip_subtree(&mut self) {
        self.skip_subtree = true;
    }

    #[cold]
    fn do_skip(&mut self) {
        self.next = self.next.take().map(|next| match next {
            WalkEvent::Enter(first_child) => WalkEvent::Leave(first_child.parent().unwrap()),
            WalkEvent::Leave(parent) => WalkEvent::Leave(parent),
        })
    }
}

impl Iterator for Preorder {
    type Item = WalkEvent<SyntaxNode>;

    fn next(&mut self) -> Option<WalkEvent<SyntaxNode>> {
        if self.skip_subtree {
            self.do_skip();
            self.skip_subtree = false;
        }
        let next = self.next.take();
        self.next = next.as_ref().and_then(|next| {
            Some(match next {
                WalkEvent::Enter(node) => match node.first_child() {
                    Some(child) => WalkEvent::Enter(child),
                    None => WalkEvent::Leave(node.clone()),
                },
                WalkEvent::Leave(node) => {
                    if node == &self.start {
                        return None;
                    }
                    match node.next_sibling() {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(node.parent()?),
                    }
                }
            })
        });
        next
    }
}

#[derive(Debug, Clone)]
pub struct PreorderWithTokens {
    start: SyntaxElement,
    next: Option<WalkEvent<SyntaxElement>>,
    skip_subtree: bool,
}

impl PreorderWithTokens {
    fn new(start: SyntaxNode) -> PreorderWithTokens {
        let next = Some(WalkEvent::Enter(start.clone().into()));
        PreorderWithTokens { start: start.into(), next, skip_subtree: false }
    }

    pub fn skip_subtree(&mut self) {
        self.skip_subtree = true;
    }

    #[cold]
    fn do_skip(&mut self) {
        self.next = self.next.take().map(|next| match next {
            WalkEvent::Enter(first_child) => WalkEvent::Leave(first_child.parent().unwrap().into()),
            WalkEvent::Leave(parent) => WalkEvent::Leave(parent),
        })
    }
}

impl Iterator for PreorderWithTokens {
    type Item = WalkEvent<SyntaxElement>;

    fn next(&mut self) -> Option<WalkEvent<SyntaxElement>> {
        if self.skip_subtree {
            self.do_skip();
            self.skip_subtree = false;
        }
        let next = self.next.take();
        self.next = next.as_ref().and_then(|next| {
            Some(match next {
                WalkEvent::Enter(el) => match el {
                    NodeOrToken::Node(node) => match node.first_child_or_token() {
                        Some(child) => WalkEvent::Enter(child),
                        None => WalkEvent::Leave(node.clone().into()),
                    },
                    NodeOrToken::Token(token) => WalkEvent::Leave(token.clone().into()),
                },
                WalkEvent::Leave(el) if el == &self.start => return None,
                WalkEvent::Leave(el) => match el.next_sibling_or_token() {
                    Some(sibling) => WalkEvent::Enter(sibling),
                    None => WalkEvent::Leave(el.parent()?.into()),
                },
            })
        });
        next
    }
}
// endregion
