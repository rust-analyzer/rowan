use std::{
    cell::Cell,
    fmt,
    hash::{Hash, Hasher},
    iter, mem,
    ops::Range,
    ptr,
};

use countme::Count;

use crate::{
    green::{GreenElementRef, GreenNodeData, SyntaxKind},
    Direction, GreenNode, GreenToken, NodeOrToken, SyntaxText, TextRange, TextSize, TokenAtOffset,
    WalkEvent,
};

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
        if self.data().dec_rc() == 0 {
            unsafe { free(self.ptr) };
        }
    }
}

#[inline(never)]
unsafe fn free(mut data: ptr::NonNull<NodeData>) {
    loop {
        debug_assert_eq!(data.as_ref().rc.get(), 0);
        match data.as_ref().parent {
            Some(parent) => {
                if parent.as_ref().dec_rc() == 0 {
                    data = parent
                } else {
                    break;
                }
            }
            None => {
                GreenNode::from_raw(data.as_ref().green);
                Box::from_raw(data.as_ptr());
                break;
            }
        }
    }
}

// Identity semantics for hash & eq
impl PartialEq for SyntaxNode {
    #[inline]
    fn eq(&self, other: &SyntaxNode) -> bool {
        self.key() == other.key()
    }
}

impl Eq for SyntaxNode {}

impl Hash for SyntaxNode {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.key().hash(state);
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
        self.preorder_with_tokens()
            .filter_map(|event| match event {
                WalkEvent::Enter(NodeOrToken::Token(token)) => Some(token),
                _ => None,
            })
            .try_for_each(|it| fmt::Display::fmt(&it, f))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SyntaxToken {
    parent: SyntaxNode,
    gindex: u32,
    index: u32,
    offset: TextSize,
}

impl fmt::Display for SyntaxToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.text(), f)
    }
}

pub type SyntaxElement = NodeOrToken<SyntaxNode, SyntaxToken>;

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

impl fmt::Display for SyntaxElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeOrToken::Node(it) => fmt::Display::fmt(it, f),
            NodeOrToken::Token(it) => fmt::Display::fmt(it, f),
        }
    }
}

struct NodeData {
    rc: Cell<u32>,
    parent: Option<ptr::NonNull<NodeData>>,
    children: Box<[Cell<usize>]>,
    gindex: u32,
    index: u32,
    offset: TextSize,
    green: ptr::NonNull<GreenNodeData>,
    _c: Count<SyntaxNode>,
}

impl Drop for NodeData {
    fn drop(&mut self) {
        for child in self.children.iter() {
            let val = child.get();
            if val & 1 == 0 {
                unsafe { Box::from_raw(val as *mut NodeData) };
            }
        }
    }
}

impl NodeData {
    fn new(
        parent: Option<&NodeData>,
        gindex: u32,
        index: u32,
        offset: TextSize,
        green: &GreenNodeData,
    ) -> NodeData {
        let children = green
            .children()
            .enumerate()
            .filter_map(|(i, ch)| match ch {
                NodeOrToken::Node(_) => Some(Cell::new(1 | (i << 1))),
                NodeOrToken::Token(_) => None,
            })
            .collect();
        NodeData {
            rc: Cell::new(0),
            parent: parent.map(ptr::NonNull::from),
            children,
            gindex,
            index,
            offset,
            green: ptr::NonNull::from(green),
            _c: Count::new(),
        }
    }
    #[inline]
    fn inc_rc(&self) -> u32 {
        let rc = self.rc.get() + 1;
        self.rc.set(rc);
        rc
    }
    #[inline]
    fn dec_rc(&self) -> u32 {
        let rc = self.rc.get() - 1;
        self.rc.set(rc);
        rc
    }
    #[inline]
    fn green(&self) -> &GreenNodeData {
        unsafe { self.green.as_ref() }
    }
    #[inline]
    fn parent(&self) -> Option<&NodeData> {
        unsafe { self.parent.as_ref().map(|it| it.as_ref()) }
    }
    fn nth_child(&self, index: u32) -> Option<&NodeData> {
        let slot = self.children.get(index as usize)?;
        let val = slot.get();
        if val & 1 == 0 {
            return Some(unsafe { mem::transmute::<usize, &NodeData>(val) });
        }
        let gindex = val >> 1;
        let child = match self.green().nth_child(gindex) {
            Some((offset, NodeOrToken::Node(green))) => {
                NodeData::new(Some(self), gindex as u32, index, self.offset + offset, green)
            }
            _ => unreachable!(),
        };
        let ptr = Box::into_raw(Box::new(child));
        slot.set(ptr as usize);
        Some(unsafe { &*ptr })
    }
}

impl SyntaxNode {
    pub fn new_root(green: GreenNode) -> SyntaxNode {
        let green = GreenNode::into_raw(green);
        let data = NodeData::new(None, 0, 0, 0.into(), unsafe { green.as_ref() });
        data.inc_rc();
        let ptr = Box::into_raw(Box::new(data));
        unsafe { SyntaxNode::new(&*ptr) }
    }
    fn new(data: &NodeData) -> SyntaxNode {
        if data.inc_rc() == 1 {
            if let Some(parent) = data.parent() {
                parent.inc_rc();
            }
        }
        SyntaxNode { ptr: ptr::NonNull::from(data) }
    }

    fn key(&self) -> (ptr::NonNull<GreenNodeData>, TextSize) {
        (self.data().green, self.data().offset)
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
                let new_parent =
                    parent.green().replace_child(self.data().gindex as usize, replacement.into());
                parent.replace_with(new_parent)
            }
        }
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.green().kind()
    }

    #[inline]
    pub fn text_range(&self) -> TextRange {
        let offset = self.data().offset;
        let len = self.green().text_len();
        TextRange::at(offset, len)
    }

    #[inline]
    pub fn text(&self) -> SyntaxText {
        SyntaxText::new(self.clone())
    }

    #[inline]
    pub fn green(&self) -> &GreenNodeData {
        self.data().green()
    }

    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.data().parent().map(SyntaxNode::new)
    }

    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.clone()), SyntaxNode::parent)
    }

    #[inline]
    pub fn children(&self) -> SyntaxNodeChildren {
        SyntaxNodeChildren::new(self.clone())
    }

    #[inline]
    pub fn children_with_tokens(&self) -> SyntaxElementChildren {
        SyntaxElementChildren::new(self.clone())
    }

    fn nth_child(&self, index: u32) -> Option<SyntaxNode> {
        self.data().nth_child(index).map(SyntaxNode::new)
    }
    fn nth_token(&self, index: u32, gindex: u32, rel_offset: TextSize) -> SyntaxToken {
        SyntaxToken { parent: self.clone(), gindex, index, offset: self.data().offset + rel_offset }
    }
    pub fn first_child(&self) -> Option<SyntaxNode> {
        self.nth_child(0)
    }

    pub fn first_child_or_token(&self) -> Option<SyntaxElement> {
        let (offset, child) = self.green().nth_child(0)?;
        Some(SyntaxElement::new(child, self.clone(), 0, 0, offset))
    }

    pub fn last_child(&self) -> Option<SyntaxNode> {
        self.nth_child(self.data().children.len().checked_sub(1)? as u32)
    }

    pub fn last_child_or_token(&self) -> Option<SyntaxElement> {
        let gindex = self.green().children().len().checked_sub(1).unwrap();
        let (offset, child) = self.green().nth_child(gindex)?;
        let index = match child {
            NodeOrToken::Node(_) => self.data().children.len() as u32 - 1,
            NodeOrToken::Token(_) => self.data().children.len() as u32,
        };
        Some(SyntaxElement::new(child, self.clone(), gindex as u32, index, offset))
    }

    pub fn next_sibling(&self) -> Option<SyntaxNode> {
        self.data().parent().and_then(|p| p.nth_child(self.data().index + 1)).map(SyntaxNode::new)
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.parent()?;
        let gindex = self.data().gindex + 1;
        let (offset, child) = parent.green().nth_child(gindex as usize)?;
        Some(SyntaxElement::new(child, parent.clone(), gindex, self.data().index + 1, offset))
    }

    pub fn prev_sibling(&self) -> Option<SyntaxNode> {
        self.data()
            .parent()
            .and_then(|p| p.nth_child(self.data().index.checked_sub(1)?))
            .map(SyntaxNode::new)
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.parent()?;
        let gindex = self.data().gindex.checked_sub(1)?;
        let (offset, child) = parent.green().nth_child(gindex as usize)?;
        let index = match child {
            NodeOrToken::Node(_) => self.data().index - 1,
            NodeOrToken::Token(_) => self.data().index,
        };
        Some(SyntaxElement::new(child, parent.clone(), gindex, index, offset))
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
    pub fn preorder_with_tokens<'a>(&'a self) -> impl Iterator<Item = WalkEvent<SyntaxElement>> {
        let start: SyntaxElement = self.clone().into();
        iter::successors(Some(WalkEvent::Enter(start.clone())), move |pos| {
            let next = match pos {
                WalkEvent::Enter(el) => match el {
                    NodeOrToken::Node(node) => match node.first_child_or_token() {
                        Some(child) => WalkEvent::Enter(child),
                        None => WalkEvent::Leave(node.clone().into()),
                    },
                    NodeOrToken::Token(token) => WalkEvent::Leave(token.clone().into()),
                },
                WalkEvent::Leave(el) => {
                    if el == &start {
                        return None;
                    }
                    match el.next_sibling_or_token() {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(el.parent().unwrap().into()),
                    }
                }
            };
            Some(next)
        })
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
            let child_range = child.text_range();
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
        let start_offset = self.text_range().start();
        let (gindex, offset, _child) = self.green().child_at_range(range - start_offset)?;
        let gindex = gindex as u32;
        let index = self.data().children.binary_search_by_key(&gindex, |slot| {
            let val = slot.get();
            if val & 1 == 0 {
                unsafe { mem::transmute::<usize, &NodeData>(val).gindex }
            } else {
                (val as u32) >> 1
            }
        });
        match index {
            Ok(it) => self.nth_child(it as u32).map(SyntaxElement::from),
            Err(it) => Some(self.nth_token(it as u32, gindex, offset).into()),
        }
    }
}

impl SyntaxToken {
    pub fn replace_with(&self, replacement: GreenToken) -> GreenNode {
        assert_eq!(self.kind(), replacement.kind());
        let mut replacement = Some(replacement);
        let parent = self.parent();
        let me = self.gindex;

        let children = parent.green().children().enumerate().map(|(i, child)| {
            if i as u32 == me {
                replacement.take().unwrap().into()
            } else {
                child.cloned()
            }
        });
        let new_parent = GreenNode::new(parent.kind(), children);
        parent.replace_with(new_parent)
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.green().kind()
    }

    #[inline]
    pub fn text_range(&self) -> TextRange {
        TextRange::at(self.offset, self.green().text_len())
    }

    #[inline]
    pub fn text(&self) -> &str {
        self.green().text()
    }

    #[inline]
    pub fn green(&self) -> &GreenToken {
        self.parent.green().children().nth(self.gindex as usize).unwrap().as_token().unwrap()
    }

    #[inline]
    pub fn parent(&self) -> SyntaxNode {
        self.parent.clone()
    }

    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        self.parent().ancestors()
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.parent();
        let gindex = self.gindex + 1;
        let (offset, child) = parent.green().nth_child(gindex as usize)?;
        Some(SyntaxElement::new(child, parent.clone(), gindex, self.index, offset))
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.parent();
        let gindex = self.gindex.checked_sub(1)?;
        let (offset, child) = parent.green().nth_child(gindex as usize)?;
        let index = match child {
            NodeOrToken::Node(_) => self.index - 1,
            NodeOrToken::Token(_) => self.index,
        };
        Some(SyntaxElement::new(child, parent.clone(), gindex, index, offset))
    }

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
        match self.next_sibling_or_token() {
            Some(element) => element.first_token(),
            None => self
                .parent()
                .ancestors()
                .find_map(|it| it.next_sibling_or_token())
                .and_then(|element| element.first_token()),
        }
    }

    pub fn prev_token(&self) -> Option<SyntaxToken> {
        match self.prev_sibling_or_token() {
            Some(element) => element.last_token(),
            None => self
                .parent()
                .ancestors()
                .find_map(|it| it.prev_sibling_or_token())
                .and_then(|element| element.last_token()),
        }
    }
}

impl SyntaxElement {
    fn new(
        element: GreenElementRef<'_>,
        parent: SyntaxNode,
        gindex: u32,
        index: u32,
        rel_offset: TextSize,
    ) -> SyntaxElement {
        match element {
            NodeOrToken::Node(_) => parent.nth_child(index).unwrap().into(),
            NodeOrToken::Token(_) => parent.nth_token(index, gindex, rel_offset).into(),
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
            NodeOrToken::Token(it) => Some(it.parent()),
        }
    }

    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        match self {
            NodeOrToken::Node(it) => it.ancestors(),
            NodeOrToken::Token(it) => it.parent().ancestors(),
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
        assert!(self.text_range().start() <= offset && offset <= self.text_range().end());
        match self {
            NodeOrToken::Token(token) => TokenAtOffset::Single(token.clone()),
            NodeOrToken::Node(node) => node.token_at_offset(offset),
        }
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxNodeChildren {
    parent: SyntaxNode,
    index: Range<u32>,
}

impl SyntaxNodeChildren {
    fn new(parent: SyntaxNode) -> SyntaxNodeChildren {
        let index = 0..parent.data().children.len() as u32;
        SyntaxNodeChildren { parent, index }
    }
}

impl Iterator for SyntaxNodeChildren {
    type Item = SyntaxNode;
    fn next(&mut self) -> Option<Self::Item> {
        self.index.next().and_then(|idx| self.parent.nth_child(idx))
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxElementChildren {
    parent: SyntaxNode,
    gindex: Range<u32>,
    index: u32,
}

impl SyntaxElementChildren {
    fn new(parent: SyntaxNode) -> SyntaxElementChildren {
        let gindex = 0..parent.green().children().len() as u32;
        SyntaxElementChildren { parent, gindex, index: 0 }
    }
}

impl Iterator for SyntaxElementChildren {
    type Item = SyntaxElement;
    fn next(&mut self) -> Option<SyntaxElement> {
        self.gindex.next().and_then(|gindex| {
            let (offset, child) = self.parent.green().nth_child(gindex as usize)?;
            let index = self.index;
            if let NodeOrToken::Node(_) = child {
                self.index += 1;
            }
            Some(SyntaxElement::new(child, self.parent.clone(), gindex, index, offset))
        })
    }
}

pub struct Preorder {
    root: SyntaxNode,
    next: Option<WalkEvent<SyntaxNode>>,
    skip_subtree: bool,
}

impl Preorder {
    fn new(root: SyntaxNode) -> Preorder {
        let next = Some(WalkEvent::Enter(root.clone()));
        Preorder { root, next, skip_subtree: false }
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
                    if node == &self.root {
                        return None;
                    }
                    match node.next_sibling() {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(node.parent().unwrap()),
                    }
                }
            })
        });
        next
    }
}
