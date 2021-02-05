use std::{
    cell::Cell,
    fmt,
    hash::{Hash, Hasher},
    iter, mem, ptr, slice,
};

use countme::Count;

use crate::{
    green::{GreenChild, GreenElementRef, GreenNodeData, SyntaxKind},
    Direction, GreenNode, GreenToken, NodeOrToken, SyntaxText, TextRange, TextSize, TokenAtOffset,
    WalkEvent,
};

pub struct SyntaxNode {
    ptr: ptr::NonNull<NodeData>,
}

impl Clone for SyntaxNode {
    #[inline]
    fn clone(&self) -> Self {
        let rc = match self.data().rc.get().checked_add(1) {
            Some(it) => it,
            None => std::process::abort(),
        };
        self.data().rc.set(rc);
        SyntaxNode { ptr: self.ptr }
    }
}

impl Drop for SyntaxNode {
    #[inline]
    fn drop(&mut self) {
        let rc = self.data().rc.get() - 1;
        self.data().rc.set(rc);
        if rc == 0 {
            unsafe { free(Box::from_raw(self.ptr.as_ptr())) }
        }
    }
}

#[inline(never)]
fn free(mut data: Box<NodeData>) {
    loop {
        debug_assert_eq!(data.rc.get(), 0);
        match data.parent.take() {
            Some(parent) => {
                let parent = mem::ManuallyDrop::new(parent);
                let rc = parent.data().rc.get() - 1;
                parent.data().rc.set(rc);
                if rc == 0 {
                    data = unsafe { Box::from_raw(parent.ptr.as_ptr()) }
                } else {
                    break;
                }
            }
            None => unsafe {
                GreenNode::from_raw(data.green);
                break;
            },
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

struct NodeData {
    rc: Cell<u32>,
    parent: Option<SyntaxNode>,
    index: u32,
    offset: TextSize,
    green: ptr::NonNull<GreenNodeData>,
    _c: Count<SyntaxNode>,
}

impl SyntaxNode {
    fn new(data: NodeData) -> SyntaxNode {
        SyntaxNode { ptr: unsafe { ptr::NonNull::new_unchecked(Box::into_raw(Box::new(data))) } }
    }

    pub fn new_root(green: GreenNode) -> SyntaxNode {
        let data = NodeData {
            rc: Cell::new(1),
            parent: None,
            index: 0,
            offset: 0.into(),
            green: GreenNode::into_raw(green),
            _c: Count::new(),
        };
        SyntaxNode::new(data)
    }

    // Technically, unsafe, but private so that's OK.
    // Safety: `green` must be a descendent of `parent.green()`
    fn new_child(
        green: &GreenNode,
        parent: SyntaxNode,
        index: u32,
        offset: TextSize,
    ) -> SyntaxNode {
        let data = NodeData {
            rc: Cell::new(1),
            parent: Some(parent),
            index,
            offset,
            green: {
                let green: &GreenNodeData = &*green;
                ptr::NonNull::from(green)
            },
            _c: Count::new(),
        };
        SyntaxNode::new(data)
    }

    fn key(&self) -> (ptr::NonNull<GreenNodeData>, TextSize) {
        (self.data().green, self.offset())
    }

    #[inline]
    fn data(&self) -> &NodeData {
        unsafe { self.ptr.as_ref() }
    }

    pub fn replace_with(&self, replacement: GreenNode) -> GreenNode {
        assert_eq!(self.kind(), replacement.kind());
        match &self.data().parent {
            None => replacement,
            Some(parent) => {
                let new_parent =
                    parent.green().replace_child(self.data().index as usize, replacement.into());
                parent.replace_with(new_parent)
            }
        }
    }

    #[inline]
    pub fn kind(&self) -> SyntaxKind {
        self.green().kind()
    }

    #[inline]
    fn offset(&self) -> TextSize {
        self.data().offset
    }

    #[inline]
    pub fn text_range(&self) -> TextRange {
        let offset = self.offset();
        let len = self.green().text_len();
        TextRange::at(offset, len)
    }

    #[inline]
    pub fn text(&self) -> SyntaxText {
        SyntaxText::new(self.clone())
    }

    #[inline]
    pub fn green(&self) -> &GreenNodeData {
        unsafe { self.data().green.as_ref() }
    }

    #[inline]
    pub fn parent(&self) -> Option<SyntaxNode> {
        self.data().parent.clone()
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

    pub fn first_child(&self) -> Option<SyntaxNode> {
        self.green().children().raw.enumerate().find_map(|(index, child)| {
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
        self.green().children().raw.enumerate().rev().find_map(|(index, child)| {
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
        self.green().children().raw.next().map(|child| {
            SyntaxElement::new(child.as_ref(), self.clone(), 0, self.offset() + child.rel_offset())
        })
    }
    pub fn last_child_or_token(&self) -> Option<SyntaxElement> {
        self.green().children().raw.enumerate().next_back().map(|(index, child)| {
            SyntaxElement::new(
                child.as_ref(),
                self.clone(),
                index as u32,
                self.offset() + child.rel_offset(),
            )
        })
    }

    pub fn next_sibling(&self) -> Option<SyntaxNode> {
        let parent = self.data().parent.as_ref()?;
        let mut children = parent.green().children().raw.enumerate();
        children.nth(self.data().index as usize);
        children.find_map(|(index, child)| {
            child.as_ref().into_node().map(|green| {
                SyntaxNode::new_child(
                    green,
                    parent.clone(),
                    index as u32,
                    parent.offset() + child.rel_offset(),
                )
            })
        })
    }
    pub fn prev_sibling(&self) -> Option<SyntaxNode> {
        let parent = self.data().parent.as_ref()?;
        let mut children = parent.green().children().raw.enumerate().rev();
        children.nth(parent.green().children().len() - self.data().index as usize);
        children.find_map(|(index, child)| {
            child.as_ref().into_node().map(|green| {
                SyntaxNode::new_child(
                    green,
                    parent.clone(),
                    index as u32,
                    parent.offset() + child.rel_offset(),
                )
            })
        })
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.data().parent.as_ref()?;
        parent.green().children().raw.enumerate().nth(self.data().index as usize + 1).map(
            |(index, child)| {
                SyntaxElement::new(
                    child.as_ref(),
                    parent.clone(),
                    index as u32,
                    parent.offset() + child.rel_offset(),
                )
            },
        )
    }
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        let parent = self.data().parent.as_ref()?;
        parent
            .green()
            .children()
            .raw
            .enumerate()
            .nth(self.data().index.checked_sub(1)? as usize)
            .map(|(index, child)| {
                SyntaxElement::new(
                    child.as_ref(),
                    parent.clone(),
                    index as u32,
                    parent.offset() + child.rel_offset(),
                )
            })
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
        let rel_range = range - self.offset();
        self.green().child_at_range(rel_range).map(|(index, rel_offset, green)| {
            SyntaxElement::new(green, self.clone(), index as u32, self.offset() + rel_offset)
        })
    }
}

impl SyntaxToken {
    fn new(parent: SyntaxNode, index: u32, offset: TextSize) -> SyntaxToken {
        SyntaxToken { parent, index, offset }
    }

    pub fn replace_with(&self, replacement: GreenToken) -> GreenNode {
        assert_eq!(self.kind(), replacement.kind());
        let mut replacement = Some(replacement);
        let parent = self.parent();
        let me = self.index;

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
        self.parent.green().children().nth(self.index as usize).unwrap().as_token().unwrap()
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
        self.parent.green().children().raw.enumerate().nth((self.index + 1) as usize).map(
            |(index, child)| {
                SyntaxElement::new(
                    child.as_ref(),
                    self.parent(),
                    index as u32,
                    self.parent.offset() + child.rel_offset(),
                )
            },
        )
    }
    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        self.parent.green().children().raw.enumerate().nth(self.index.checked_sub(1)? as usize).map(
            |(index, child)| {
                SyntaxElement::new(
                    child.as_ref(),
                    self.parent(),
                    index as u32,
                    self.parent.offset() + child.rel_offset(),
                )
            },
        )
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
        index: u32,
        offset: TextSize,
    ) -> SyntaxElement {
        match element {
            NodeOrToken::Node(node) => {
                SyntaxNode::new_child(node, parent, index as u32, offset).into()
            }
            NodeOrToken::Token(_) => SyntaxToken::new(parent, index as u32, offset).into(),
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
struct Iter {
    parent: SyntaxNode,
    green: iter::Enumerate<slice::Iter<'static, GreenChild>>,
}

impl Iter {
    fn new(parent: SyntaxNode) -> Iter {
        let green = parent.green().children().raw.enumerate();
        // Dirty lifetime laundering: the memory for the children is
        // indirectly owned by parent.
        let green = unsafe { mem::transmute(green) };
        Iter { parent, green }
    }
    fn next_node(&mut self) -> Option<SyntaxNode> {
        let parent = &self.parent;
        self.green.find_map(|(index, child)| {
            child.as_ref().into_node().map(|green| {
                SyntaxNode::new_child(
                    green,
                    parent.clone(),
                    index as u32,
                    parent.offset() + child.rel_offset(),
                )
            })
        })
    }
    fn next_element(&mut self) -> Option<SyntaxElement> {
        let parent = &self.parent;
        self.green.next().map(|(index, child)| {
            SyntaxElement::new(
                child.as_ref(),
                parent.clone(),
                index as u32,
                parent.offset() + child.rel_offset(),
            )
        })
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxNodeChildren(Iter);

impl SyntaxNodeChildren {
    fn new(parent: SyntaxNode) -> SyntaxNodeChildren {
        SyntaxNodeChildren(Iter::new(parent))
    }
}

impl Iterator for SyntaxNodeChildren {
    type Item = SyntaxNode;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_node()
    }
}

#[derive(Clone, Debug)]
pub struct SyntaxElementChildren(Iter);

impl SyntaxElementChildren {
    fn new(parent: SyntaxNode) -> SyntaxElementChildren {
        SyntaxElementChildren(Iter::new(parent))
    }
}

impl Iterator for SyntaxElementChildren {
    type Item = SyntaxElement;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next_element()
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
