use std::{
    cell::RefCell,
    fmt,
    hash::{Hash, Hasher},
    iter, mem,
    ops::Bound,
    ptr,
    rc::Rc,
};

use crate::{
    ArcGreenNode, Direction, GreenChildren, GreenElement, GreenElementRef, GreenNode, GreenToken,
    NodeOrToken, SmolStr, SyntaxText, TextRange, TextUnit, TokenAtOffset, WalkEvent,
};

/// SyntaxKind is a type tag for each token or node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxKind(pub u16);

#[derive(Debug, Clone)]
pub struct SyntaxNode(Rc<NodeData>);

impl Drop for SyntaxNode {
    fn drop(&mut self) {
        NodeData::delete(&mut self.0)
    }
}

impl PartialEq for SyntaxNode {
    fn eq(&self, other: &SyntaxNode) -> bool {
        ptr::eq(self.green(), other.green())
            && self.text_range().start() == other.text_range().start()
    }
}

impl Eq for SyntaxNode {}

impl Hash for SyntaxNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        ptr::hash(self.green(), state);
        self.text_range().start().hash(state);
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
    fam_index: u16,
    offset: TextUnit,
}

impl fmt::Display for SyntaxToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.text(), f)
    }
}

pub(crate) type SyntaxElement = NodeOrToken<SyntaxNode, SyntaxToken>;

impl From<SyntaxNode> for SyntaxElement {
    fn from(node: SyntaxNode) -> SyntaxElement {
        NodeOrToken::Node(node)
    }
}

impl From<SyntaxToken> for SyntaxElement {
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

#[derive(Debug)]
enum Kind {
    Root(ArcGreenNode),
    Child { parent: SyntaxNode, fam_index: u16, offset: TextUnit },
    Free { next_free: Option<Rc<NodeData>> },
}

impl Kind {
    fn as_child(&self) -> Option<(&SyntaxNode, u16, TextUnit)> {
        match self {
            Kind::Child { parent, fam_index, offset } => Some((parent, *fam_index, *offset)),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct NodeData {
    kind: Kind,
    green: ptr::NonNull<GreenNode>,
}

struct FreeList {
    first_free: Option<Rc<NodeData>>,
    len: usize,
}

const FREE_LIST_LEN: usize = 128;

impl FreeList {
    fn new() -> FreeList {
        let mut res = FreeList { first_free: None, len: 0 };
        for _ in 0..FREE_LIST_LEN {
            res.try_push(&mut Rc::new(NodeData {
                kind: Kind::Free { next_free: None },
                green: ptr::NonNull::dangling(),
            }))
        }
        res
    }

    fn with<T, F: FnOnce(&mut FreeList) -> T>(f: F) -> T {
        thread_local! {
            static INSTANCE: RefCell<FreeList> = RefCell::new(FreeList::new());
        }
        INSTANCE.with(|it| f(&mut *it.borrow_mut()))
    }

    fn pop(&mut self) -> Option<Rc<NodeData>> {
        let mut node = self.first_free.take()?;
        self.len -= 1;
        {
            let node = Rc::get_mut(&mut node).unwrap();
            self.first_free = match &mut node.kind {
                Kind::Free { next_free } => next_free.take(),
                _ => unreachable!(),
            }
        }
        Some(node)
    }

    fn try_push(&mut self, node: &mut Rc<NodeData>) {
        if self.len >= FREE_LIST_LEN {
            return;
        }
        Rc::get_mut(node).unwrap().kind = Kind::Free { next_free: self.first_free.take() };
        self.first_free = Some(Rc::clone(node));
        self.len += 1;
    }
}

impl NodeData {
    fn new(kind: Kind, green: ptr::NonNull<GreenNode>) -> Rc<NodeData> {
        let mut node = FreeList::with(|it| it.pop()).unwrap_or_else(|| {
            Rc::new(NodeData {
                kind: Kind::Free { next_free: None },
                green: ptr::NonNull::dangling(),
            })
        });

        {
            let node = Rc::get_mut(&mut node).unwrap();
            node.kind = kind;
            node.green = green;
        }
        node
    }

    fn delete(this: &mut Rc<NodeData>) {
        if let Some(this_mut) = Rc::get_mut(this) {
            // NB: this might drop SyntaxNodes
            this_mut.kind = Kind::Free { next_free: None };
            FreeList::with(|it| it.try_push(this))
        }
    }
}

impl SyntaxNode {
    fn new(data: Rc<NodeData>) -> SyntaxNode {
        SyntaxNode(data)
    }

    pub fn new_root(green: ArcGreenNode) -> SyntaxNode {
        let data = NodeData::new(Kind::Root(green), ptr::NonNull::dangling());
        let mut ret = SyntaxNode::new(data);
        let green: ptr::NonNull<GreenNode> = match &ret.0.kind {
            Kind::Root(green) => (&**green).into(),
            _ => unreachable!(),
        };
        Rc::get_mut(&mut ret.0).unwrap().green = green;
        ret
    }

    /// # Safety
    ///
    /// - `green` must be a descendent of `parent.green()`
    /// - `fam_index` must be valid in `green`
    unsafe fn new_child(
        green: &GreenNode,
        parent: SyntaxNode,
        fam_index: u16,
        offset: TextUnit,
    ) -> SyntaxNode {
        let data = NodeData::new(Kind::Child { parent, fam_index, offset }, green.into());
        SyntaxNode::new(data)
    }

    /// Returns a green tree, equal to the green tree this node
    /// belongs two, except with this node substitute. The complexity
    /// of operation is proportional to the depth of the tree
    pub fn replace_with(&self, replacement: ArcGreenNode) -> ArcGreenNode {
        assert_eq!(self.kind(), replacement.kind());
        match self.0.kind.as_child() {
            None => replacement,
            Some((parent, me, _offset)) => {
                let mut replacement = Some(replacement);
                let children: Vec<GreenElement> = parent
                    .green()
                    .children()
                    .map(|child| -> GreenElement {
                        if unsafe { parent.green().child_idx(child) } == me {
                            replacement.take().unwrap().into()
                        } else {
                            child.to_owned()
                        }
                    })
                    .collect();
                assert!(replacement.is_none());
                let new_parent = GreenNode::new(parent.kind(), children);
                parent.replace_with(new_parent)
            }
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.green().kind()
    }

    pub fn text_range(&self) -> TextRange {
        let offset = match self.0.kind.as_child() {
            Some((_, _, it)) => it,
            _ => 0.into(),
        };
        TextRange::offset_len(offset, self.green().text_len())
    }

    pub fn text(&self) -> SyntaxText {
        SyntaxText::new(self.clone())
    }

    pub fn green(&self) -> &GreenNode {
        unsafe { self.0.green.as_ref() }
    }

    pub fn parent(&self) -> Option<SyntaxNode> {
        match &self.0.kind {
            Kind::Root(_) => None,
            Kind::Child { parent, .. } => Some(parent.clone()),
            Kind::Free { .. } => unreachable!(),
        }
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.clone()), SyntaxNode::parent)
    }

    pub fn children(&self) -> SyntaxNodeChildren {
        SyntaxNodeChildren::new(self.clone())
    }

    pub fn children_with_tokens(&self) -> SyntaxElementChildren {
        SyntaxElementChildren::new(self.clone())
    }

    #[inline]
    pub fn first_child(&self) -> Option<SyntaxNode> {
        unsafe {
            let (node, offset) =
                filter_nodes(self.green().children_from_inclusive(0, self.text_range().start()))
                    .next()?;
            Some(SyntaxNode::new_child(node, self.clone(), self.green().child_idx(node), offset))
        }
    }

    pub fn first_child_or_token(&self) -> Option<SyntaxElement> {
        unsafe {
            let (element, offset) =
                self.green().children_from_inclusive(0, self.text_range().start()).next()?;
            Some(SyntaxElement::new(element, self.clone(), self.green().child_idx(element), offset))
        }
    }

    #[inline]
    pub fn last_child(&self) -> Option<SyntaxNode> {
        unsafe {
            let (node, offset) = filter_nodes(
                self.green().children_to_exclusive(self.green().fam_end(), self.text_range().end()),
            )
            .next()?;

            Some(SyntaxNode::new_child(node, self.clone(), self.green().child_idx(node), offset))
        }
    }

    pub fn last_child_or_token(&self) -> Option<SyntaxElement> {
        unsafe {
            let (element, offset) = self
                .green()
                .children_to_exclusive(self.green().fam_end(), self.text_range().end())
                .next()?;
            Some(SyntaxElement::new(element, self.clone(), self.green().child_idx(element), offset))
        }
    }

    pub fn next_sibling(&self) -> Option<SyntaxNode> {
        let (parent, fam_index, _) = self.0.kind.as_child()?;

        unsafe {
            let (node, offset) = filter_nodes(
                parent.green().children_from_exclusive(fam_index, self.text_range().end()),
            )
            .next()?;

            Some(SyntaxNode::new_child(
                node,
                parent.clone(),
                parent.green().child_idx(node),
                offset,
            ))
        }
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        let (parent, fam_index, _) = self.0.kind.as_child()?;

        unsafe {
            let (element, offset) = parent
                .green()
                .children_from_exclusive(fam_index, self.text_range().end())
                .next()?;

            Some(SyntaxElement::new(
                element,
                parent.clone(),
                parent.green().child_idx(element),
                offset,
            ))
        }
    }

    pub fn prev_sibling(&self) -> Option<SyntaxNode> {
        let (parent, fam_index, _) = self.0.kind.as_child()?;

        unsafe {
            let (node, offset) = filter_nodes(
                parent.green().children_to_exclusive(fam_index, self.text_range().start()),
            )
            .next()?;

            Some(SyntaxNode::new_child(
                node,
                parent.clone(),
                parent.green().child_idx(node),
                offset,
            ))
        }
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        let (parent, fam_index, _) = self.0.kind.as_child()?;

        unsafe {
            let (element, offset) = parent
                .green()
                .children_to_exclusive(fam_index, self.text_range().start())
                .next()?;

            Some(SyntaxElement::new(
                element,
                parent.clone(),
                parent.green().child_idx(element),
                offset,
            ))
        }
    }

    /// Return the leftmost token in the subtree of this node
    #[inline]
    pub fn first_token(&self) -> Option<SyntaxToken> {
        self.first_child_or_token()?.first_token()
    }

    /// Return the rightmost token in the subtree of this node
    #[inline]
    pub fn last_token(&self) -> Option<SyntaxToken> {
        self.last_child_or_token()?.last_token()
    }

    pub fn siblings(&self, direction: Direction) -> impl Iterator<Item = SyntaxNode> {
        iter::successors(Some(self.clone()), move |node| match direction {
            Direction::Next => node.next_sibling(),
            Direction::Prev => node.prev_sibling(),
        })
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

    pub fn descendants(&self) -> impl Iterator<Item = SyntaxNode> {
        self.preorder().filter_map(|event| match event {
            WalkEvent::Enter(node) => Some(node),
            WalkEvent::Leave(_) => None,
        })
    }

    pub fn descendants_with_tokens(&self) -> impl Iterator<Item = SyntaxElement> {
        self.preorder_with_tokens().filter_map(|event| match event {
            WalkEvent::Enter(it) => Some(it),
            WalkEvent::Leave(_) => None,
        })
    }

    /// Traverse the subtree rooted at the current node (including the current
    /// node) in preorder, excluding tokens.
    #[inline]
    pub fn preorder(&self) -> impl Iterator<Item = WalkEvent<SyntaxNode>> {
        let this = self.clone();
        iter::successors(Some(WalkEvent::Enter(self.clone())), move |pos| {
            let next = match pos {
                WalkEvent::Enter(node) => match node.first_child() {
                    Some(child) => WalkEvent::Enter(child),
                    None => WalkEvent::Leave(node.clone()),
                },
                WalkEvent::Leave(node) => {
                    if node == &this {
                        return None;
                    }
                    match node.next_sibling() {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(node.parent().unwrap()),
                    }
                }
            };
            Some(next)
        })
    }

    /// Traverse the subtree rooted at the current node (including the current
    /// node) in preorder, including tokens.
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

    /// Find a token in the subtree corresponding to this node, which covers the offset.
    /// Precondition: offset must be withing node's range.
    pub fn token_at_offset(&self, offset: TextUnit) -> TokenAtOffset<SyntaxToken> {
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

    /// Return the deepest node or token in the current subtree that fully
    /// contains the range. If the range is empty and is contained in two leaf
    /// nodes, either one can be returned. Precondition: range must be contained
    /// withing the current node
    pub fn covering_element(&self, range: TextRange) -> SyntaxElement {
        let mut res: SyntaxElement = self.clone().into();
        loop {
            assert!(
                range.is_subrange(&res.text_range()),
                "Bad range: node range {:?}, range {:?}",
                res.text_range(),
                range,
            );
            res = match &res {
                NodeOrToken::Token(_) => return res,
                NodeOrToken::Node(node) => {
                    match node
                        .children_with_tokens()
                        .find(|child| range.is_subrange(&child.text_range()))
                    {
                        Some(child) => child,
                        None => return res,
                    }
                }
            };
        }
    }
}

impl SyntaxToken {
    unsafe fn new(parent: SyntaxNode, fam_index: u16, offset: TextUnit) -> SyntaxToken {
        SyntaxToken { parent, fam_index, offset }
    }

    /// Returns a green tree, equal to the green tree this token
    /// belongs two, except with this token substitute. The complexity
    /// of operation is proportional to the depth of the tree
    pub fn replace_with(&self, replacement: GreenToken) -> ArcGreenNode {
        assert_eq!(self.kind(), replacement.kind());
        let mut replacement = Some(replacement);
        let parent = self.parent();
        let me = self.fam_index;

        let children: Vec<GreenElement> = parent
            .green()
            .children()
            .map(|child| {
                if unsafe { parent.green().child_idx(child) } == me {
                    replacement.take().unwrap().into()
                } else {
                    child.to_owned()
                }
            })
            .collect();
        assert!(replacement.is_none());
        let new_parent = GreenNode::new(parent.kind(), children);
        parent.replace_with(new_parent)
    }

    pub fn kind(&self) -> SyntaxKind {
        self.green().kind()
    }

    pub fn text_range(&self) -> TextRange {
        TextRange::offset_len(self.offset, self.green().text_len())
    }

    pub fn text(&self) -> &SmolStr {
        self.green().text()
    }

    pub fn green(&self) -> &GreenToken {
        unsafe {
            self.parent.green().children_slice(self.fam_index..).next().unwrap().as_token().unwrap()
        }
    }

    pub fn parent(&self) -> SyntaxNode {
        self.parent.clone()
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        self.parent().ancestors()
    }

    pub fn next_sibling_or_token(&self) -> Option<SyntaxElement> {
        unsafe {
            let (element, offset) = self
                .parent
                .green()
                .children_from_exclusive(self.fam_index, self.text_range().end())
                .next()?;

            Some(SyntaxElement::new(
                element,
                self.parent.clone(),
                self.parent.green().child_idx(element),
                offset,
            ))
        }
    }

    pub fn prev_sibling_or_token(&self) -> Option<SyntaxElement> {
        unsafe {
            let (element, offset) = self
                .parent
                .green()
                .children_to_exclusive(self.fam_index, self.text_range().start())
                .next()?;

            Some(SyntaxElement::new(
                element,
                self.parent.clone(),
                self.parent.green().child_idx(element),
                offset,
            ))
        }
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

    /// Next token in the tree (i.e, not necessary a sibling)
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
    /// Previous token in the tree (i.e, not necessary a sibling)
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
    unsafe fn new(
        element: GreenElementRef<'_>,
        parent: SyntaxNode,
        fam_index: u16,
        offset: TextUnit,
    ) -> SyntaxElement {
        match element {
            NodeOrToken::Node(node) => {
                SyntaxNode::new_child(node, parent, fam_index, offset).into()
            }
            NodeOrToken::Token(_) => SyntaxToken::new(parent, fam_index, offset).into(),
        }
    }

    pub fn text_range(&self) -> TextRange {
        match self {
            NodeOrToken::Node(it) => it.text_range(),
            NodeOrToken::Token(it) => it.text_range(),
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        match self {
            NodeOrToken::Node(it) => it.kind(),
            NodeOrToken::Token(it) => it.kind(),
        }
    }

    pub fn parent(&self) -> Option<SyntaxNode> {
        match self {
            NodeOrToken::Node(it) => it.parent(),
            NodeOrToken::Token(it) => Some(it.parent()),
        }
    }

    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        match self {
            NodeOrToken::Node(it) => it.ancestors(),
            NodeOrToken::Token(it) => it.parent().ancestors(),
        }
    }

    #[inline]
    pub fn first_token(&self) -> Option<SyntaxToken> {
        match self {
            NodeOrToken::Node(it) => it.first_token(),
            NodeOrToken::Token(it) => Some(it.clone()),
        }
    }

    #[inline]
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

    fn token_at_offset(&self, offset: TextUnit) -> TokenAtOffset<SyntaxToken> {
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
    green: GreenChildren<'static>,
    offset: TextUnit,
    fam_index: u16,
}

impl Iter {
    fn new(parent: SyntaxNode) -> Iter {
        let offset = parent.text_range().start();
        let green: GreenChildren<'_> = parent.green().children();
        // Dirty lifetime laundering: the memory for the children is indirectly owned by parent.
        let green: GreenChildren<'static> =
            unsafe { mem::transmute::<GreenChildren<'_>, GreenChildren<'static>>(green) };
        Iter { parent, green, offset, fam_index: 0 }
    }

    fn next(&mut self) -> Option<((GreenElementRef<'_>, u16, TextUnit))> {
        self.green.next().map(|element| {
            let offset = self.offset;
            let fam_index = self.fam_index;
            self.offset += element.text_len();
            self.fam_index += element.fam_len();
            (element, fam_index, offset)
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
        let parent = self.0.parent.clone();
        while let Some((element, fam_index, offset)) = self.0.next() {
            if let Some(node) = element.as_node() {
                unsafe {
                    return Some(SyntaxNode::new_child(node, parent, fam_index, offset));
                }
            }
        }
        None
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
        let parent = self.0.parent.clone();
        self.0.next().map(|(green, fam_index, offset)| unsafe {
            SyntaxElement::new(green, parent, fam_index, offset)
        })
    }
}

impl GreenNode {
    /// Children starting at a given index, first-to-last.
    /// This is a small constant time more efficient than `children_from_exclusive`.
    ///
    /// `offset` is the start text position at the first element.
    unsafe fn children_from_inclusive(
        &self,
        fam_index: u16,
        mut offset: TextUnit,
    ) -> impl Iterator<Item = (GreenElementRef<'_>, TextUnit)> {
        self.children_slice(fam_index..).map(move |element| {
            let element_offset = offset;
            offset += element.text_len();
            (element, element_offset)
        })
    }

    /// Children starting at a given index, first-to-last.
    ///
    /// `offset` is the end text position at the excluded element.
    unsafe fn children_from_exclusive(
        &self,
        fam_index: u16,
        mut offset: TextUnit,
    ) -> impl Iterator<Item = (GreenElementRef<'_>, TextUnit)> {
        self.children_slice((Bound::Excluded(fam_index), Bound::Unbounded)).map(move |element| {
            let element_offset = offset;
            offset += element.text_len();
            (element, element_offset)
        })
    }

    /// Children ending at a given index, inclusive, last-to-first.
    ///
    /// `offset` is the end text position at the textually last element.
    #[allow(dead_code)]
    unsafe fn children_to_inclusive(
        &self,
        fam_index: u16,
        mut offset: TextUnit,
    ) -> impl Iterator<Item = (GreenElementRef<'_>, TextUnit)> {
        self.children_slice(..=fam_index).rev().map(move |element| {
            offset -= element.text_len();
            (element, offset)
        })
    }

    /// Children ending at a given index, exclusive, last-to-first.
    /// This is a small constant time more efficient than `children_to_inclusive`.
    ///
    /// `offset` is the start text position at the excluded end element.
    unsafe fn children_to_exclusive(
        &self,
        fam_index: u16,
        mut offset: TextUnit,
    ) -> impl Iterator<Item = (GreenElementRef<'_>, TextUnit)> {
        self.children_slice(..fam_index).rev().map(move |element| {
            offset -= element.text_len();
            (element, offset)
        })
    }
}

fn filter_nodes<'a, I: Iterator<Item = (GreenElementRef<'a>, T)>, T>(
    iter: I,
) -> impl Iterator<Item = (&'a GreenNode, T)> {
    iter.filter_map(|(element, data)| match element {
        NodeOrToken::Node(it) => Some((it, data)),
        NodeOrToken::Token(_) => None,
    })
}
