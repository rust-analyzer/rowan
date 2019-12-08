use {
    crate::{
        green::{self, GreenElement, GreenElementBorrow, GreenNode, GreenToken},
        syntax::{FreeList, Generic, Language, Text, Token},
        Direction, Kind, NodeOrToken, WalkEvent,
    },
    rc_borrow::ArcBorrow,
    rc_box::RcBox,
    std::{
        hash::{Hash, Hasher},
        iter::{self, FusedIterator, TrustedLen},
        mem::{self, ManuallyDrop},
        ops::Deref,
        ptr,
        rc::Rc,
        sync::Arc,
    },
    str_index::{StrIndex, StrRange},
};

/// Node (or token) in the syntax tree.
#[derive(Debug, Clone)]
pub struct Node<Lang = Generic> {
    pub(super) inner: ManuallyDrop<Rc<NodeInner>>,
    lang: Lang,
}

#[derive(Debug)]
pub(super) struct NodeInner {
    pub(super) kind: NodeKind,
    pub(super) green: GreenElementBorrow<'static>,
}

#[derive(Debug)]
pub(super) enum NodeKind {
    Root(GreenElement),
    Child { parent: Node<Generic>, index: u16, offset: StrIndex },
    Free(Option<RcBox<NodeInner>>),
}

impl NodeInner {
    pub(super) fn new(kind: NodeKind, green: GreenElementBorrow<'static>) -> RcBox<Self> {
        let mut node = FreeList::with(|list| list.pop()).unwrap_or_else(|| unsafe {
            RcBox::new(NodeInner {
                kind: NodeKind::Free(None),
                green: GreenElementBorrow::dangling(),
            })
        });
        node.kind = kind;
        node.green = green;
        node
    }
}

impl<Lang> Drop for Node<Lang> {
    fn drop(&mut self) {
        unsafe { FreeList::with(|list| list.push(ManuallyDrop::take(&mut self.inner))) }
    }
}

impl<Lang: Language> Eq for Node<Lang> {}
impl<Lang: Language> PartialEq for Node<Lang> {
    fn eq(&self, other: &Self) -> bool {
        match (self.green(), other.green()) {
            (NodeOrToken::Node(lhs), NodeOrToken::Node(rhs)) => {
                ptr::eq(&*lhs, &*rhs) && self.text_range() == other.text_range()
            }
            (NodeOrToken::Token(lhs), NodeOrToken::Token(rhs)) => {
                ptr::eq(&*lhs, &*rhs) && self.text_range() == other.text_range()
            }
            _ => false,
        }
    }
}

impl<Lang: Language> Hash for Node<Lang> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.green() {
            NodeOrToken::Node(node) => ptr::hash(&*node, state),
            NodeOrToken::Token(token) => ptr::hash(&*token, state),
        }
        self.text_range().hash(state)
    }
}

/// Internal helpers.
impl<Lang> Node<Lang> {
    /// Hack to move members out even though a `Drop` impl is provided.
    fn split(self) -> (Lang, Rc<NodeInner>) {
        unsafe {
            let mut this = ManuallyDrop::new(self);
            let inner = ManuallyDrop::take(&mut this.inner);
            let lang = ptr::read(&this.lang);
            (lang, inner)
        }
    }

    /// Separate the language from this node.
    fn splat(self) -> (Lang, Node<Generic>) {
        let (lang, inner) = self.split();
        (lang, Node { inner: ManuallyDrop::new(inner), lang: Generic })
    }
}

/// Constructors and transformations between `Node`s and [`Token`]s.
impl<Lang: Language> Node<Lang> {
    pub(super) fn new_raw(inner: Rc<NodeInner>, lang: Lang) -> Self {
        Node { inner: ManuallyDrop::new(inner), lang }
    }

    /// Create a new root syntax node.
    pub fn new_root(
        green: impl Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>,
        lang: Lang,
    ) -> Self {
        let mut inner = unsafe {
            NodeInner::new(NodeKind::Root(green.into().into()), GreenElementBorrow::dangling())
        };
        inner.green = match &inner.kind {
            NodeKind::Root(root) => unsafe { erase_geb_lt(root.borrow()) },
            _ => unreachable!(),
        };
        Node::new_raw(inner.into(), lang)
    }

    /// # Safety
    ///
    /// `green` must be a descendent of `parent`.
    pub(super) unsafe fn new_child(
        green: NodeOrToken<impl Deref<Target = GreenNode>, impl Deref<Target = GreenToken>>,
        parent: Node<Lang>,
        index: u16,
        offset: StrIndex,
    ) -> Self {
        let (lang, parent) = parent.splat();
        let inner = NodeInner::new(
            NodeKind::Child { parent, index, offset },
            erase_geb_lt(green.as_deref().into()),
        );
        Node::new_raw(inner.into(), lang)
    }

    /// A generic version of this node without language-specific behavior.
    pub fn generic(self) -> Node<Generic> {
        self.splat().1
    }

    /// This node with language-specific behavior.
    pub fn with_lang<L>(self, lang: L) -> Node<L> {
        let (_, inner) = self.split();
        Node { inner: ManuallyDrop::new(inner), lang }
    }

    /// Is this node a token?
    ///
    /// Note: it is possible for a node to be backed by a green token without
    /// being a token, if the green token represents an unparsed subtree.
    /// For more info, see [`Language::is_token`].
    pub fn is_token(&self) -> bool {
        let is_token = self.lang.is_token(self);
        if is_token {
            debug_assert!(self.green().is_token(), "Token nodes must be green tokens");
        }
        is_token
    }

    /// Convert this node into a token.
    ///
    /// # Panics
    ///
    /// Panics if the node is not backed by a green token.
    ///
    /// For a non-panicking version, use `Token::new_ref`.
    pub fn as_token(&self) -> Option<&Token<Lang>> {
        if self.is_token() {
            if let Some(node) = Token::new_ref(self) {
                Some(node)
            } else {
                panic!("Token nodes must be green tokens")
            }
        } else {
            None
        }
    }

    /// Convert this node into a token.
    ///
    /// # Panics
    ///
    /// Panics if the node is not a token or is not backed by a green token.
    ///
    /// For a non-panicking version, use `Token::new`.
    pub fn into_token(self) -> Token<Lang> {
        if self.is_token() {
            if let Some(node) = Token::new(self) {
                node
            } else {
                panic!("Token nodes must be green tokens")
            }
        } else {
            panic!("Called `into_token` on non-token node")
        }
    }
}

/// Tree traversal iterators.
impl<Lang: Language> Node<Lang> {
    /// Walk this subtree, including this node.
    pub fn walk(self) -> impl Iterator<Item = WalkEvent<Node<Lang>>> {
        let this = self.clone();
        iter::successors(Some(WalkEvent::Enter(self)), move |pos| {
            Some(match pos {
                WalkEvent::Enter(node) => match node.clone().children().next() {
                    Some(child) => WalkEvent::Enter(child),
                    None => WalkEvent::Leave(node.clone()),
                },
                WalkEvent::Leave(node) => {
                    if node == &this {
                        return None;
                    }
                    match node.clone().next_sibling() {
                        Some(sibling) => WalkEvent::Enter(sibling),
                        None => WalkEvent::Leave(node.clone().parent().unwrap()),
                    }
                }
            })
        })
    }

    /// All nodes in this subtree in preorder, including this node.
    pub fn preorder(self) -> impl Iterator<Item = Node<Lang>> {
        self.walk().filter_map(WalkEvent::enter)
    }

    /// All nodes in this subtree in postorder, including this node.
    pub fn postorder(self) -> impl Iterator<Item = Node<Lang>> {
        self.walk().filter_map(WalkEvent::leave)
    }
    /// Child nodes (or tokens) of this node.
    pub fn children(self) -> Children<Lang> {
        Children::new(self)
    }

    /// Children non-token subtrees of this node.
    pub fn subtrees(self) -> impl Iterator<Item = Node<Lang>> {
        self.children().filter(|node| !node.is_token())
    }

    /// Sibling trees (and tokens) starting at and including this node.
    pub fn siblings(self, direction: Direction) -> impl Iterator<Item = Node<Lang>> {
        iter::successors(Some(self), move |node| match direction {
            Direction::Next => node.clone().next_sibling(),
            Direction::Prev => node.clone().prev_sibling(),
        })
    }

    /// The parent chain from this node, starting with this node.
    pub fn ancestors(self) -> impl Iterator<Item=Node<Lang>> {
        iter::successors(Some(self), |node| node.clone().parent())
    }
}

/// Accessors.
impl<Lang: Language> Node<Lang> {
    /// The green element backing this syntax node.
    pub fn green(&self) -> NodeOrToken<ArcBorrow<'_, GreenNode>, ArcBorrow<'_, GreenToken>> {
        self.inner.green.into()
    }

    /// The kind of this node.
    pub fn kind(&self) -> Kind {
        self.green().kind()
    }

    /// The text of this node.
    pub fn text(self) -> Text {
        Text::new(self.generic())
    }

    /// The range of text this node covers.
    pub fn text_range(&self) -> StrRange {
        let offset = match self.inner.kind {
            NodeKind::Child { offset, .. } => offset,
            _ => 0.into(),
        };
        offset.range_for(self.green().text_len())
    }

    /// Create a new green tree with this node replaced.
    /// The complexity is proportional to the depth of the tree.
    pub fn replace_with(
        &self,
        replacement: impl Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>,
    ) -> NodeOrToken<Arc<GreenNode>, Arc<GreenToken>> {
        let replacement = replacement.into();
        match self.inner.kind {
            NodeKind::Root(_) => replacement,
            NodeKind::Child { ref parent, index, .. } => {
                let mut replacement = Some(replacement);
                let new_parent: Arc<GreenNode> = GreenNode::new(
                    parent.kind(),
                    parent.green().children().enumerate().map(|(i, child)| {
                        if i as u16 == index {
                            replacement.take().unwrap()
                        } else {
                            child.map(ArcBorrow::upgrade, ArcBorrow::upgrade)
                        }
                    }),
                )
                    .into();
                assert!(replacement.is_none());
                parent.replace_with(new_parent)
            }
            NodeKind::Free(_) => unreachable!(),
        }
    }

    /// The parent of this node.
    pub fn parent(self) -> Option<Node<Lang>> {
        let (lang, this) = self.splat();
        match &this.inner.kind {
            NodeKind::Root(_) => None,
            NodeKind::Child { parent, .. } => Some(parent.clone().with_lang(lang)),
            NodeKind::Free(_) => unreachable!(),
        }
    }

    /// The next token or node of this node's parent.
    pub fn next_sibling(self) -> Option<Node<Lang>> {
        let text_range = self.text_range();
        let (lang, this) = self.splat();
        if let NodeKind::Child { ref parent, index, offset } = this.inner.kind {
            let index = index.checked_add(1)?;
            let parent_green = parent.green();
            let green = parent_green.children().nth(index as usize)?;
            let offset = offset + text_range.len();
            unsafe { Some(Node::new_child(green, parent.clone().with_lang(lang), index, offset)) }
        } else {
            None
        }
    }

    /// The next non-token node of this node's parent.
    pub fn next_subtree(self) -> Option<Node<Lang>> {
        self.siblings(Direction::Next)
            .skip(1) // this
            .find(|node| !node.is_token())
    }

    /// The previous token or node of this node's parent.
    pub fn prev_sibling(self) -> Option<Node<Lang>> {
        let (lang, this) = self.splat();
        if let NodeKind::Child { ref parent, index, offset } = this.inner.kind {
            let index = index.checked_sub(1)?;
            let parent_green = parent.green();
            let green = parent_green.children().nth(index as usize)?;
            let offset = offset - green.text_len();
            unsafe { Some(Node::new_child(green, parent.clone().with_lang(lang), index, offset)) }
        } else {
            None
        }
    }

    /// The previous non-token node of this node's parent.
    pub fn prev_subtree(self) -> Option<Node<Lang>> {
        self.siblings(Direction::Prev)
            .skip(1) // this
            .find(|node| !node.is_token())
    }

    /// The first child node or token of this node.
    pub fn first_child(self) -> Option<Node<Lang>> {
        self.children().next()
    }

    /// The first child non-token subtree of this node.
    pub fn first_subtree(self) -> Option<Node<Lang>> {
        self.children().find(|node| !node.is_token())
    }

    /// The last child node or token of this node.
    pub fn last_child(self) -> Option<Node<Lang>> {
        self.children().next_back()
    }

    /// The last child non-token subtree of this node.
    pub fn last_subtree(self) -> Option<Node<Lang>> {
        self.children().rfind(|node| !node.is_token())
    }

    // TODO: first_token / last_token / tokens: how do we handle green token but not lang token?
}

/// Children nodes in the syntax tree.
#[derive(Debug)]
pub struct Children<Lang> {
    parent: Node<Lang>,
    green: green::Children<'static>,
    offset: StrIndex,
    index: u16,
}

impl<Lang: Language> Children<Lang> {
    fn new(parent: Node<Lang>) -> Children<Lang> {
        let offset = parent.text_range().start();
        let green = parent.green();
        let green = unsafe { erase_gch_lt(green.children()) };
        Children { parent, green, offset, index: 0 }
    }

    fn promote(
        &mut self,
        element: NodeOrToken<ArcBorrow<'_, GreenNode>, ArcBorrow<'_, GreenToken>>,
    ) -> Node<Lang> {
        let offset = self.offset;
        let index = self.index;
        self.offset += element.text_len();
        self.index += 1;
        unsafe { Node::new_child(element, self.parent.clone(), index, offset) }
    }
}

impl<Lang: Language> Iterator for Children<Lang> {
    type Item = Node<Lang>;

    fn next(&mut self) -> Option<Self::Item> {
        self.green.next().map(|element| self.promote(element))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.green.size_hint()
    }

    fn count(self) -> usize {
        self.green.count()
    }

    fn last(mut self) -> Option<Self::Item> {
        self.green.by_ref().last().map(|element| self.promote(element))
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.green.nth(n).map(|element| self.promote(element))
    }
}

impl<Lang: Language> ExactSizeIterator for Children<Lang> {
    fn len(&self) -> usize {
        self.green.len()
    }
}

impl<Lang: Language> DoubleEndedIterator for Children<Lang> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.green.next_back().map(|element| self.promote(element))
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.green.nth_back(n).map(|element| self.promote(element))
    }
}

impl<Lang: Language> FusedIterator for Children<Lang> {}
unsafe impl<Lang: Language> TrustedLen for Children<Lang> {}

unsafe fn erase_geb_lt<'a>(geb: GreenElementBorrow<'_>) -> GreenElementBorrow<'a> {
    mem::transmute(geb)
}

unsafe fn erase_gch_lt<'a>(gch: green::Children<'_>) -> green::Children<'a> {
    mem::transmute(gch)
}
