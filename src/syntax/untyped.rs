use {
    crate::{
        green::{self, GreenElement, GreenElementBorrow, GreenNode, GreenToken},
        syntax::{FreeList, Text},
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
    text_unit::TextUnit,
};

/// Node (or token) in the syntax tree.
#[derive(Debug, Clone)]
pub struct Node {
    pub(super) inner: ManuallyDrop<Rc<Inner>>,
}

#[derive(Debug)]
pub(super) struct Inner {
    pub(super) kind: NodeKind,
    pub(super) green: GreenElementBorrow<'static>,
}

#[derive(Debug)]
pub(super) enum NodeKind {
    Root(GreenElement),
    Child { parent: Node, index: u16, offset: TextUnit },
    Free(Option<RcBox<Inner>>),
}

impl Inner {
    fn new(kind: NodeKind, green: GreenElementBorrow<'static>) -> RcBox<Inner> {
        let mut node = FreeList::with(|list| list.pop()).unwrap_or_else(|| unsafe {
            RcBox::new(Inner { kind: NodeKind::Free(None), green: GreenElementBorrow::dangling() })
        });
        node.kind = kind;
        node.green = green;
        node
    }
}

impl Drop for Node {
    fn drop(&mut self) {
        unsafe { FreeList::with(|list| list.push(ManuallyDrop::take(&mut self.inner))) }
    }
}

impl Eq for Node {}
impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        match (self.green(), other.green()) {
            (NodeOrToken::Node(lhs), NodeOrToken::Node(rhs)) => {
                ptr::eq(&*lhs, &*rhs) && self.text().range() == other.text().range()
            }
            (NodeOrToken::Token(lhs), NodeOrToken::Token(rhs)) => {
                ptr::eq(&*lhs, &*rhs) && self.text().range() == other.text().range()
            }
            _ => false,
        }
    }
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self.green() {
            NodeOrToken::Node(node) => ptr::hash(&*node, state),
            NodeOrToken::Token(token) => ptr::hash(&*token, state),
        }
        self.text().range().hash(state)
    }
}

impl Node {
    fn new(inner: Rc<Inner>) -> Node {
        Node { inner: ManuallyDrop::new(inner) }
    }

    /// Create a new root syntax node.
    pub fn new_root(green: impl Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>) -> Node {
        let mut inner = unsafe {
            Inner::new(NodeKind::Root(green.into().into()), GreenElementBorrow::dangling())
        };
        inner.green = match &inner.kind {
            NodeKind::Root(root) => unsafe { erase_geb_lt(root.borrow()) },
            _ => unreachable!(),
        };
        Node::new(inner.into())
    }

    /// # Safety
    ///
    /// `green` must be a descendent of `parent`.
    unsafe fn new_child(
        green: NodeOrToken<impl Deref<Target = GreenNode>, impl Deref<Target = GreenToken>>,
        parent: Node,
        index: u16,
        offset: TextUnit,
    ) -> Node {
        let inner = Inner::new(
            NodeKind::Child { parent, index, offset },
            erase_geb_lt(green.as_deref().into()),
        );
        Node::new(inner.into())
    }

    /// The green element backing this syntax node.
    pub fn green(&self) -> NodeOrToken<ArcBorrow<'_, GreenNode>, ArcBorrow<'_, GreenToken>> {
        self.inner.green.into()
    }

    /// The kind of this node.
    pub fn kind(&self) -> Kind {
        self.green().kind()
    }

    /// The text of this node.
    pub fn text(&self) -> Text {
        Text::new(self.clone())
    }

    /// Create a new green tree with this node replaced.
    /// The complexity is proportional to the depth of the tree.
    pub fn replace_with(&self, replacement: Arc<GreenNode>) -> Arc<GreenNode> {
        match self.inner.kind {
            NodeKind::Root(_) => replacement,
            NodeKind::Child { ref parent, index, .. } => {
                let mut replacement = Some(replacement);
                let new_parent = GreenNode::new(
                    parent.kind(),
                    parent.green().children().enumerate().map(|(i, child)| {
                        if i as u16 == index {
                            replacement.take().unwrap().into()
                        } else {
                            child.map(ArcBorrow::upgrade, ArcBorrow::upgrade)
                        }
                    }),
                );
                assert!(replacement.is_none());
                parent.replace_with(new_parent.into())
            }
            NodeKind::Free(_) => unreachable!(),
        }
    }

    /// The parent of this node.
    pub fn parent(&self) -> Option<Node> {
        match &self.inner.kind {
            NodeKind::Root(_) => None,
            NodeKind::Child { parent, .. } => Some(parent.clone()),
            NodeKind::Free(_) => unreachable!(),
        }
    }

    /// The parent chain from this node, starting with this node.
    pub fn ancestors(&self) -> impl Iterator<Item = Node> {
        iter::successors(Some(self.clone()), Node::parent)
    }

    /// The next subtree of this node's parent.
    pub fn next_sibling(&self) -> Option<Node> {
        if let NodeKind::Child { ref parent, index, offset } = self.inner.kind {
            let index = index.checked_add(1)?;
            let parent_green = parent.green();
            let green = parent_green.children().nth(index as usize)?;
            let offset = offset + self.text().range().len();
            unsafe { Some(Node::new_child(green, parent.clone(), index, offset)) }
        } else {
            None
        }
    }

    /// The previous subtree of this node's parent.
    pub fn prev_sibling(&self) -> Option<Node> {
        if let NodeKind::Child { ref parent, index, offset } = self.inner.kind {
            let index = index.checked_sub(1)?;
            let parent_green = parent.green();
            let green = parent_green.children().nth(index as usize)?;
            let offset = offset - green.text_len();
            unsafe { Some(Node::new_child(green, parent.clone(), index, offset)) }
        } else {
            None
        }
    }

    /// Subtrees of this node.
    pub fn children(&self) -> Children {
        Children::new(self.clone())
    }

    /// Sibling trees starting at and including this node.
    pub fn siblings(self, direction: Direction) -> impl Iterator<Item = Node> {
        iter::successors(Some(self), move |node| match direction {
            Direction::Next => node.next_sibling(),
            Direction::Prev => node.prev_sibling(),
        })
    }

    /// Walk this subtree, including this node.
    pub fn walk(self) -> impl Iterator<Item = WalkEvent<Node>> {
        let this = self.clone();
        iter::successors(Some(WalkEvent::Enter(self)), move |pos| {
            Some(match pos {
                WalkEvent::Enter(node) => match node.children().next() {
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
            })
        })
    }

    /// All nodes in this subtree in preorder, including this node.
    pub fn preorder(self) -> impl Iterator<Item = Node> {
        self.walk().filter_map(WalkEvent::enter)
    }

    /// All nodes in this subtree in postorder, including this node.
    pub fn postorder(self) -> impl Iterator<Item = Node> {
        self.walk().filter_map(WalkEvent::leave)
    }
}

/// Children nodes in the syntax tree.
#[derive(Debug)]
pub struct Children {
    parent: Node,
    green: green::Children<'static>,
    offset: TextUnit,
    index: u16,
}

impl Children {
    fn new(parent: Node) -> Children {
        let offset = parent.text().range().start();
        let green = parent.green();
        let green = unsafe { erase_gch_lt(green.children()) };
        Children { parent, green, offset, index: 0 }
    }

    fn promote(
        &mut self,
        element: NodeOrToken<ArcBorrow<'_, GreenNode>, ArcBorrow<'_, GreenToken>>,
    ) -> Node {
        let offset = self.offset;
        let index = self.index;
        self.offset += element.text_len();
        self.index += 1;
        unsafe { Node::new_child(element, self.parent.clone(), index, offset) }
    }
}

impl Iterator for Children {
    type Item = Node;

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

impl ExactSizeIterator for Children {
    fn len(&self) -> usize {
        self.green.len()
    }
}

impl DoubleEndedIterator for Children {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.green.next_back().map(|element| self.promote(element))
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        self.green.nth_back(n).map(|element| self.promote(element))
    }
}

impl FusedIterator for Children {}
unsafe impl TrustedLen for Children {}

unsafe fn erase_geb_lt<'a>(geb: GreenElementBorrow<'_>) -> GreenElementBorrow<'a> {
    mem::transmute(geb)
}

unsafe fn erase_gch_lt<'a>(gch: green::Children<'_>) -> green::Children<'a> {
    mem::transmute(gch)
}
