use {
    crate::{
        green::{GreenNode, GreenToken},
        syntax::{Language, Node, Text},
        Direction, Kind, NodeOrToken,
    },
    rc_borrow::ArcBorrow,
    std::{mem, sync::Arc},
    str_index::StrRange,
};

/// Token in the syntax tree.
#[repr(transparent)] // ref-cast-capable from Node<Lang>
#[derive(Debug, Clone)]
pub struct Token<Lang> {
    /// # Safety
    /// Must be a node backed by a green token
    /// (i.e. `node.green().is_token()` holds).
    node: Node<Lang>,
}

/// Constructors and transformations between `Token`s and [`Node`]s.
impl<Lang: Language> Token<Lang> {
    /// Create a new token from a node.
    ///
    /// This does not check if the language considers the node a token,
    /// just whether the node is backed by a green token.
    /// If you need the language check, check `Node::is_token`.
    pub fn new(node: Node<Lang>) -> Option<Token<Lang>> {
        if node.green().is_token() {
            Some(Token { node })
        } else {
            None
        }
    }

    /// Create a new token reference from a node reference.
    ///
    /// This does not check if the language considers the node a token,
    /// just whether the node is backed by a green token.
    /// If you need the language check, use `Node::as_token`.
    pub fn new_ref(node: &Node<Lang>) -> Option<&Token<Lang>> {
        if node.green().is_token() {
            unsafe { Some(&*(node as *const Node<Lang> as *const Token<Lang>)) }
        } else {
            None
        }
    }

    /// Convert this token back into a node.
    pub fn into_node(self) -> Node<Lang> {
        self.node
    }

    /// Convert this token back into a node.
    pub fn as_node(&self) -> &Node<Lang> {
        &self.node
    }
}

/// Tree traversal iterators.
impl<Lang: Language> Token<Lang> {
    /// The parent chain from this token, starting with this node.
    pub fn ancestors(self) -> impl Iterator<Item = Node<Lang>> {
        self.into_node().ancestors()
    }

    /// Sibling trees starting at and including this token.
    pub fn siblings(self, direction: Direction) -> impl Iterator<Item = Node<Lang>> {
        self.into_node().siblings(direction)
    }
}

/// Accessors.
impl<Lang: Language> Token<Lang> {
    /// The green token backing this token.
    pub fn green(&self) -> ArcBorrow<'_, GreenToken> {
        self.as_node().green().into_token().unwrap()
    }

    /// The kind of this token.
    pub fn kind(&self) -> Kind {
        self.as_node().kind()
    }

    /// The text of this token.
    pub fn text(self) -> Text {
        self.into_node().text()
    }

    /// The range of text this token covers.
    pub fn text_range(&self) -> StrRange {
        self.as_node().text_range()
    }

    /// The raw string of this token.
    pub fn as_str(&self) -> &str {
        unsafe { erase_ref_lt(&self.green().text) }
    }

    /// Create a new green tree with this node replaced.
    /// The complexity is proportional to the depth of the tree.
    pub fn replace_with(
        &self,
        replacement: impl Into<NodeOrToken<Arc<GreenNode>, Arc<GreenToken>>>,
    ) -> NodeOrToken<Arc<GreenNode>, Arc<GreenToken>> {
        self.as_node().replace_with(replacement)
    }

    /// The parent of this token.
    pub fn parent(self) -> Option<Node<Lang>> {
        self.into_node().parent()
    }

    /// The next subtree of this node's parent.
    pub fn next_sibling(self) -> Option<Node<Lang>> {
        self.into_node().next_sibling()
    }

    /// The previous subtree of this node's parent.
    pub fn prev_sibling(self) -> Option<Node<Lang>> {
        self.into_node().prev_sibling()
    }

    // TODO: next_token / prev_token: how do we handle green token but not lang token?
}

unsafe fn erase_ref_lt<'a, T: ?Sized>(x: &T) -> &'a T {
    mem::transmute(x)
}
