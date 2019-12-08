use std::{
    alloc::{Layout, LayoutErr},
    ops::Deref,
};

pub(crate) fn repr_c_3(fields: [Layout; 3]) -> Result<(Layout, [usize; 3]), LayoutErr> {
    let mut offsets: [usize; 3] = [0; 3];
    let mut layout = fields[0];
    for i in 1..3 {
        let (new_layout, this_offset) = layout.extend(fields[i])?;
        layout = new_layout;
        offsets[i] = this_offset;
    }
    Ok((layout.pad_to_align(), offsets))
}

pub(crate) fn repr_c_4(fields: [Layout; 4]) -> Result<(Layout, [usize; 4]), LayoutErr> {
    let mut offsets: [usize; 4] = [0; 4];
    let mut layout = fields[0];
    for i in 1..4 {
        let (new_layout, this_offset) = layout.extend(fields[i])?;
        layout = new_layout;
        offsets[i] = this_offset;
    }
    Ok((layout.pad_to_align(), offsets))
}

/// Convenience enum wrapping either a node or a token.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum NodeOrToken<Node, Token> {
    Node(Node),
    Token(Token),
}

#[allow(missing_docs)]
impl<Node, Token> NodeOrToken<Node, Token> {
    pub fn into_node(self) -> Option<Node> {
        self.map(Some, |_| None).flatten()
    }

    pub fn as_node(&self) -> Option<&Node> {
        self.as_ref().into_node()
    }

    pub fn is_node(&self) -> bool {
        self.as_node().is_some()
    }

    pub fn into_token(self) -> Option<Token> {
        self.map(|_| None, Some).flatten()
    }

    pub fn as_token(&self) -> Option<&Token> {
        self.as_ref().into_token()
    }

    pub fn is_token(&self) -> bool {
        self.as_token().is_some()
    }
}

#[allow(missing_docs)]
impl<Node, Token> NodeOrToken<Node, Token> {
    pub fn as_ref(&self) -> NodeOrToken<&Node, &Token> {
        match *self {
            NodeOrToken::Node(ref node) => NodeOrToken::Node(node),
            NodeOrToken::Token(ref token) => NodeOrToken::Token(token),
        }
    }

    pub(crate) fn map<N, T>(
        self,
        n: impl FnOnce(Node) -> N,
        t: impl FnOnce(Token) -> T,
    ) -> NodeOrToken<N, T> {
        match self {
            NodeOrToken::Node(node) => NodeOrToken::Node(n(node)),
            NodeOrToken::Token(token) => NodeOrToken::Token(t(token)),
        }
    }

    pub fn as_deref(&self) -> NodeOrToken<&Node::Target, &Token::Target>
    where
        Node: Deref,
        Token: Deref,
    {
        self.as_ref().map(Deref::deref, Deref::deref)
    }
}

impl<T> NodeOrToken<T, T> {
    pub(crate) fn flatten(self) -> T {
        match self {
            NodeOrToken::Node(node) => node,
            NodeOrToken::Token(token) => token,
        }
    }
}

/// Direction of an iterator.
#[allow(missing_docs)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Direction {
    Next,
    Prev,
}

/// Steps in the tree walking process.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum WalkEvent<T> {
    /// Fired when entering the node.
    /// Listen for this event for preorder traversal.
    Enter(T),
    /// Fired when leaving the node.
    /// Listen for this event for postorder traversal.
    Leave(T),
}

#[allow(missing_docs)]
impl<T> WalkEvent<T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> WalkEvent<U> {
        match self {
            WalkEvent::Enter(t) => WalkEvent::Enter(f(t)),
            WalkEvent::Leave(t) => WalkEvent::Leave(f(t)),
        }
    }

    pub fn enter(self) -> Option<T> {
        match self {
            WalkEvent::Enter(t) => Some(t),
            WalkEvent::Leave(_) => None,
        }
    }

    pub fn leave(self) -> Option<T> {
        match self {
            WalkEvent::Enter(_) => None,
            WalkEvent::Leave(t) => Some(t),
        }
    }
}
