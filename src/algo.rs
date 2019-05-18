use std::{fmt, iter};
use std::ops::Range;

use crate::{SyntaxNode, SyntaxElement, TextUnit, TextRange, SyntaxToken, SyntaxIndex};

/// `WalkEvent` describes tree walking process.
#[derive(Debug, Copy, Clone)]
pub enum WalkEvent<T> {
    /// Fired before traversing the node.
    Enter(T),
    /// Fired after the node is traversed.
    Leave(T),
}

/// There might be zero, one or two leaves at a given offset.
#[derive(Clone, Debug)]
pub enum TokenAtOffset<T> {
    /// No leaves at offset -- possible for the empty file.
    None,
    /// Only a single leaf at offset.
    Single(T),
    /// Offset is exactly between two leaves.
    Between(T, T),
}

impl<T> TokenAtOffset<T> {
    /// Convert to option, preferring the right leaf in case of a tie.
    pub fn right_biased(self) -> Option<T> {
        match self {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(node) => Some(node),
            TokenAtOffset::Between(_, right) => Some(right),
        }
    }
    /// Convert to option, preferring the left leaf in case of a tie.
    pub fn left_biased(self) -> Option<T> {
        match self {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(node) => Some(node),
            TokenAtOffset::Between(left, _) => Some(left),
        }
    }
}

impl<T> Iterator for TokenAtOffset<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match std::mem::replace(self, TokenAtOffset::None) {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(node) => {
                *self = TokenAtOffset::None;
                Some(node)
            }
            TokenAtOffset::Between(left, right) => {
                *self = TokenAtOffset::Single(right);
                Some(left)
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            TokenAtOffset::None => (0, Some(0)),
            TokenAtOffset::Single(_) => (1, Some(1)),
            TokenAtOffset::Between(_,_) => (2, Some(2)),
        }
    }
}

impl<T> ExactSizeIterator for TokenAtOffset<T> {}

/// Iterator over node's children, excluding tokens.
#[derive(Debug)]
pub struct SyntaxNodeChildren<'a> {
    parent: &'a SyntaxNode,
    iter: Range<u32>,
}

impl<'a> Iterator for SyntaxNodeChildren<'a> {
    type Item = &'a SyntaxNode;

    #[inline]
    fn next(&mut self) -> Option<&'a SyntaxNode> {
        self.iter.next().map(|i| self.parent.get_child(SyntaxIndex(i)).unwrap())
    }
}

#[derive(Debug)]
/// Iterator over node's children, including tokens.
pub struct SyntaxElementChildren<'a> {
    current: Option<SyntaxElement<'a>>,
}

impl<'a> Iterator for SyntaxElementChildren<'a> {
    type Item = SyntaxElement<'a>;

    #[inline]
    fn next(&mut self) -> Option<SyntaxElement<'a>> {
        self.current.take().map(|current| {
            self.current = current.next_sibling_or_token();
            current
        })
    }
}

/// Display implementor which indents the AST and executes a callback
/// on each element
#[derive(Clone, Debug)]
pub struct SyntaxNodeDump<'a, FE, FL>
where
    FE: Fn(&mut fmt::Formatter, SyntaxElement) -> fmt::Result,
    FL: Fn(&mut fmt::Formatter, &SyntaxNode) -> fmt::Result
{
    indent: usize,
    node: &'a SyntaxNode,
    enter: FE,
    leave: Option<FL>
}
impl<'a, FE, FL> fmt::Display for SyntaxNodeDump<'a, FE, FL>
where
    FE: Fn(&mut fmt::Formatter, SyntaxElement) -> fmt::Result,
    FL: Fn(&mut fmt::Formatter, &SyntaxNode) -> fmt::Result
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        let mut indent = 0;
        for event in self.node.preorder_with_tokens() {
            match event {
                WalkEvent::Enter(elem) => {
                    if !first {
                        writeln!(f)?;
                    }
                    write!(f, "{:indent$}", "", indent=indent)?;
                    (self.enter)(f, elem)?;

                    if let SyntaxElement::Node(_) = elem {
                        indent += self.indent;
                    }
                },
                WalkEvent::Leave(SyntaxElement::Node(node)) => {
                    indent -= self.indent;

                    if let Some(ref leave) = self.leave {
                        if !first {
                            writeln!(f)?;
                        }
                        write!(f, "{:indent$}", "", indent=indent)?;
                        leave(f, node)?;
                    }
                },
                WalkEvent::Leave(SyntaxElement::Token(_)) => {}
            }
            first = false;
        }
        Ok(())
    }
}

impl SyntaxNode {
    /// Get iterator over children, excluding tokens.
    #[inline]
    pub fn children(&self) -> SyntaxNodeChildren<'_> {
        SyntaxNodeChildren { parent: self, iter: (0..self.children_len().0) }
    }

    /// Get iterator over children, including tokens.
    #[inline]
    pub fn children_with_tokens(&self) -> SyntaxElementChildren<'_> {
        SyntaxElementChildren { current: self.first_child_or_token() }
    }

    /// All ancestors of the current node, including itself
    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = &SyntaxNode> {
        iter::successors(Some(self), |node| node.parent())
    }

    /// Traverse the subtree rooted at the current node (including the current
    /// node) in preorder, excluding tokens.
    #[inline]
    pub fn preorder(&self) -> impl Iterator<Item = WalkEvent<&SyntaxNode>> {
        iter::successors(Some(WalkEvent::Enter(self)), move |pos| {
            let next = match *pos {
                WalkEvent::Enter(node) => match node.first_child() {
                    Some(child) => WalkEvent::Enter(child),
                    None => WalkEvent::Leave(node),
                },
                WalkEvent::Leave(node) => {
                    if node == self {
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
    pub fn preorder_with_tokens<'a>(
        &'a self,
    ) -> impl Iterator<Item = WalkEvent<SyntaxElement<'a>>> {
        let start: SyntaxElement = self.into();
        iter::successors(Some(WalkEvent::Enter(start)), move |pos| {
            let next = match *pos {
                WalkEvent::Enter(el) => match el {
                    SyntaxElement::Node(node) => match node.first_child_or_token() {
                        Some(child) => WalkEvent::Enter(child),
                        None => WalkEvent::Leave(node.into()),
                    },
                    SyntaxElement::Token(token) => WalkEvent::Leave(token.into()),
                },
                WalkEvent::Leave(el) => {
                    if el == start {
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

    /// Traverse the subtree and call an function for printing the
    /// representation of each SyntaxElement.
    /// ```rust
    /// use rowan::{syntax, GreenNode, GreenToken, GreenElement, SyntaxElement, SyntaxNode, SmolStr};
    /// use std::fmt::{Formatter, Display};
    /// syntax! { start 0; resolver MyResolver; MY_SYNTAX_TOKEN MY_SYNTAX_NODE}
    ///
    /// fn my_dump<'a>(node: &'a SyntaxNode) -> impl Display + 'a {
    ///     node.dump(2, |f, elem| match elem {
    ///         SyntaxElement::Node(node) => write!(f, "{} {{", node.kind().name::<MyResolver>()),
    ///         SyntaxElement::Token(token) => write!(f, "{} = \"{}\"", token.kind().name::<MyResolver>(), token.text().escape_default()),
    ///     }, Some(|f: &mut Formatter, _: &_| write!(f, "}}")))
    /// }
    ///
    /// let node = SyntaxNode::new(GreenNode::new(
    ///     MY_SYNTAX_NODE,
    ///     Box::new([GreenElement::Token(GreenToken::new(MY_SYNTAX_TOKEN, SmolStr::new("hi!")))])
    /// ), None);
    ///
    /// assert_eq!(format!("{}", my_dump(&node)),
    /// r#"MY_SYNTAX_NODE {
    ///   MY_SYNTAX_TOKEN = "hi!"
    /// }"#);
    /// ```
    pub fn dump<'a, FE, FL>(&'a self, indent: usize, enter: FE, leave: Option<FL>) -> SyntaxNodeDump<'a, FE, FL>
    where
        FE: Fn(&mut fmt::Formatter, SyntaxElement) -> fmt::Result,
        FL: Fn(&mut fmt::Formatter, &SyntaxNode) -> fmt::Result
    {
        SyntaxNodeDump {
            node: self,
            indent,
            enter,
            leave
        }
    }

    /// Returns common ancestor of the two nodes.
    /// Precondition: nodes must be from the same tree.
    pub fn common_ancestor<'a>(&'a self, other: &'a SyntaxNode) -> &'a SyntaxNode {
        // TODO: use small-vec to memoize other's ancestors
        for p in self.ancestors() {
            if other.ancestors().any(|a| a == p) {
                return p;
            }
        }
        panic!("No common ancestor for {:?} and {:?}", self, other)
    }

    /// Find a token in the subtree corresponding to this node, which covers the offset.
    /// Precondition: offset must be withing node's range.
    pub fn token_at_offset<'a>(&'a self, offset: TextUnit) -> TokenAtOffset<SyntaxToken<'a>> {
        // TODO: this could be faster if we first drill-down to node, and only
        // then switch to token search. We should also replace explicit
        // recursion with a loop.
        let range = self.range();
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
            let child_range = child.range();
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
    pub fn covering_node(&self, range: TextRange) -> SyntaxElement {
        let mut res: SyntaxElement = self.into();
        loop {
            assert!(
                range.is_subrange(&res.range()),
                "Bad range: node range {:?}, range {:?}",
                res.range(),
                range,
            );
            res = match res {
                SyntaxElement::Token(_) => return res,
                SyntaxElement::Node(node) => {
                    match node
                        .children_with_tokens()
                        .find(|child| range.is_subrange(&child.range()))
                    {
                        Some(child) => child,
                        None => return res,
                    }
                }
            };
        }
    }

    /// Number of memory bytes of occupied by subtree rooted at `self`.
    pub fn memory_size_of_subtree(&self) -> usize {
        std::mem::size_of::<Self>()
            + self.green().memory_size_of_subtree()
            + self.memory_size_of_red_children()
    }
}

impl<'a> SyntaxElement<'a> {
    fn token_at_offset(&self, offset: TextUnit) -> TokenAtOffset<SyntaxToken<'a>> {
        assert!(self.range().start() <= offset && offset <= self.range().end());
        match *self {
            SyntaxElement::Token(token) => TokenAtOffset::Single(token),
            SyntaxElement::Node(node) => node.token_at_offset(offset),
        }
    }
}
