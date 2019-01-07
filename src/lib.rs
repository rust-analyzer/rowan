//! A generic library for lossless syntax trees.
//! See `examples/s_expressions.rs` for a tutorial.
#![forbid(
    missing_debug_implementations,
    unconditional_recursion,
    future_incompatible,
    missing_docs
)]
#![deny(unsafe_code)]

extern crate parking_lot;
extern crate smol_str;
extern crate text_unit;

mod green;

#[allow(unsafe_code)]
mod imp;
#[allow(unsafe_code)]
mod swap_cell;

use std::{
    fmt,
    hash::{Hash, Hasher},
    ops::Range,
    ptr,
};

pub use crate::{
    green::{GreenNode, GreenNodeBuilder},
    imp::{SyntaxNode, TreePtr},
    smol_str::SmolStr,

    // Reexport types for working with strings.
    // We might be too opinionated about these,
    // as a custom interner might work better,
    // but `SmolStr` is a pretty good default.
    text_unit::{TextRange, TextUnit},
};

/// `Types` customizes data, stored in the
/// syntax tree. All types in this crate are
/// parametrized over `T: Types`.
pub trait Types: Send + Sync + 'static {
    /// `Kind` is stored in each node and designates
    /// it's class. Typically it is a fieldless enum.
    type Kind: fmt::Debug + Copy + Eq + Send + Sync;
    /// `RootData` is stored in the root of the syntax trees.
    /// It can be just `()`, but you can use it to store,
    /// for example, syntax errors.
    type RootData: fmt::Debug + Send + Sync;
}

impl<T: Types> fmt::Debug for SyntaxNode<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}@{:?}", self.kind(), self.range())
    }
}

impl<T: Types> fmt::Display for SyntaxNode<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        self.preorder()
            .filter_map(|event| match event {
                WalkEvent::Enter(node) => Some(node),
                _ => None,
            })
            .filter_map(|it| it.leaf_text())
            .try_for_each(|it| write!(fmt, "{}", it))
    }
}

// SyntaxNodes have identity equality semantics
impl<T: Types> PartialEq<SyntaxNode<T>> for SyntaxNode<T> {
    fn eq(&self, other: &SyntaxNode<T>) -> bool {
        ptr::eq(self, other)
    }
}

impl<T: Types> Eq for SyntaxNode<T> {}
impl<T: Types> Hash for SyntaxNode<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self as *const SyntaxNode<T>).hash(state)
    }
}

impl<T: Types> fmt::Debug for TreePtr<SyntaxNode<T>> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let inner: &SyntaxNode<T> = &*self;
        fmt::Debug::fmt(inner, fmt)
    }
}

/// `WalkeEvent` describes tree walking process.
#[derive(Debug, Copy, Clone)]
pub enum WalkEvent<T> {
    /// Fired before traversing the node.
    Enter(T),
    /// Fired after the node is traversed.
    Leave(T),
}

/// There might be zero, one or two leaves at a given offset.
#[derive(Clone, Debug)]
pub enum LeafAtOffset<T> {
    /// No leaves at offset -- possible for the empty file.
    None,
    /// Only a single leaf at offset.
    Single(T),
    /// Offset is exactly between two leaves.
    Between(T, T),
}

impl<T> LeafAtOffset<T> {
    /// Convert to option, preferring the right leaf in case of a tie.
    pub fn right_biased(self) -> Option<T> {
        match self {
            LeafAtOffset::None => None,
            LeafAtOffset::Single(node) => Some(node),
            LeafAtOffset::Between(_, right) => Some(right),
        }
    }
    /// Convert to option, preferring the left leaf in case of a tie.
    pub fn left_biased(self) -> Option<T> {
        match self {
            LeafAtOffset::None => None,
            LeafAtOffset::Single(node) => Some(node),
            LeafAtOffset::Between(left, _) => Some(left),
        }
    }
}

impl<T> Iterator for LeafAtOffset<T> {
    type Item = T;

    fn next(&mut self) -> Option<T> {
        match std::mem::replace(self, LeafAtOffset::None) {
            LeafAtOffset::None => None,
            LeafAtOffset::Single(node) => {
                *self = LeafAtOffset::None;
                Some(node)
            }
            LeafAtOffset::Between(left, right) => {
                *self = LeafAtOffset::Single(right);
                Some(left)
            }
        }
    }
}

impl<T: Types> SyntaxNode<T> {
    /// Creates a new `SyntaxNode`, whihc becomes the root of the tree.
    pub fn new(green: GreenNode<T>, data: T::RootData) -> TreePtr<SyntaxNode<T>> {
        Self::new_root(green, data)
    }

    /// Switch this node to owned flavor.
    pub fn to_owned(&self) -> TreePtr<SyntaxNode<T>> {
        TreePtr::new(self)
    }

    /// Get the green node for this node
    pub fn green(&self) -> &GreenNode<T> {
        &self.green
    }
    /// Get the **root** node but with the children replaced. See `replace_with`.
    pub fn replace_children(&self, children: Box<[GreenNode<T>]>) -> GreenNode<T> {
        self.replace_self(GreenNode::new_branch(self.kind(), children))
    }

    /// Returns a green tree, equal to the green tree this node
    /// belongs two, except with this node substitute. The complexity
    /// of operation is proportional to the depth of the tree
    /// TODO: naming is unfortunate, the return value is not *current*
    /// node, it is the new root node.
    pub fn replace_self(&self, green: GreenNode<T>) -> GreenNode<T> {
        assert_eq!(self.kind(), green.kind());
        match self.parent() {
            None => green,
            Some(parent) => {
                let children: Vec<_> = parent
                    .children()
                    .map(|child| {
                        if child == self {
                            green.clone()
                        } else {
                            child.green.clone()
                        }
                    })
                    .collect();
                let new_parent = GreenNode::new_branch(parent.kind(), children.into_boxed_slice());
                parent.replace_self(new_parent)
            }
        }
    }

    /// Returns `true` if this node is a leaf node.
    pub fn is_leaf(&self) -> bool {
        self.green.leaf_text().is_some()
    }

    /// Text of this node if it is a leaf.
    // Only for `RefRoot` to extend lifetime to `'a`.
    pub fn leaf_text<'a>(&'a self) -> Option<&'a SmolStr> {
        self.green.leaf_text()
    }

    /// Get root data.
    pub fn root_data(&self) -> &T::RootData {
        &self.root().data
    }

    /// Get kind of this node.
    pub fn kind(&self) -> T::Kind {
        self.green.kind()
    }

    /// Get text range, covered by this node.
    pub fn range(&self) -> TextRange {
        TextRange::offset_len(self.start_offset(), self.green.text_len())
    }

    /// Get the parent node.
    pub fn parent(&self) -> Option<&SyntaxNode<T>> {
        self.parent_impl()
    }

    /// Get first child.
    pub fn first_child(&self) -> Option<&SyntaxNode<T>> {
        self.get_child(0)
    }

    /// Get last child.
    pub fn last_child(&self) -> Option<&SyntaxNode<T>> {
        let n = self.n_children();
        let n = n.checked_sub(1)?;
        self.get_child(n)
    }

    /// Get next sibling.
    pub fn next_sibling(&self) -> Option<&SyntaxNode<T>> {
        let parent = self.parent()?;
        let next_sibling_idx = self.index_in_parent()? + 1;
        parent.get_child(next_sibling_idx)
    }

    /// Get previous sibling.
    pub fn prev_sibling(&self) -> Option<&SyntaxNode<T>> {
        let parent = self.parent()?;
        let prev_sibling_idx = self.index_in_parent()?.checked_sub(1)?;
        parent.get_child(prev_sibling_idx)
    }

    /// Get iterator over children.
    pub fn children(&self) -> SyntaxNodeChildren<T> {
        SyntaxNodeChildren {
            parent: self,
            iter: (0..self.n_children()),
        }
    }

    /// All ancestors of the current node, including itself
    pub fn ancestors(&self) -> impl Iterator<Item = &SyntaxNode<T>> {
        generate(Some(self), |node| node.parent())
    }

    /// Traverse the subtree rooted at the current node (including the current
    /// node) in preorder.
    pub fn preorder(&self) -> impl Iterator<Item = WalkEvent<&SyntaxNode<T>>> {
        generate(Some(WalkEvent::Enter(self)), move |pos| {
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

    /// Returns common ancestor of the two nodes.
    /// Precondition: nodes must be from the same tree.
    pub fn common_ancestor<'a>(&'a self, other: &'a SyntaxNode<T>) -> &'a SyntaxNode<T> {
        // TODO: use small-vec to memoize other's ancestors
        for p in self.ancestors() {
            if other.ancestors().any(|a| a == p) {
                return p;
            }
        }
        panic!("No common ancestor for {:?} and {:?}", self, other)
    }

    /// Find a leaf in the subtree corresponding to this node, which covers the offset.
    /// Precondition: offset must be withing node's range.
    pub fn leaf_at_offset(&self, offset: TextUnit) -> LeafAtOffset<&SyntaxNode<T>> {
        // TODO: replace with non-recursive implementation
        let range = self.range();
        assert!(
            range.start() <= offset && offset <= range.end(),
            "Bad offset: range {:?} offset {:?}",
            range,
            offset
        );
        if range.is_empty() {
            return LeafAtOffset::None;
        }

        if self.is_leaf() {
            return LeafAtOffset::Single(self);
        }

        let mut children = self.children().filter(|child| {
            let child_range = child.range();
            !child_range.is_empty()
                && (child_range.start() <= offset && offset <= child_range.end())
        });

        let left = children.next().unwrap();
        let right = children.next();
        assert!(children.next().is_none());

        if let Some(right) = right {
            match (left.leaf_at_offset(offset), right.leaf_at_offset(offset)) {
                (LeafAtOffset::Single(left), LeafAtOffset::Single(right)) => {
                    LeafAtOffset::Between(left, right)
                }
                _ => unreachable!(),
            }
        } else {
            left.leaf_at_offset(offset)
        }
    }

    /// Return the deepest node in the current subtree that fully contains the range.
    /// If the range is empty and is contained in two leaf nodes, either one can be returned.
    /// Precondition: range must be contained withing the current node
    pub fn covering_node(&self, range: TextRange) -> &SyntaxNode<T> {
        let mut res = self;
        loop {
            assert!(
                range.is_subrange(&res.range()),
                "Bad range: node range {:?}, range {:?}",
                res.range(),
                range,
            );
            res = match res
                .children()
                .find(|child| range.is_subrange(&child.range()))
            {
                Some(child) => child,
                None => return res,
            }
        }
    }
}

/// Iterator over node's children.
#[derive(Debug)]
pub struct SyntaxNodeChildren<'a, T: Types> {
    parent: &'a SyntaxNode<T>,
    iter: Range<usize>,
}

impl<'a, T: Types> Iterator for SyntaxNodeChildren<'a, T> {
    type Item = &'a SyntaxNode<T>;

    fn next(&mut self) -> Option<&'a SyntaxNode<T>> {
        self.iter.next().map(|i| self.parent.get_child(i).unwrap())
    }
}

fn generate<'a, T: 'a, F: Fn(&T) -> Option<T> + 'a>(
    seed: Option<T>,
    step: F,
) -> impl Iterator<Item = T> + 'a {
    std::iter::repeat(()).scan(seed, move |state, ()| {
        state.take().map(|curr| {
            *state = step(&curr);
            curr
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Copy)]
    enum SillyTypes {}
    impl Types for SillyTypes {
        type Kind = u8;
        type RootData = ();
    }

    #[test]
    fn assert_send_sync() {
        fn f<T: Send + Sync>() {}
        f::<GreenNode<SillyTypes>>();
        f::<SyntaxNode<SillyTypes>>();
        f::<TreePtr<SyntaxNode<SillyTypes>>>();
    }
}
