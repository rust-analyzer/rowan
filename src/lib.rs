//! A generic library for lossless syntax trees.
//! See `examples/s_expressions.rs` for a tutorial.
#![forbid(
    missing_debug_implementations,
    unconditional_recursion,
    future_incompatible,
    missing_docs
)]

extern crate parking_lot;
extern crate smol_str;
extern crate text_unit;

mod green;
mod red;
mod roots;
mod syntax;

use std::fmt::Debug;

pub use crate::{
    green::{GreenNode, GreenNodeBuilder},
    roots::{OwnedRoot, RefRoot, TreeRoot},
    smol_str::SmolStr,

    syntax::{LeafAtOffset, SyntaxNode, SyntaxNodeChildren, WalkEvent},
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
    type Kind: Debug + Copy + Eq + Send + Sync;
    /// `RootData` is stored in the root of the syntax trees.
    /// It can be just `()`, but you can use it to store,
    /// for example, syntax errors.
    type RootData: Debug + Send + Sync;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::red::RedNode;

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
        f::<RedNode<SillyTypes>>();
        // f::<SyntaxNode>();
    }

    #[test]
    fn syntax_node_ref_is_copy() {
        fn assert_copy<T: Copy>() {}
        assert_copy::<SyntaxNode<SillyTypes, RefRoot<SillyTypes>>>()
    }
}
