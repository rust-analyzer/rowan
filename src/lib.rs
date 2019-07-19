//! A generic library for lossless syntax trees.
//! See `examples/s_expressions.rs` for a tutorial.
#![forbid(
    // missing_debug_implementations,
    unconditional_recursion,
    future_incompatible,
    // missing_docs,
)]
#![deny(unsafe_code)]
#![allow(unused)]

#[allow(unsafe_code)]
mod swap_cell;
mod green;
#[allow(unsafe_code)]
pub mod cursor;
mod syntax_text;
mod api;
#[allow(unsafe_code)]
mod imp;
mod syntax_node;
mod syntax_token;
mod syntax_element;
mod algo;

use std::fmt;
use crate::{green::GreenIndex, imp::SyntaxIndex};

// Reexport types for working with strings.
// We might be too opinionated about these,
// as a custom interner might work better,
// but `SmolStr` is a pretty good default.
pub use smol_str::SmolStr;
pub use text_unit::{TextRange, TextUnit};

pub use crate::{
    green::{GreenNode, GreenToken, GreenElement, GreenNodeBuilder, Checkpoint},
    imp::SyntaxNode,
    syntax_token::SyntaxToken,
    syntax_element::SyntaxElement,
    algo::{WalkEvent, TokenAtOffset, SyntaxNodeChildren, SyntaxElementChildren},
    syntax_text::SyntaxText
};

/// SyntaxKind is a type tag for each token or node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxKind(pub u16);

pub use crate::imp::{TransparentNewType, TreeArc};

// NB: borrow requires that Eq & Hash for `Owned` are consistent with those for
// `Borrowed`. This is true for `TreeArc`, but for a slightly peculiar reason:
// it forces "identity" (comparisons of pointers) semantics on the contents.
impl<N> std::borrow::Borrow<N> for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode>,
{
    fn borrow(&self) -> &N {
        &*self
    }
}

impl<N> fmt::Debug for TreeArc<N>
where
    N: TransparentNewType<Repr = SyntaxNode> + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let inner: &N = &*self;
        fmt::Debug::fmt(inner, fmt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_send_sync() {
        fn f<T: Send + Sync>() {}
        f::<GreenNode>();
        f::<SyntaxNode>();
        f::<TreeArc<SyntaxNode>>();
    }

    #[test]
    fn test_size_of() {
        use std::mem::size_of;

        eprintln!("GreenNode    {}", size_of::<GreenNode>());
        eprintln!("GreenToken   {}", size_of::<GreenToken>());
        eprintln!("GreenElement {}", size_of::<GreenElement>());
        eprintln!();
        eprintln!("SyntaxNode    {}", size_of::<SyntaxNode>());
        eprintln!("SyntaxToken   {}", size_of::<SyntaxToken>());
        eprintln!("SyntaxElement {}", size_of::<SyntaxElement>());
    }
}
