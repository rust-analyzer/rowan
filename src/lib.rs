//! A generic library for lossless syntax trees.
//! See `examples/s_expressions.rs` for a tutorial.
#![forbid(
    missing_debug_implementations,
    unconditional_recursion,
    future_incompatible,
    // missing_docs
)]
#![deny(unsafe_code)]

#[allow(unsafe_code)]
mod swap_cell;
mod green;
#[allow(unsafe_code)]
mod imp;
mod syntax_token;
mod syntax_node;
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
    green::{GreenNode, GreenToken, GreenElement, GreenNodeBuilder},
    imp::SyntaxNode,
    syntax_token::SyntaxToken,
    syntax_element::SyntaxElement,
    algo::{WalkEvent, TokenAtOffset, SyntaxNodeChildren, SyntaxElementChildren},
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

pub use crate::imp::{TransparentNewType, TreeArc};

// NB: borrow requires that Eq & Hash for `Owned` are consistent with thouse for
// `Borrowed`. This is true for `TreeArc`, but for a slightly peculiar reason:
// it forces "identity" (comparisons of pointers) semantics on the contents.
impl<T, N> std::borrow::Borrow<N> for TreeArc<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>>,
{
    fn borrow(&self) -> &N {
        &*self
    }
}

impl<T, N> fmt::Debug for TreeArc<T, N>
where
    T: Types,
    N: TransparentNewType<Repr = SyntaxNode<T>> + fmt::Debug,
{
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let inner: &N = &*self;
        fmt::Debug::fmt(inner, fmt)
    }
}

// FIXME: replace with successors once it is stable
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
        type Kind = u16;
        type RootData = ();
    }

    #[test]
    fn assert_send_sync() {
        fn f<T: Send + Sync>() {}
        f::<GreenNode<SillyTypes>>();
        f::<SyntaxNode<SillyTypes>>();
        f::<TreeArc<SillyTypes, SyntaxNode<SillyTypes>>>();
    }

    #[test]
    fn test_size_of() {
        use std::mem::size_of;

        eprintln!("GreenNode    {}", size_of::<GreenNode<SillyTypes>>());
        eprintln!("GreenToken   {}", size_of::<GreenToken<SillyTypes>>());
        eprintln!("GreenElement {}", size_of::<GreenElement<SillyTypes>>());
        eprintln!();
        eprintln!("SyntaxNode    {}", size_of::<SyntaxNode<SillyTypes>>());
        eprintln!("SyntaxToken   {}", size_of::<SyntaxToken<SillyTypes>>());
        eprintln!("SyntaxElement {}", size_of::<SyntaxElement<SillyTypes>>());
    }
}
