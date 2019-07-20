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

mod green;
#[allow(unsafe_code)]
pub mod cursor;
pub mod api;
mod syntax_text;
mod algo;

// Reexport types for working with strings. We might be too opinionated about
// these, as a custom interner might work better, but `SmolStr` is a pretty good
// default.
pub use smol_str::SmolStr;
pub use text_unit::{TextRange, TextUnit};

pub use crate::{
    algo::{TokenAtOffset, WalkEvent},
    green::{Checkpoint, GreenElement, GreenNode, GreenNodeBuilder, GreenToken},
    syntax_text::SyntaxText,
    api::*,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_send_sync() {
        fn f<T: Send + Sync>() {}
        f::<GreenNode>();
    }

    #[test]
    fn test_size_of() {
        use std::mem::size_of;

        eprintln!("GreenNode    {}", size_of::<GreenNode>());
        eprintln!("GreenToken   {}", size_of::<GreenToken>());
        eprintln!("GreenElement {}", size_of::<GreenElement>());
        eprintln!();
        eprintln!("SyntaxNode    {}", size_of::<cursor::SyntaxNode>());
        eprintln!("SyntaxToken   {}", size_of::<cursor::SyntaxToken>());
        eprintln!("SyntaxElement {}", size_of::<cursor::SyntaxElement>());
    }
}
