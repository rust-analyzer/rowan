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
mod green;
#[allow(unsafe_code)]
pub mod cursor;
pub mod api;
mod syntax_text;
mod utility_types;
#[cfg(feature = "serde1")]
mod serde_impls;

use green::{GreenElementRef, GreenElement, GreenChildren};

// Reexport types for working with strings. We might be too opinionated about
// these, as a custom interner might work better, but `SmolStr` is a pretty good
// default.
pub use smol_str::SmolStr;
pub use text_unit::{TextRange, TextUnit};

pub use crate::{
    api::*,
    green::{ArcGreenNode, Checkpoint, GreenNode, GreenNodeBuilder, GreenToken},
    syntax_text::SyntaxText,
    utility_types::{Direction, NodeOrToken, TokenAtOffset, WalkEvent},
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn assert_send_sync() {
        fn f<T: Send + Sync>() {}
        f::<ArcGreenNode>();
    }

    #[test]
    fn test_size_of() {
        use std::mem::size_of;

        eprintln!("ArcGreenNode      {}", size_of::<ArcGreenNode>());
        eprintln!("GreenToken        {}", size_of::<GreenToken>());
        eprintln!("OwnedGreenElement {}", size_of::<GreenElement>());
        eprintln!("GreenElementRef   {}", size_of::<GreenElementRef>());
        eprintln!();
        eprintln!("SyntaxNode    {}", size_of::<cursor::SyntaxNode>());
        eprintln!("SyntaxToken   {}", size_of::<cursor::SyntaxToken>());
        eprintln!("SyntaxElement {}", size_of::<cursor::SyntaxElement>());
    }
}
