//! A generic library for lossless syntax trees.
//! See `examples/s_expressions.rs` for a tutorial.
#![forbid(
    // missing_debug_implementations,
    unconditional_recursion,
    future_incompatible,
    // missing_docs,
)]
#![deny(unsafe_code)]
#![cfg_attr(miri, recursion_limit = "1024")] // recursive syntax tree drops

#[allow(unsafe_code)]
mod green;
#[allow(unsafe_code)]
pub mod cursor;

pub mod api;
mod syntax_text;
mod utility_types;
#[cfg(feature = "serde1")]
mod serde_impls;

pub use rc_borrow::ArcBorrow;
pub use text_size::{TextLen, TextRange, TextSize};

pub use crate::{
    api::{
        Language, SyntaxElement, SyntaxElementChildren, SyntaxNode, SyntaxNodeChildren, SyntaxToken,
    },
    green::{Checkpoint, Children, GreenNode, GreenNodeBuilder, GreenToken, NodeCache, SyntaxKind},
    syntax_text::SyntaxText,
    utility_types::{Direction, NodeOrToken, TokenAtOffset, WalkEvent},
};
