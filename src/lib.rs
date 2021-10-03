//! A generic library for lossless syntax trees.
//! See `examples/s_expressions.rs` for a tutorial.
#![forbid(
    // missing_debug_implementations,
    unconditional_recursion,
    future_incompatible,
    // missing_docs,
)]
#![deny(unsafe_code)]

#[allow(unsafe_code)]
mod green;
#[allow(unsafe_code)]
pub mod cursor;

pub mod api;
mod syntax_text;
mod utility_types;

mod cow_mut;
#[allow(unsafe_code)]
mod sll;
#[allow(unsafe_code)]
mod arc;
#[cfg(feature = "serde1")]
mod serde_impls;

pub use text_size::{TextLen, TextRange, TextSize};

pub use crate::{
    api::{
        Language, SyntaxElement, SyntaxElementChildren, SyntaxNode, SyntaxNodeChildren,
        SyntaxNodeChildrenMatching, SyntaxToken,
    },
    green::{
        Checkpoint, Children, GreenNode, GreenNodeBuilder, GreenNodeData, GreenToken,
        GreenTokenData, SyntaxKind,
    },
    syntax_text::SyntaxText,
    utility_types::{Direction, NodeOrToken, TokenAtOffset, WalkEvent},
};
