//! A generic library for lossless syntax trees.
//! See `examples/s_expressions.rs` for a tutorial.
#![no_std]
#![forbid(
    // missing_debug_implementations,
    unconditional_recursion,
    future_incompatible,
    // missing_docs,
)]
#![deny(unsafe_code)]

extern crate alloc;
#[cfg(test)]
extern crate std;

#[allow(unsafe_code)]
mod green;
#[allow(unsafe_code)]
pub mod cursor;

pub mod api;
mod countme;
mod syntax_text;
mod text_size;
mod utility_types;

mod cow_mut;
#[allow(unsafe_code)]
mod sll;
#[allow(unsafe_code)]
mod arc;
#[cfg(feature = "serde1")]
mod serde_impls;
pub mod ast;

pub use crate::text_size::{TextLen, TextRange, TextSize};

pub use crate::{
    api::{
        Language, SyntaxElement, SyntaxElementChildren, SyntaxNode, SyntaxNodeChildren, SyntaxToken,
    },
    green::{
        Checkpoint, Children, GreenNode, GreenNodeBuilder, GreenNodeData, GreenToken,
        GreenTokenData, NodeCache, SyntaxKind,
    },
    syntax_text::SyntaxText,
    utility_types::{Direction, NodeOrToken, TokenAtOffset, WalkEvent},
};
