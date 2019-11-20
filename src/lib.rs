//! A generic library for lossless syntax trees.

#![forbid(unconditional_recursion, future_incompatible)]
#![warn(missing_debug_implementations, missing_docs)]
#![deny(unsafe_code)]

#[cfg(feature = "serde")]
extern crate serde_ as serde; // rename back

pub use text_unit::{TextRange, TextUnit};
