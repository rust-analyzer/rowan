#![feature(alloc_layout_extra, hash_raw_entry, hash_set_entry, slice_from_raw_parts, trusted_len)]

//! A generic library for lossless syntax trees.

#![forbid(unconditional_recursion, future_incompatible)]
#![warn(missing_debug_implementations, missing_docs)]
#![deny(unsafe_code)]

#[cfg(feature = "serde")]
extern crate serde_ as serde; // rename back

mod helpers;

#[allow(unsafe_code)]
pub mod green;

pub use {
    crate::{
        green::{GreenNode, GreenToken},
        helpers::NodeOrToken,
    },
    rc_borrow::ArcBorrow,
    text_unit::{TextRange, TextUnit},
};

/// A kind tag for each token or node in the tree.
#[repr(transparent)]
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Kind(pub u16);
