#![feature(
    alloc_layout_extra,
    hash_raw_entry,
    hash_set_entry,
    manually_drop_take,
    slice_from_raw_parts,
    trusted_len
)]

//! A generic library for lossless syntax trees.

#![forbid(unconditional_recursion, future_incompatible)]
#![warn(missing_debug_implementations, missing_docs)]

#[cfg(feature = "serde")]
extern crate serde_ as serde; // rename back

mod helpers;

pub mod green;
pub mod syntax;

#[doc(inline)]
pub use crate::helpers::{Direction, NodeOrToken, WalkEvent};
#[doc(no_inline)] // Explicitly ask for a "Re-exports" header.
pub use {
    crate::{
        green::{GreenNode, GreenToken},
        syntax::{Language, Node, Text, Token},
    },
    rc_borrow::ArcBorrow,
    str_index::{StrIndex, StrRange},
};

/// Raw kind tag for each token or node in the tree.
#[repr(transparent)]
#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Kind(pub u16);
