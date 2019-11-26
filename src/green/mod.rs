//! The "green tree" is the immutable, atomically reference counted tree.
//!
//! Leaf tokens are [`GreenToken`] and non-leaf nodes are [`GreenNode`].
//! Both of these types are unsized and are only used behind references.
//! All tree nodes are aggressively reference counted and reused,
//! to minimize space taken up by the immutable tree and to maximize sharing.

mod token;
mod element;
mod node;
mod builder;

pub(crate) use element::GreenElement;
pub use {node::GreenNode, token::GreenToken, builder::*};
