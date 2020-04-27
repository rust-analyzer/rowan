mod node;
mod token;
mod element;
mod builder;

pub(crate) use self::element::{GreenElement, GreenElementRef};
pub use self::{
    builder::{Checkpoint, GreenNodeBuilder, NodeCache},
    node::{Children, GreenNode},
    token::GreenToken,
};

/// SyntaxKind is a type tag for each token or node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxKind(pub u16);
