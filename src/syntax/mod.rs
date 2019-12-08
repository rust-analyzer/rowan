//! The "syntax tree" is a thread-local view of the green tree
//! with parent pointers and text offsets.

pub(self) use self::free_list::FreeList;
pub use self::{
    node::{Children, Node},
    text::Text,
    token::Token,
    traits::Language,
};
use {crate::Kind, std::fmt};

mod text;
mod node;
mod token;
mod free_list;

// NB: this module is just here to make rustdoc treat all re-exported types uniformly.
// For some reason it treats re-exports ultimately from a private module differently.
mod traits {
    use super::*;
    /// Specialization information for a particular language grammar.
    ///
    /// Rather than be a purely typesystem-level trait where all functions are
    /// associated functions, this trait's methods take `self` by reference.
    /// This allows dynamic interpretation of language rules for interpreters.
    /// For the common, statically known case, you should use a ZST impl.
    ///
    /// The language resolver must be cloneable, as all node handles have one.
    /// (As such, it should also be small and quickly cloneable – `Rc` level.)
    /// If at all possible, it should be a `Copy` handle and only `usize` big.
    pub trait Language: Clone {
        /// A typed kind to identify the kind of a node in the tree.
        ///
        /// The kind must be representable as a raw `u16`,
        /// but may use an arbitrarily complex encoding if desired.
        /// This allows you to use a different kind model
        /// than rowan's fully erased storage layer.
        type Kind: fmt::Debug;

        /// Turn a typed kind into a raw kind.
        fn kind_into_raw(&self, kind: Self::Kind) -> Kind;
        /// Turn a raw kind into a typed kind.
        fn kind_from_raw(&self, kind: Kind) -> Self::Kind;

        /// Determine if a node is a token.
        ///
        /// For a node to be a token, it must be backed by a `GreenToken`.
        ///
        /// By default, all `GreenToken` nodes are considered tokens.
        /// However, it is possible to implement delayed parsing
        /// by implementing this method to check the kind instead,
        /// and allowing non-token nodes to remain as unparsed `GreenToken`
        /// nodes until their content is required to be parsed.
        ///
        /// If doing this, the root node should be stored in a cell
        /// or similar structure to allow updating it when needed.
        /// [arc-swap] explains the pattern here, though it uses
        /// atomic reference counting instead of nonatomic.
        ///
        //  NB: arc-swap actually supports `ArcSwapAny<Rc<_>>`,
        //  but this is stronger than anyone realistically needs,
        //  as it makes swapping the Rc atomic (when it's already
        //  confined to one thread, so the atomic swap is unneeded).
        //  Someone should put together an rc-swap that exposes a similar
        //  API but uses the fact that it doesn't need to be thread-safe.
        //  Until then `RefCell<Rc<_>>` is close enough.
        ///   [arc-swap]: <https://docs.rs/arc-swap/>
        fn is_token(&self, node: &Node<Self>) -> bool {
            node.green().is_token()
        }
    }
}

/// Generic treatment of language trees.
#[derive(Debug, Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Generic;
#[rustfmt::skip]
impl Language for Generic {
    type Kind = Kind;
    fn kind_into_raw(&self, kind: Kind) -> Kind { kind }
    fn kind_from_raw(&self, kind: Kind) -> Kind { kind }
}
