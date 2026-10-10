mod node;
mod token;
mod element;
mod builder;
mod node_cache;

use self::element::GreenElement;

pub(crate) use self::{element::GreenElementRef, node::GreenChild};

pub use self::{
    builder::{Checkpoint, GreenNodeBuilder},
    node::{Children, GreenNode, GreenNodeData},
    node_cache::NodeCache,
    token::{GreenToken, GreenTokenData},
};

/// SyntaxKind is a type tag for each token or node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxKind(pub u16);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{cursor::SyntaxNode, TextRange};

    #[test]
    fn assert_send_sync() {
        fn f<T: Send + Sync>() {}
        f::<GreenNode>();
        f::<GreenToken>();
        f::<GreenElement>();
    }

    #[test]
    fn test_size_of() {
        use std::mem::size_of;

        eprintln!("GreenNode          {}", size_of::<GreenNode>());
        eprintln!("GreenToken         {}", size_of::<GreenToken>());
        eprintln!("GreenElement       {}", size_of::<GreenElement>());
    }

    #[test]
    fn covering_element_skips_zero_width() {
        let mut builder = GreenNodeBuilder::new();
        let root = SyntaxKind(0);
        builder.start_node(root);
        let node = SyntaxKind(1);
        builder.start_node(node);
        let ident = SyntaxKind(2);
        builder.token(ident, "x");
        let empty = SyntaxKind(3);
        builder.start_node(empty);
        builder.finish_node();
        builder.finish_node();
        let marker = SyntaxKind(4);
        builder.token(marker, "");
        let ws = SyntaxKind(5);
        builder.token_with_trivia(ident, "", [(ws, " ")], [(ws, "  ")]);
        builder.token(ident, "z");
        builder.finish_node();
        let tree = SyntaxNode::new_root(builder.finish());

        assert_eq!(tree.covering_element(TextRange::empty(1.into())).kind(), empty);
        let token = tree.covering_element(TextRange::empty(2.into())).into_token().unwrap();
        assert_eq!((token.kind(), token.text()), (ws, " "));
    }
}
