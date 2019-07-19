use crate::{
    SyntaxKind,
    cursor::{SyntaxNode, SyntaxToken},
};

impl SyntaxNode {
    pub fn ancestors(&self) -> impl Iterator<Item = SyntaxNode> {
        std::iter::successors(Some(self.clone()), SyntaxNode::parent)
    }
}
