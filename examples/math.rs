//! Example that takes the input
//! 1 + 2 * 3 + 4
//! and builds the tree
//! - Marker(Root)
//!   - Marker(Operation)
//!     - Marker(Operation)
//!       - "1" Token(Number)
//!       - "+" Token(Add)
//!       - Marker(Operation)
//!         - "2" Token(Number)
//!         - "*" Token(Mul)
//!         - "3" Token(Number)
//!     - "+" Token(Add)
//!     - "4" Token(Number)

use rowan::{GreenNodeBuilder, NodeOrToken};
use std::iter::Peekable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
enum SyntaxKind {
    Whitespace = 0,
    Add = 1,
    Subtract = 2,
    Multiply = 3,
    Divide = 4,

    Number = 5,
    Error = 6,
    Operation = 7,
    Root = 8,
}

use SyntaxKind::*;

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= Root as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

struct Parser<I: Iterator<Item = (SyntaxKind, String)>> {
    builder: GreenNodeBuilder<'static>,
    iter: Peekable<I>,
}
impl<I: Iterator<Item = (SyntaxKind, String)>> Parser<I> {
    fn peek(&mut self) -> Option<SyntaxKind> {
        while self.iter.peek().map(|&(t, _)| t == Whitespace).unwrap_or(false) {
            self.bump();
        }
        self.iter.peek().map(|&(t, _)| t)
    }
    fn bump(&mut self) {
        if let Some((token, string)) = self.iter.next() {
            self.builder.token(token.into(), string.as_str());
        }
    }
    fn parse_val(&mut self) {
        match self.peek() {
            Some(Number) => self.bump(),
            _ => {
                self.builder.start_node(Error.into());
                self.bump();
                self.builder.finish_node();
            }
        }
    }
    fn handle_operation(&mut self, tokens: &[SyntaxKind], next: fn(&mut Self)) {
        let checkpoint = self.builder.checkpoint();
        next(self);
        while self.peek().map(|t| tokens.contains(&t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, Operation.into());
            self.bump();
            next(self);
            self.builder.finish_node();
        }
    }
    fn parse_mul(&mut self) {
        self.handle_operation(&[Multiply, Divide], Self::parse_val)
    }
    fn parse_add(&mut self) {
        self.handle_operation(&[Add, Subtract], Self::parse_mul)
    }
    fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(Root.into());
        self.parse_add();
        self.builder.finish_node();

        SyntaxNode::new_root(self.builder.finish())
    }
}

fn print(indent: usize, element: SyntaxElement) {
    let kind: SyntaxKind = element.kind();
    print!("{:indent$}", "", indent = indent);
    match element {
        NodeOrToken::Node(node) => {
            println!("- {:?}", kind);
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }

        NodeOrToken::Token(token) => println!("- {:?} {:?}", token.text(), kind),
    }
}

fn main() {
    let ast = Parser {
        builder: GreenNodeBuilder::new(),
        iter: vec![
            // 1 + 2 * 3 + 4
            (Number, "1".into()),
            (Whitespace, " ".into()),
            (Add, "+".into()),
            (Whitespace, " ".into()),
            (Number, "2".into()),
            (Whitespace, " ".into()),
            (Multiply, "*".into()),
            (Whitespace, " ".into()),
            (Number, "3".into()),
            (Whitespace, " ".into()),
            (Add, "+".into()),
            (Whitespace, " ".into()),
            (Number, "4".into()),
        ]
        .into_iter()
        .peekable(),
    }
    .parse();
    print(0, ast.into());
}
