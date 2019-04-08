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
extern crate rowan;

use rowan::{GreenNodeBuilder, SmolStr, SyntaxElement, SyntaxKind};
use std::iter::Peekable;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(u16)]
enum Kind {
    Whitespace = 0,

    Add,
    Sub,
    Mul,
    Div,
    Number,

    Error,
    Operation,
    Root,
}

impl From<SyntaxKind> for Kind {
    fn from(kind: SyntaxKind) -> Kind {
        match kind.0 {
            0 => Kind::Whitespace,

            1 => Kind::Add,
            2 => Kind::Sub,
            3 => Kind::Mul,
            4 => Kind::Div,
            5 => Kind::Number,

            6 => Kind::Error,
            7 => Kind::Operation,
            8 => Kind::Root,
            _ => unreachable!(),
        }
    }
}

impl From<Kind> for SyntaxKind {
    fn from(kind: Kind) -> SyntaxKind {
        SyntaxKind(kind as u16)
    }
}

type Node = rowan::SyntaxNode;
type Element<'a> = rowan::SyntaxElement<'a>;
type TreeArc<T> = rowan::TreeArc<T>;

struct Parser<I: Iterator<Item = (Kind, SmolStr)>> {
    builder: GreenNodeBuilder,
    iter: Peekable<I>,
}
impl<I: Iterator<Item = (Kind, SmolStr)>> Parser<I> {
    fn peek(&mut self) -> Option<Kind> {
        while self.iter.peek().map(|&(t, _)| t == Kind::Whitespace).unwrap_or(false) {
            self.bump();
        }
        self.iter.peek().map(|&(t, _)| t)
    }
    fn bump(&mut self) {
        if let Some((token, string)) = self.iter.next() {
            self.builder.token(token.into(), string);
        }
    }
    fn parse_val(&mut self) {
        match self.peek() {
            Some(Kind::Number) => self.bump(),
            _ => {
                self.builder.start_node(Kind::Error.into());
                self.bump();
                self.builder.finish_node();
            }
        }
    }
    fn handle_operation(&mut self, tokens: &[Kind], next: fn(&mut Self)) {
        let checkpoint = self.builder.checkpoint();
        next(self);
        while self.peek().map(|t| tokens.contains(&t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, Kind::Operation.into());
            self.bump();
            next(self);
            self.builder.finish_node();
        }
    }
    fn parse_mul(&mut self) {
        self.handle_operation(&[Kind::Mul, Kind::Div], Self::parse_val)
    }
    fn parse_add(&mut self) {
        self.handle_operation(&[Kind::Add, Kind::Sub], Self::parse_mul)
    }
    fn parse(mut self) -> TreeArc<Node> {
        self.builder.start_node(Kind::Root.into());
        self.parse_add();
        self.builder.finish_node();

        Node::new(self.builder.finish(), None)
    }
}

fn print(indent: usize, element: Element) {
    let kind: Kind = element.kind().into();
    print!("{:indent$}", "", indent = indent);
    match element {
        SyntaxElement::Node(node) => {
            println!("- {:?}", kind);
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }

        SyntaxElement::Token(token) => println!("- {:?} {:?}", token.text(), kind),
    }
}

fn main() {
    let ast = Parser {
        builder: GreenNodeBuilder::new(),
        iter: vec![
            // 1 + 2 * 3 + 4
            (Kind::Number, "1".into()),
            (Kind::Whitespace, " ".into()),
            (Kind::Add, "+".into()),
            (Kind::Whitespace, " ".into()),
            (Kind::Number, "2".into()),
            (Kind::Whitespace, " ".into()),
            (Kind::Mul, "*".into()),
            (Kind::Whitespace, " ".into()),
            (Kind::Number, "3".into()),
            (Kind::Whitespace, " ".into()),
            (Kind::Add, "+".into()),
            (Kind::Whitespace, " ".into()),
            (Kind::Number, "4".into()),
        ]
        .into_iter()
        .peekable(),
    }
    .parse();
    print(0, (&*ast).into());
}
