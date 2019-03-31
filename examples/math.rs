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

use rowan::{GreenNodeBuilder, SmolStr, SyntaxElement};
use std::iter::Peekable;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Token {
    Whitespace,

    Add,
    Sub,
    Mul,
    Div,
    Number,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ASTKind {
    Error,
    Operation,
    Root,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum NodeType {
    /// A marker node, will have tokens as children
    Marker(ASTKind),
    /// A raw token in the AST
    Token(Token),
}

#[derive(Debug)]
struct Types;
impl rowan::Types for Types {
    type Kind = NodeType;
    type RootData = ();
}

type Node = rowan::SyntaxNode<Types>;
type Element<'a> = rowan::SyntaxElement<'a, Types>;
type TreeArc<T> = rowan::TreeArc<Types, T>;

struct Parser<I: Iterator<Item = (Token, SmolStr)>> {
    builder: GreenNodeBuilder<Types>,
    iter: Peekable<I>,
}
impl<I: Iterator<Item = (Token, SmolStr)>> Parser<I> {
    fn peek(&mut self) -> Option<Token> {
        while self.iter.peek().map(|&(t, _)| t == Token::Whitespace).unwrap_or(false) {
            self.bump();
        }
        self.iter.peek().map(|&(t, _)| t)
    }
    fn bump(&mut self) {
        if let Some((token, string)) = self.iter.next() {
            self.builder.token(NodeType::Token(token), string);
        }
    }
    fn parse_val(&mut self) {
        match self.peek() {
            Some(Token::Number) => self.bump(),
            _ => {
                self.builder.start_node(NodeType::Marker(ASTKind::Error));
                self.bump();
                self.builder.finish_node();
            }
        }
    }
    fn handle_operation(&mut self, tokens: &[Token], next: fn(&mut Self)) {
        let checkpoint = self.builder.checkpoint();
        next(self);
        while self.peek().map(|t| tokens.contains(&t)).unwrap_or(false) {
            self.builder.start_node_at(checkpoint, NodeType::Marker(ASTKind::Operation));
            self.bump();
            next(self);
            self.builder.finish_node();
        }
    }
    fn parse_mul(&mut self) {
        self.handle_operation(&[Token::Mul, Token::Div], Self::parse_val)
    }
    fn parse_add(&mut self) {
        self.handle_operation(&[Token::Add, Token::Sub], Self::parse_mul)
    }
    fn parse(mut self) -> TreeArc<Node> {
        self.builder.start_node(NodeType::Marker(ASTKind::Root));
        self.parse_add();
        self.builder.finish_node();

        Node::new(self.builder.finish(), ())
    }
}

fn print(indent: usize, element: Element) {
    print!("{:indent$}", "", indent = indent);
    match element {
        SyntaxElement::Node(node) => {
            println!("- {:?}", node.kind());
            for child in node.children_with_tokens() {
                print(indent + 2, child);
            }
        }
        SyntaxElement::Token(token) => println!("- {:?} {:?}", token.text(), token.kind()),
    }
}

fn main() {
    let ast = Parser {
        builder: GreenNodeBuilder::new(),
        iter: vec![
            // 1 + 2 * 3 + 4
            (Token::Number, "1".into()),
            (Token::Whitespace, " ".into()),
            (Token::Add, "+".into()),
            (Token::Whitespace, " ".into()),
            (Token::Number, "2".into()),
            (Token::Whitespace, " ".into()),
            (Token::Mul, "*".into()),
            (Token::Whitespace, " ".into()),
            (Token::Number, "3".into()),
            (Token::Whitespace, " ".into()),
            (Token::Add, "+".into()),
            (Token::Whitespace, " ".into()),
            (Token::Number, "4".into()),
        ]
        .into_iter()
        .peekable(),
    }
    .parse();
    print(0, (&*ast).into());
}
