//! In this tutorial, we will write parser
//! and evaluator of arithmetic S-expressions,
//! which look like this:
//! ```
//! (+ (* 15 2) 62)
//! ```
extern crate m_lexer;
extern crate rowan;

/// Currently, rowan doesn't have a hook to add your own interner,
/// but `SmolStr` should be a "good enough" type for representing
/// tokens.
/// Additionally, rowan uses `TextUnit` and `TextRange` types to
/// represent utf8 offsets and ranges.
use rowan::SmolStr;

/// Let's start with defining all kinds of tokens and
/// composite nodes.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(non_camel_case_types)]
enum SyntaxKind {
    // tokens
    L_PAREN,    // '('
    R_PAREN,    // ')'
    ATOM,       // '+', '15'
    WHITESPACE, // whitespaces is explicit
    ERROR,      // as well as errors
    // composite nodes
    LIST, // `(+ 2 3)`
    ROOT, // top-level node: a list of s-expressions
}
/// We'll be using these a bunch, so let's add a `*` import
use SyntaxKind::*;

/// Now, let's teach `rowan` to use this type,
/// by implementing the `Types` trait.
enum STypes {}

impl rowan::Types for STypes {
    /// Each node will store a `Kind`.
    type Kind = SyntaxKind;
    /// This is the data stored in the root of the tree.
    /// Here, we'll use it to store error messages from
    /// the parser.
    type RootData = Vec<String>;
}

/// Let's define type aliases for `rowan` types, specialized
/// to `STypes`.
/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
type GreenNode = rowan::GreenNode<STypes>;

/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
type GreenNodeBuilder = rowan::GreenNodeBuilder<STypes>;

/// This is the main type this crate exports.
/// It is also immutable, like a GreenNode,
/// but it contains parent pointers, offsets, and
/// has identity semantics.
/// SyntaxNode exist in borrowed and owned flavors,
/// which is controlled by the `R` parameter.
#[allow(type_alias_bounds)]
type SyntaxNode = rowan::SyntaxNode<STypes>;

type TreePtr<T> = rowan::TreePtr<STypes, T>;

use rowan::TransparentNewType;

/// Now, let's write a parser.
/// Note that `parse` does not return a `Result`:
/// by design, syntax tree can be build even for
/// completely invalid source code.
fn parse(text: &str) -> TreePtr<Root> {
    struct Parser {
        /// input tokens, including whitespace,
        /// in *reverse* order.
        tokens: Vec<(SyntaxKind, SmolStr)>,
        /// the in-progress tree.
        builder: GreenNodeBuilder,
        /// the list of syntax errors we've accumulated
        /// so far.
        errors: Vec<String>,
    }

    enum SexpRes {
        Eof,
        RParen,
        Ok,
    }

    impl Parser {
        fn parse(mut self) -> TreePtr<Root> {
            // Make sure that the root node covers all source
            self.builder.start_internal(ROOT);
            // Parse a list of S-expressions
            loop {
                match self.sexp() {
                    SexpRes::Eof => break,
                    SexpRes::RParen => {
                        self.builder.start_internal(ERROR);
                        self.errors.push("unmatched `)`".to_string());
                        self.bump(); // be sure to chug along in case of error
                        self.builder.finish_internal();
                    }
                    SexpRes::Ok => (),
                }
            }
            // Don't forget to eat *trailing* whitespace
            self.skip_ws();
            // Close the root node.
            self.builder.finish_internal();

            // Turn the builder into a complete node.
            let green: GreenNode = self.builder.finish();
            // Construct a `SyntaxNode` from `GreenNode`,
            // using errors as the root data.
            SyntaxNode::new(green, self.errors).cast()
        }
        fn list(&mut self) {
            // Start the list node
            self.builder.start_internal(LIST);
            self.bump(); // '('
            loop {
                match self.sexp() {
                    SexpRes::Eof => {
                        self.errors.push("expected `)`".to_string());
                        break;
                    }
                    SexpRes::RParen => {
                        self.bump();
                        break;
                    }
                    SexpRes::Ok => (),
                }
            }
            // close the list node
            self.builder.finish_internal();
        }
        fn sexp(&mut self) -> SexpRes {
            // Eat leading whitespace
            self.skip_ws();
            // Either a list, and atom, a closing paren
            // or an eof.
            let t = match self.current() {
                None => return SexpRes::Eof,
                Some(R_PAREN) => return SexpRes::RParen,
                Some(t) => t,
            };
            match t {
                L_PAREN => self.list(),
                ATOM | ERROR => self.bump(),
                _ => unreachable!(),
            }
            SexpRes::Ok
        }
        fn bump(&mut self) {
            let (kind, text) = self.tokens.pop().unwrap();
            self.builder.leaf(kind, text);
        }
        fn current(&self) -> Option<SyntaxKind> {
            self.tokens.last().map(|(kind, _)| *kind)
        }
        fn skip_ws(&mut self) {
            while self.current() == Some(WHITESPACE) {
                self.bump()
            }
        }
    }

    let mut tokens = lex(text);
    tokens.reverse();
    Parser {
        tokens,
        builder: GreenNodeBuilder::new(),
        errors: Vec::new(),
    }
    .parse()
}

/// Let's check that the parser works as expected
#[test]
fn test_parser() {
    let text = "(+ (* 15 2) 62)";
    let node = parse(text);
    assert_eq!(
        format!("{:?}", node),
        "ROOT@[0; 15)", // root node, spanning 15 bytes
    );
    assert_eq!(node.children().count(), 1);
    let list = node.children().next().unwrap();
    let children = list
        .children()
        .map(|child| format!("{:?}", child))
        .collect::<Vec<_>>();

    assert_eq!(
        children,
        vec![
            "L_PAREN@[0; 1)".to_string(),
            "ATOM@[1; 2)".to_string(),
            "WHITESPACE@[2; 3)".to_string(), // note, explicit whitespace!
            "LIST@[3; 11)".to_string(),
            "WHITESPACE@[11; 12)".to_string(),
            "ATOM@[12; 14)".to_string(),
            "R_PAREN@[14; 15)".to_string(),
        ]
    );
}

/// So far, we've been working with a homogeneous untyped tree.
/// It's nice to provide generic tree operations, like traversals,
/// but it's a bad fit for semantic analysis.
/// This crate itself does not provide AST facilities directly,
/// but it is possible to layer AST on top of `SyntaxNode` API.
/// Let's write a function to evaluate S-expression.
///
/// For that, let's define AST nodes.
/// It'll be quite a bunch of repetitive code, so we'll use a macro.
///
/// For a real language, you'd want to generate an AST. I find a
/// combination of `serde`, `ron` and `tera` crates invaluable for that!
macro_rules! ast_node {
    ($ast:ident, $kind:ident) => {
        #[derive(PartialEq, Eq, Hash)]
        #[repr(transparent)]
        struct $ast(SyntaxNode);
        unsafe impl TransparentNewType for $ast {
            type Repr = SyntaxNode;
        }
        impl $ast {
            #[allow(unused)]
            fn cast(node: &SyntaxNode) -> Option<&Self> {
                if node.kind() == $kind {
                    Some(Self::from_repr(node))
                } else {
                    None
                }
            }
            #[allow(unused)]
            fn to_owned(&self) -> TreePtr<Self> {
                let owned = self.0.to_owned();
                owned.cast()
            }
        }
    };
}

ast_node!(Root, ROOT);
ast_node!(Atom, ATOM);
ast_node!(List, LIST);

// Sexp is slightly different, so let's do it by hand.
#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
struct Sexp(SyntaxNode);

enum SexpKind<'a> {
    Atom(&'a Atom),
    List(&'a List),
}

impl Sexp {
    fn cast(node: &SyntaxNode) -> Option<&Self> {
        if Atom::cast(node).is_some() || List::cast(node).is_some() {
            Some(unsafe { std::mem::transmute(node) })
        } else {
            None
        }
    }

    fn kind(&self) -> SexpKind {
        Atom::cast(&self.0)
            .map(SexpKind::Atom)
            .or_else(|| List::cast(&self.0).map(SexpKind::List))
            .unwrap()
    }
}

// Let's enhance AST nodes with ancillary functions and
// eval.
impl Root {
    fn sexps(&self) -> impl Iterator<Item = &Sexp> {
        self.0.children().filter_map(Sexp::cast)
    }
}

enum Op {
    Add,
    Sub,
    Div,
    Mul,
}

impl Atom {
    fn eval(&self) -> Option<i64> {
        self.0.leaf_text().unwrap().parse().ok()
    }
    fn as_op(&self) -> Option<Op> {
        let text = self.0.leaf_text().unwrap();
        let op = match text.as_str() {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            _ => return None,
        };
        Some(op)
    }
}

impl List {
    fn sexps(&self) -> impl Iterator<Item = &Sexp> {
        self.0.children().filter_map(Sexp::cast)
    }
    fn eval(&self) -> Option<i64> {
        let op = match self.sexps().nth(0)?.kind() {
            SexpKind::Atom(atom) => atom.as_op()?,
            _ => return None,
        };
        let arg1 = self.sexps().nth(1)?.eval()?;
        let arg2 = self.sexps().nth(2)?.eval()?;
        let res = match op {
            Op::Add => arg1 + arg2,
            Op::Sub => arg1 - arg2,
            Op::Mul => arg1 * arg2,
            Op::Div if arg2 == 0 => return None,
            Op::Div => arg1 / arg2,
        };
        Some(res)
    }
}

impl Sexp {
    fn eval(&self) -> Option<i64> {
        match self.kind() {
            SexpKind::Atom(atom) => atom.eval(),
            SexpKind::List(list) => list.eval(),
        }
    }
}

/// Let's test the eval!
fn main() {
    let sexps = "
92
(+ 62 30)
(/ 92 0)
nan
(+ (* 15 2) 62)
";
    let root = parse(sexps);
    let res = root.sexps().map(|it| it.eval()).collect::<Vec<_>>();
    eprintln!("{:?}", res);
    assert_eq!(res, vec![Some(92), Some(92), None, None, Some(92),])
}

fn lex(text: &str) -> Vec<(SyntaxKind, SmolStr)> {
    use SyntaxKind::*;

    fn tok(t: SyntaxKind) -> m_lexer::TokenKind {
        m_lexer::TokenKind(t as u16)
    }
    fn kind(t: m_lexer::TokenKind) -> SyntaxKind {
        match t.0 {
            0 => L_PAREN,
            1 => R_PAREN,
            2 => ATOM,
            3 => WHITESPACE,
            4 => ERROR,
            _ => unreachable!(),
        }
    }

    let lexer = m_lexer::LexerBuilder::new()
        .error_token(tok(ERROR))
        .tokens(&[
            (tok(L_PAREN), r"\("),
            (tok(R_PAREN), r"\)"),
            (tok(ATOM), r"[^\s()]+"),
            (tok(WHITESPACE), r"\s+"),
        ])
        .build();

    lexer
        .tokenize(text)
        .into_iter()
        .map(|t| (t.len, kind(t.kind)))
        .scan(0usize, |start_offset, (len, kind)| {
            let s: SmolStr = text[*start_offset..*start_offset + len].into();
            *start_offset += len;
            Some((kind, s))
        })
        .collect()
}
