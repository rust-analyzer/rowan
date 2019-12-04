//! In this tutorial, we will write parser
//! and evaluator of arithmetic S-expressions,
//! which look like this:
//! ```
//! (+ (* 15 2) 62)
//! ```

/// Currently, rowan doesn't have a hook to add your own interner,
/// but `SmolStr` should be a "good enough" type for representing
/// tokens.
/// Additionally, rowan uses `TextUnit` and `TextRange` types to
/// represent utf8 offsets and ranges.
use rowan::SmolStr;

/// Let's start with defining all kinds of tokens and
/// composite nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(non_camel_case_types)]
#[repr(u16)]
enum SyntaxKind {
    L_PAREN = 0, // '('
    R_PAREN,     // ')'
    WORD,        // '+', '15'
    WHITESPACE,  // whitespaces is explicit
    ERROR,       // as well as errors

    // composite nodes
    LIST, // `(+ 2 3)`
    ATOM, // `+`, `15`, wraps a WORD token
    ROOT, // top-level node: a list of s-expressions
}
use SyntaxKind::*;

impl From<SyntaxKind> for rowan::cursor::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::cursor::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= ROOT as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::cursor::SyntaxKind {
        kind.into()
    }
}

type SyntaxNode = rowan::SyntaxNode<Lang>;
#[allow(unused)]
type SyntaxToken = rowan::SyntaxToken<Lang>;
#[allow(unused)]
type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

/// GreenNode is an immutable tree, which is cheap to change,
/// but doesn't contain offsets and parent pointers.
use rowan::GreenNode;

/// You can construct GreenNodes by hand, but a builder
/// is helpful for top-down parsers: it maintains a stack
/// of currently in-progress nodes
use rowan::GreenNodeBuilder;

/// This is the main type this crate exports.
/// It is also immutable, like a GreenNode,
/// but it contains parent pointers, offsets, and
/// has identity semantics.
/// SyntaxNode exist in borrowed and owned flavors,
/// which is controlled by the `R` parameter.

struct Parse {
    green_node: rowan::GreenNode,
    #[allow(unused)]
    errors: Vec<String>,
}

impl Parse {
    fn syntax(&self) -> Root {
        Root::cast(SyntaxNode::new_root(self.green_node.clone())).unwrap()
    }
}

/// Now, let's write a parser.
/// Note that `parse` does not return a `Result`:
/// by design, syntax tree can be build even for
/// completely invalid source code.
fn parse(text: &str) -> Parse {
    struct Parser {
        /// input tokens, including whitespace,
        /// in *reverse* order.
        tokens: Vec<(SyntaxKind, SmolStr)>,
        /// the in-progress tree.
        builder: GreenNodeBuilder<'static>,
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
        fn parse(mut self) -> Parse {
            // Make sure that the root node covers all source
            self.builder.start_node(ROOT.into());
            // Parse a list of S-expressions
            loop {
                match self.sexp() {
                    SexpRes::Eof => break,
                    SexpRes::RParen => {
                        self.builder.start_node(ERROR.into());
                        self.errors.push("unmatched `)`".to_string());
                        self.bump(); // be sure to chug along in case of error
                        self.builder.finish_node();
                    }
                    SexpRes::Ok => (),
                }
            }
            // Don't forget to eat *trailing* whitespace
            self.skip_ws();
            // Close the root node.
            self.builder.finish_node();

            // Turn the builder into a complete node.
            let green: GreenNode = self.builder.finish();
            // Construct a `SyntaxNode` from `GreenNode`,
            // using errors as the root data.
            Parse { green_node: green, errors: self.errors }
        }
        fn list(&mut self) {
            // Start the list node
            self.builder.start_node(LIST.into());
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
            self.builder.finish_node();
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
                WORD => {
                    self.builder.start_node(ATOM.into());
                    self.bump();
                    self.builder.finish_node();
                }
                ERROR => self.bump(),
                _ => unreachable!(),
            }
            SexpRes::Ok
        }
        fn bump(&mut self) {
            let (kind, text) = self.tokens.pop().unwrap();
            self.builder.token(kind.into(), text);
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
    Parser { tokens, builder: GreenNodeBuilder::new(), errors: Vec::new() }.parse()
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
    let children = list.children().map(|child| format!("{:?}", child)).collect::<Vec<_>>();

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
        impl $ast {
            #[allow(unused)]
            fn cast(node: SyntaxNode) -> Option<Self> {
                if node.kind() == $kind {
                    Some(Self(node))
                } else {
                    None
                }
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

enum SexpKind {
    Atom(Atom),
    List(List),
}

impl Sexp {
    fn cast(node: SyntaxNode) -> Option<Self> {
        if Atom::cast(node.clone()).is_some() || List::cast(node.clone()).is_some() {
            Some(Sexp(node))
        } else {
            None
        }
    }

    fn kind(&self) -> SexpKind {
        Atom::cast(self.0.clone())
            .map(SexpKind::Atom)
            .or_else(|| List::cast(self.0.clone()).map(SexpKind::List))
            .unwrap()
    }
}

// Let's enhance AST nodes with ancillary functions and
// eval.
impl Root {
    fn sexps(&self) -> impl Iterator<Item = Sexp> + '_ {
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
        self.text().parse().ok()
    }
    fn as_op(&self) -> Option<Op> {
        let op = match self.text().as_str() {
            "+" => Op::Add,
            "-" => Op::Sub,
            "*" => Op::Mul,
            "/" => Op::Div,
            _ => return None,
        };
        Some(op)
    }
    fn text(&self) -> &SmolStr {
        match &self.0.green().children().next() {
            Some(rowan::NodeOrToken::Token(token)) => token.text(),
            _ => unreachable!(),
        }
    }
}

impl List {
    fn sexps(&self) -> impl Iterator<Item = Sexp> + '_ {
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
    let res = root.syntax().sexps().map(|it| it.eval()).collect::<Vec<_>>();
    eprintln!("{:?}", res);
    assert_eq!(res, vec![Some(92), Some(92), None, None, Some(92),])
}

fn lex(text: &str) -> Vec<(SyntaxKind, SmolStr)> {
    fn tok(t: SyntaxKind) -> m_lexer::TokenKind {
        m_lexer::TokenKind(rowan::cursor::SyntaxKind::from(t).0)
    }
    fn kind(t: m_lexer::TokenKind) -> SyntaxKind {
        match t.0 {
            0 => L_PAREN,
            1 => R_PAREN,
            2 => WORD,
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
            (tok(WORD), r"[^\s()]+"),
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
