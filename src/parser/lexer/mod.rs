mod low_lexer;
pub use low_lexer::{Lexer as LLexer, TokenKind as LKind};

use super::symbol::{self, symbols, Symbol};
use super::{error::PError, span::Span};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// ":"
    Colon,
    /// "::"
    ColonColon,
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "@"
    At,
    /// "="
    Eq,
    /// "=="
    EqEq,
    /// "!"
    Bang,
    /// "!="
    BangEq,
    /// "<"
    Lt,
    /// "<="
    LE,
    /// ">"
    Gt,
    /// ">="
    GE,
    /// "&"
    And,
    /// "&&"
    AndAnd,
    /// "|"
    Or,
    /// "||"
    OrOr,
    /// "%"
    Percent,
    /// "->"
    Arrow,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// " \t\n"
    Whitespace,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,

    /// "["
    OpenSquare,
    /// "]"
    CloseSquare,

    Number,
    /// Any identifier
    Ident(Symbol),

    // Keywords:
    Kw(Symbol),

    /// End of file
    EOF,
    /// For errors
    Unknown,
}

impl From<LKind> for TokenKind {
    fn from(lkind: LKind) -> Self {
        match lkind {
            LKind::Plus => TokenKind::Plus,
            LKind::Minus => TokenKind::Minus,
            LKind::Star => TokenKind::Star,
            LKind::Slash => TokenKind::Slash,
            LKind::Colon => TokenKind::Colon,
            LKind::ColonColon => TokenKind::ColonColon,
            LKind::Semi => TokenKind::Semi,
            LKind::Comma => TokenKind::Comma,
            LKind::Dot => TokenKind::Dot,
            LKind::At => TokenKind::At,
            LKind::Eq => TokenKind::Eq,
            LKind::EqEq => TokenKind::EqEq,
            LKind::Bang => TokenKind::Bang,
            LKind::BangEq => TokenKind::BangEq,
            LKind::Lt => TokenKind::Lt,
            LKind::LE => TokenKind::LE,
            LKind::Gt => TokenKind::Gt,
            LKind::GE => TokenKind::GE,
            LKind::And => TokenKind::And,
            LKind::AndAnd => TokenKind::AndAnd,
            LKind::Or => TokenKind::Or,
            LKind::OrOr => TokenKind::OrOr,
            LKind::Percent => TokenKind::Percent,
            LKind::Arrow => TokenKind::Arrow,
            LKind::OpenParen => TokenKind::OpenParen,
            LKind::CloseParen => TokenKind::CloseParen,
            LKind::Whitespace => TokenKind::Whitespace,
            LKind::OpenBrace => TokenKind::OpenBrace,
            LKind::CloseBrace => TokenKind::CloseBrace,
            LKind::OpenSquare => TokenKind::OpenSquare,
            LKind::CloseSquare => TokenKind::CloseSquare,
            LKind::Number => TokenKind::Number,
            LKind::EOF => TokenKind::EOF,
            LKind::Unknown => TokenKind::Unknown,
            LKind::As => TokenKind::Kw(symbols::AS),
            LKind::If => TokenKind::Kw(symbols::IF),
            LKind::Else => TokenKind::Kw(symbols::ELSE),
            LKind::While => TokenKind::Kw(symbols::WHILE),
            LKind::Loop => TokenKind::Kw(symbols::LOOP),
            LKind::Fn => TokenKind::Kw(symbols::FN),
            LKind::Struct => TokenKind::Kw(symbols::STRUCT),
            LKind::Import => TokenKind::Kw(symbols::IMPORT),
            LKind::Const => TokenKind::Kw(symbols::CONST),
            LKind::Static => TokenKind::Kw(symbols::STATIC),
            LKind::Enum => TokenKind::Kw(symbols::ENUM),
            LKind::Mut => TokenKind::Kw(symbols::MUT),
            LKind::Type => TokenKind::Kw(symbols::TYPE),
            LKind::Return => TokenKind::Kw(symbols::RETURN),
            LKind::Break => TokenKind::Kw(symbols::BREAK),
            LKind::Continue => TokenKind::Kw(symbols::CONTINUE),
            LKind::Let => TokenKind::Kw(symbols::LET),
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub span: Span,
    pub kind: TokenKind,
}

impl Token {
    pub fn is(&self, kind: TokenKind) -> bool {
        self.kind == kind
    }
    pub fn eof() -> Self {
        Self {
            span: Span::new(0, 0),
            kind: TokenKind::EOF,
        }
    }
}

/// High-level lexer. Uses low-level one to get all the tokens,
/// sieve errors and returns them along with the valid tokens with span info
pub struct Lexer<'a> {
    src: &'a str,
    tokens: Vec<Token>,
    errors: Vec<PError>,
    lexer: LLexer<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            src: input,
            tokens: Vec::new(),
            errors: Vec::new(),
            lexer: LLexer::new(input),
        }
    }

    pub fn tokenize(mut self) -> (Vec<Token>, Vec<PError>) {
        let mut start = 0;
        loop {
            let tk = self.lexer.next_token();
            let end = start + tk.len;
            let span = Span::new(start, end);
            match tk.kind {
                LKind::Unknown => {
                    let msg = format!("Unknown token: '{}'", span.extract(self.src));
                    self.errors.push(PError::new(msg, span));
                }
                LKind::Whitespace => {}
                kind => {
                    let kind = if let LKind::Ident = kind {
                        let str = span.extract(self.src);
                        let symb = Symbol::from(str);
                        TokenKind::Ident(symb)
                    } else {
                        kind.into()
                    };
                    self.tokens.push(Token { span, kind });
                    if kind == TokenKind::EOF {
                        break;
                    }
                }
            }
            start = end;
        }
        self.tokens.reverse();
        (self.tokens, self.errors)
    }
}
