use std::str::Chars;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
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
    Ident,

    // Keywords:
    As,
    If,
    While,
    Fn,
    Struct,
    Import,
    Const,
    Static,
    Enum,
    Mut,
    Type,
    Union,

    /// End of file
    EOF,
    /// For errors
    Unknown,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub len: usize,
}

pub struct Lexer<'a> {
    start: &'a str,
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            start: src,
            chars: src.chars(),
        }
    }

    pub fn bump(&mut self) -> char {
        self.chars.next().unwrap_or('\0')
    }

    fn bump_while(&mut self, pred: fn(char) -> bool) {
        while pred(self.first()) {
            self.bump();
        }
    }

    pub fn first(&self) -> char {
        self.chars.clone().next().unwrap_or('\0')
    }

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;
        let first = self.bump();
        let kind = match first {
            c if c.is_whitespace() => self.whitespace(),
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenSquare,
            ']' => CloseSquare,
            '+' => Plus,
            '-' => self.maybe_two_chars('>', Arrow, Minus),
            '*' => Star,
            '/' => Slash,
            ':' => self.maybe_two_chars(':', ColonColon, Colon),
            ';' => TokenKind::Semi,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '@' => TokenKind::At,
            '=' => self.maybe_two_chars('=', EqEq, Eq),
            '!' => self.maybe_two_chars('=', BangEq, Bang),
            '<' => self.maybe_two_chars('=', LE, Lt),
            '>' => self.maybe_two_chars('=', GE, Gt),
            '&' => self.maybe_two_chars('&', AndAnd, And),
            '|' => self.maybe_two_chars('|', OrOr, Or),
            '%' => TokenKind::Percent,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            '0'..='9' => self.number(),
            c if is_ident_start(c) => self.ident(),
            '\0' => TokenKind::EOF,
            _ => TokenKind::Unknown,
        };
        let len = self.advance();
        Token { kind, len }
    }

    pub fn advance(&mut self) -> usize {
        let res = self.bumped();
        self.start = self.chars.as_str();
        res
    }

    fn whitespace(&mut self) -> TokenKind {
        self.bump_while(|c| c.is_whitespace());
        TokenKind::Whitespace
    }

    fn eat(&mut self, c: char) -> bool {
        if self.first() == c {
            self.bump();
            true
        } else {
            false
        }
    }

    fn number(&mut self) -> TokenKind {
        let mut dot = false;
        loop {
            let c = self.first();
            match c {
                '0'..='9' => {}
                '.' => {
                    if !dot {
                        dot = true;
                    } else {
                        break;
                    }
                }
                _ => break,
            }
            self.bump();
        }
        TokenKind::Number
    }

    fn bumped(&self) -> usize {
        self.start.len() - self.chars.as_str().len()
    }

    fn ident(&mut self) -> TokenKind {
        self.bump_while(is_ident_body);
        let len = self.bumped();
        let ident = &self.start[..len];
        match ident {
            "as" => TokenKind::As,
            "if" => TokenKind::If,
            "while" => TokenKind::While,
            "fn" => TokenKind::Fn,
            "struct" => TokenKind::Struct,
            "import" => TokenKind::Import,
            "const" => TokenKind::Const,
            "static" => TokenKind::Static,
            "enum" => TokenKind::Enum,
            "mut" => TokenKind::Mut,
            "type" => TokenKind::Type,
            "union" => TokenKind::Union,
            _ => TokenKind::Ident,
        }
    }

    fn maybe_two_chars(&mut self, next: char, iftrue: TokenKind, iffalse: TokenKind) -> TokenKind {
        if self.eat(next) {
            iftrue
        } else {
            iffalse
        }
    }

    pub fn collect(mut self) -> Vec<Token> {
        let mut res = Vec::new();
        loop {
            let tk = self.next_token();
            if tk.kind == TokenKind::EOF {
                return res;
            }
            res.push(tk);
        }
    }
}

fn is_ident_start(c: char) -> bool {
    matches!(c, 'A'..='Z' | 'a'..='z' | '_')
}

fn is_ident_body(c: char) -> bool {
    matches!(c, 'A'..='Z' | 'a'..='z' | '_' | '0'..='9')
}

#[test]
fn basic_test() {
    use TokenKind::*;
    let input = "{+-*/:;}@ \t";
    let tokens = Lexer::new(input).collect();
    assert_eq!(
        tokens,
        vec![
            Token {
                kind: OpenBrace,
                len: 1
            },
            Token { kind: Plus, len: 1 },
            Token {
                kind: Minus,
                len: 1
            },
            Token { kind: Star, len: 1 },
            Token {
                kind: Slash,
                len: 1
            },
            Token {
                kind: Colon,
                len: 1
            },
            Token { kind: Semi, len: 1 },
            Token {
                kind: CloseBrace,
                len: 1
            },
            Token { kind: At, len: 1 },
            Token {
                kind: Whitespace,
                len: 2
            },
        ]
    );
}

#[test]
fn two_chars() {
    use TokenKind::*;
    let input = "=== <= >! !=";
    let tokens = Lexer::new(input).collect();
    assert_eq!(
        tokens,
        vec![
            Token { kind: EqEq, len: 2 },
            Token { kind: Eq, len: 1 },
            Token {
                kind: Whitespace,
                len: 1
            },
            Token { kind: LE, len: 2 },
            Token {
                kind: Whitespace,
                len: 1
            },
            Token { kind: Gt, len: 1 },
            Token { kind: Bang, len: 1 },
            Token {
                kind: Whitespace,
                len: 1
            },
            Token {
                kind: BangEq,
                len: 2
            },
        ]
    );
}

#[test]
fn test_num() {
    use TokenKind::{Dot, Number, Whitespace as Wh};
    let input = "123 1 100.123.3";
    let tokens = Lexer::new(input).collect();
    assert_eq!(
        tokens,
        vec![
            Token {
                kind: Number,
                len: 3
            },
            Token { kind: Wh, len: 1 },
            Token {
                kind: Number,
                len: 1
            },
            Token { kind: Wh, len: 1 },
            Token {
                kind: Number,
                len: 7
            },
            Token { kind: Dot, len: 1 },
            Token {
                kind: Number,
                len: 1
            },
        ]
    );
}

#[test]
fn ident_test() {
    use TokenKind::{Ident, Whitespace as Wh};
    let input = "ab ab_c __abcd A0_b1";
    let tokens = Lexer::new(input).collect();
    assert_eq!(
        tokens,
        vec![
            Token {
                kind: Ident,
                len: 2
            },
            Token { kind: Wh, len: 1 },
            Token {
                kind: Ident,
                len: 4
            },
            Token { kind: Wh, len: 1 },
            Token {
                kind: Ident,
                len: 6
            },
            Token { kind: Wh, len: 1 },
            Token {
                kind: Ident,
                len: 5
            },
        ]
    );
}

#[test]
fn keywords_test() {
    use TokenKind::*;
    let input = "if while as ident";
    let tokens: Vec<_> = Lexer::new(input)
        .collect()
        .into_iter()
        .map(|tk| tk.kind)
        .filter(|kind| !matches!(kind, Whitespace | EOF))
        .collect();
    assert_eq!(tokens, vec![If, While, As, Ident]);
}
