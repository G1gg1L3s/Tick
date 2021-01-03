mod low_lexer;
pub use low_lexer::{Lexer as LLexer, TokenKind};

use super::{error::PError, span::Span};

#[derive(Debug, Clone, Copy)]
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
                TokenKind::Unknown => {
                    let msg = format!("Unknown token: '{}'", span.extract(self.src));
                    self.errors.push(PError::new(msg, span));
                }
                TokenKind::Whitespace => {}
                TokenKind::EOF => {
                    break;
                }
                kind => {
                    self.tokens.push(Token { span, kind });
                }
            }
            start = end;
        }

        (self.tokens, self.errors)
    }
}
