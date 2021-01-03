use super::ast::*;
use super::error::PError;
use super::lexer::{Token, TokenKind};

type PResult<T> = Result<T, PError>;

pub struct Parser<'a> {
    src: &'a str,
    tokens: Vec<Token>,
    idx: usize,
    token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            src,
            tokens,
            idx: 0,
            token: Token::eof(),
        }
    }

    pub fn is_end(&self) -> bool {
        self.idx == self.tokens.len()
    }

    pub fn bump(&mut self) {
        self.token = *self.tokens.get(self.idx).unwrap_or(&Token::eof());
        self.idx += 1;
    }

    fn consume(&mut self, kind: TokenKind, expect: &str) -> PResult<()> {
        if self.token.is(kind) {
            self.bump();
            Ok(())
        } else {
            Err(PError::new(format!("Expect {}", expect), self.token.span))
        }
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        if self.token.is(kind) {
            self.bump();
            true
        } else {
            false
        }
    }

    pub fn parse(mut self) -> PResult<Expr> {
        self.bump();
        self.parse_expr()
    }

    fn parse_expr(&mut self) -> PResult<Expr> {
        self.parse_expr_with(0)
    }

    fn parse_lit(&mut self) -> PResult<Expr> {
        let tk = self.token;
        self.bump();
        Ok(Expr::new_lit(tk, tk.span))
    }

    fn parse_un(&mut self, op: UnOp) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump();
        let expr = self.parse_prefix_expr()?;
        let span = lo.to(expr.span);
        Ok(Expr::new_un(span, op, expr))
    }

    fn parse_prefix_expr(&mut self) -> PResult<Expr> {
        match self.token.kind {
            TokenKind::Number | TokenKind::Ident => self.parse_lit(),
            TokenKind::Minus => self.parse_un(UnOp::Neg),
            TokenKind::Bang => self.parse_un(UnOp::Not),
            TokenKind::At => self.parse_un(UnOp::Deref),
            TokenKind::OpenParen => self.parse_grouping(),
            _ => Err(PError::new("Expect expression", self.token.span)),
        }
    }

    fn parse_expr_with(&mut self, min_prec: usize) -> PResult<Expr> {
        let lhs = self.parse_prefix_expr()?;
        Ok(lhs)
    }

    fn parse_grouping(&mut self) -> PResult<Expr> {
        self.consume(TokenKind::OpenParen, "(")?;
        let expr = self.parse_expr()?;
        self.consume(TokenKind::CloseParen, ")")?;
        Ok(Expr::new_group(expr.span, expr))
    }
}
