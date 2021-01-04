use super::ast::*;
use super::error::PError;
use super::lexer::{Token, TokenKind};

type PResult<T> = Result<T, PError>;

pub struct Parser<'a> {
    src: &'a str,
    /// Stack of tokens in reverse order
    tokens: Vec<Token>,
    idx: usize,
    token: Token,
}

type PrecPair = (i32, i32);

const OR: PrecPair = (1, 2);
const AND: PrecPair = (3, 4);
const COMP: PrecPair = (5, 6);
const ADD: PrecPair = (7, 8);
const MUL: PrecPair = (9, 10);
const AS: PrecPair = (12, 11);
const UN: i32 = 13;
const FIELD: PrecPair = (15, 16);

impl TokenKind {
    fn infix_binding_power(&self) -> PrecPair {
        match self {
            TokenKind::OrOr => OR,
            TokenKind::AndAnd => AND,
            TokenKind::LE
            | TokenKind::Lt
            | TokenKind::EqEq
            | TokenKind::GE
            | TokenKind::Gt
            | TokenKind::BangEq => COMP,
            TokenKind::Plus | TokenKind::Minus => ADD,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => MUL,
            TokenKind::Dot => FIELD,
            _ => (0, 0),
        }
    }
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
        self.tokens.is_empty()
    }

    pub fn bump(&mut self) {
        self.token = self.tokens.pop().unwrap_or(Token::eof());
    }

    fn consume(&mut self, kind: TokenKind, expect: &str) -> PResult<Token> {
        if self.token.is(kind) {
            let tk = self.token;
            self.bump();
            Ok(tk)
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
        self.parse_expr_with(1)
    }

    fn parse_lit(&mut self) -> PResult<Expr> {
        let tk = self.token;
        self.bump();
        Ok(Expr::new_lit(tk, tk.span))
    }

    fn parse_un(&mut self, op: UnOp) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump();
        let expr = self.parse_expr_with(UN)?;
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
            TokenKind::And | TokenKind::AndAnd => self.parse_addrof(),
            _ => Err(PError::new("Expect expression", self.token.span)),
        }
    }

    fn parse_expr_with(&mut self, min_prec: i32) -> PResult<Expr> {
        let mut lhs = self.parse_prefix_expr()?;

        loop {
            let (left_prec, right_prec) = self.token.kind.infix_binding_power();
            if left_prec < min_prec {
                break;
            }
            lhs = self.parse_infix(lhs, right_prec)?;
        }
        Ok(lhs)
    }

    fn parse_infix(&mut self, lhs: Expr, right_prec: i32) -> PResult<Expr> {
        match self.token.kind {
            TokenKind::Plus => self.parse_bin(BinOp::Add, lhs, right_prec),
            TokenKind::Minus => self.parse_bin(BinOp::Sub, lhs, right_prec),
            TokenKind::Star => self.parse_bin(BinOp::Mul, lhs, right_prec),
            TokenKind::Slash => self.parse_bin(BinOp::Div, lhs, right_prec),
            TokenKind::Percent => self.parse_bin(BinOp::Rem, lhs, right_prec),
            TokenKind::AndAnd => self.parse_bin(BinOp::And, lhs, right_prec),
            TokenKind::OrOr => self.parse_bin(BinOp::Or, lhs, right_prec),
            TokenKind::Lt => self.parse_bin(BinOp::Lt, lhs, right_prec),
            TokenKind::LE => self.parse_bin(BinOp::Le, lhs, right_prec),
            TokenKind::EqEq => self.parse_bin(BinOp::Eq, lhs, right_prec),
            TokenKind::Gt => self.parse_bin(BinOp::Gt, lhs, right_prec),
            TokenKind::GE => self.parse_bin(BinOp::Ge, lhs, right_prec),
            TokenKind::BangEq => self.parse_bin(BinOp::Ne, lhs, right_prec),

            TokenKind::Dot => self.parse_field(lhs, right_prec),
            _ => unimplemented!(),
        }
    }

    fn parse_bin(&mut self, op: BinOp, lhs: Expr, right_prec: i32) -> PResult<Expr> {
        self.bump();
        let rhs = self.parse_expr_with(right_prec)?;
        let span = lhs.span.to(rhs.span);
        Ok(Expr::new_bin(span, op, lhs, rhs))
    }

    fn parse_field(&mut self, lhs: Expr, _: i32) -> PResult<Expr> {
        self.bump(); // '.'
        let field = self.consume(TokenKind::Ident, "identifier after dot expression")?;
        let span = lhs.span.to(field.span);
        Ok(Expr::new_field(span, lhs, field))
    }

    fn parse_grouping(&mut self) -> PResult<Expr> {
        self.bump(); // "("
        let expr = self.parse_expr()?;
        self.consume(TokenKind::CloseParen, ")")?;
        Ok(Expr::new_group(expr.span, expr))
    }

    fn parse_addrof(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.eat_and()?;
        let mutab = if self.eat(TokenKind::Mut) {
            Mutability::Mut
        } else {
            Mutability::Const
        };
        let expr = self.parse_expr_with(UN)?;
        let span = lo.to(expr.span);
        Ok(Expr::new_addr_of(span, mutab, expr))
    }

    // Eats '&' possibly breaking '&&' if present
    fn eat_and(&mut self) -> PResult<()> {
        if self.token.is(TokenKind::AndAnd) {
            let (_, hi) = self.token.span.split(1);
            self.token.kind = TokenKind::And;
            self.token.span = hi;
        } else {
            self.consume(TokenKind::And, "&")?;
        }
        Ok(())
    }
}
