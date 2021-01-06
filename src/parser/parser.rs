use super::error::PError;
use super::lexer::{Token, TokenKind};
use super::{ast::*, span::Span};

type PResult<T> = Result<T, PError>;

pub struct Parser<'a> {
    src: &'a str,
    /// Stack of tokens in reverse order
    tokens: Vec<Token>,
    token: Token,
}

type PrecPair = (i32, i32);

const ASSIGN: PrecPair = (1, 2);
const OR: PrecPair = (3, 4);
const AND: PrecPair = (5, 6);
const COMP: PrecPair = (7, 8);
const ADD: PrecPair = (9, 10);
const MUL: PrecPair = (11, 12);
const AS: PrecPair = (13, 14);
const UN: i32 = 15;
const CALL: PrecPair = (17, 18);
const FIELD: PrecPair = (19, 20);

impl TokenKind {
    fn infix_binding_power(&self) -> PrecPair {
        match self {
            TokenKind::Eq => ASSIGN,
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
            TokenKind::OpenSquare | TokenKind::OpenParen => CALL,
            TokenKind::As => AS,
            _ => (0, 0),
        }
    }

    fn cab_begin_expr(&self) -> bool {
        match self {
            TokenKind::Number
            | TokenKind::Ident
            | TokenKind::Minus
            | TokenKind::Bang
            | TokenKind::At
            | TokenKind::OpenParen
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::Return
            | TokenKind::Break
            | TokenKind::Continue => true,
            _ => false,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            src,
            tokens,
            token: Token::eof(),
        }
    }

    pub fn is_end(&self) -> bool {
        self.tokens.is_empty() && self.token.is(TokenKind::EOF)
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

    /// Entry point of parser. For now, parses only expressions
    pub fn parse(mut self) -> PResult<Vec<Item>> {
        self.bump();
        let mut items = Vec::new();
        while !self.is_end() {
            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(items)
    }

    fn parse_expr(&mut self) -> PResult<Expr> {
        self.parse_expr_with(1)
    }

    /// Parses literals
    /// self.token should be literal
    fn parse_lit(&mut self) -> PResult<Expr> {
        let tk = self.token;
        self.bump();
        Ok(Expr::new_lit(tk, tk.span))
    }

    /// Parses unary expression
    /// self.token should be operator
    fn parse_un(&mut self, op: UnOp) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump();
        let expr = self.parse_expr_with(UN)?;
        let span = lo.to(expr.span);
        Ok(Expr::new_un(span, op, expr))
    }

    /// Parses return expr
    /// self.token 'return' keyword
    fn parse_return(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump();
        let expr = self.maybe_parse_expr().transpose()?;
        let span = if let Some(ref expr) = expr {
            lo.to(expr.span)
        } else {
            lo
        };
        Ok(Expr::new_ret(span, expr))
    }

    /// Checks is self.token can begin expression and tries to parse it
    fn maybe_parse_expr(&mut self) -> Option<PResult<Expr>> {
        if self.token.kind.cab_begin_expr() {
            Some(self.parse_expr())
        } else {
            None
        }
    }

    /// Bumpes self.token and returns expression with token's span and kind
    fn parse_one_token_expr(&mut self, kind: ExprKind) -> PResult<Expr> {
        let span = self.token.span;
        self.bump();
        Ok(Expr { kind, span })
    }

    /// Dispatch and parse prefix expression
    fn parse_prefix_expr(&mut self) -> PResult<Expr> {
        match self.token.kind {
            TokenKind::Number | TokenKind::Ident => self.parse_lit(),
            TokenKind::Minus => self.parse_un(UnOp::Neg),
            TokenKind::Bang => self.parse_un(UnOp::Not),
            TokenKind::At => self.parse_un(UnOp::Deref),
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::And | TokenKind::AndAnd => self.parse_addrof(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Break => self.parse_one_token_expr(ExprKind::Break),
            TokenKind::Continue => self.parse_one_token_expr(ExprKind::Continue),
            TokenKind::OpenSquare => self.parse_array_expr(),
            _ => Err(PError::new("Expect expression", self.token.span)),
        }
    }

    /// Pratt parser for binary expressions
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

    /// Dispatch table for infix token
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
            TokenKind::Eq => self.parse_bin(BinOp::Assign, lhs, right_prec),
            TokenKind::OpenParen => self.parse_call(lhs),
            TokenKind::As => self.parse_as_expr(lhs),

            TokenKind::Dot => self.parse_field(lhs),
            TokenKind::OpenSquare => self.parse_index(lhs),
            _ => unreachable!(),
        }
    }

    /// Parses binary expression
    /// self.token should be operator
    fn parse_bin(&mut self, op: BinOp, lhs: Expr, right_prec: i32) -> PResult<Expr> {
        self.bump();
        let rhs = self.parse_expr_with(right_prec)?;
        let span = lhs.span.to(rhs.span);
        Ok(Expr::new_bin(span, op, lhs, rhs))
    }

    /// Parses 'a.b'
    /// lhs = 'a', self.token = '.'
    fn parse_field(&mut self, lhs: Expr) -> PResult<Expr> {
        self.bump(); // '.'
        let field = self.consume(TokenKind::Ident, "identifier after dot expression")?;
        let span = lhs.span.to(field.span);
        Ok(Expr::new_field(span, lhs, field))
    }

    /// Parses array expression '[a, b, c]'
    /// self.token = '['
    fn parse_array_expr(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // '['
        let (exprs, hi) = self.parse_comma_list_expr(lo, TokenKind::CloseSquare)?;
        let span = lo.to(hi);
        Ok(Expr::new_array(span, exprs))
    }

    /// Parses call expression 'a(b)'
    /// func = 'a', self.token = '('
    fn parse_call(&mut self, func: Expr) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // '('
        let (params, hi) = self.parse_comma_list_expr(lo, TokenKind::CloseParen)?;
        let span = lo.to(hi);
        Ok(Expr::new_call(func, params, span))
    }

    /// Parses list of expression possibly with trailling comma and delimeter = delim
    /// Open delimeter is already consumed
    fn parse_comma_list_expr(
        &mut self,
        mut span: Span,
        delim: TokenKind,
    ) -> PResult<(Vec<Expr>, Span)> {
        let mut res = Vec::new();

        if !self.token.is(delim) {
            loop {
                let expr = self.parse_expr()?;
                span.grow(expr.span);
                res.push(expr);
                match self.token.kind {
                    TokenKind::Comma => {
                        self.bump();
                        // Maybe the last comma was the trailling comma
                        if self.token.is(delim) {
                            break;
                        }
                    }
                    del if del == delim => break,
                    _ => return Err(PError::new("Expect ',' or delimeter", self.token.span)),
                }
            }
        }
        span.grow(self.token.span);
        self.bump();
        return Ok((res, span));
    }

    /// Parses expression in parentheses
    /// self.token = "("
    fn parse_grouping(&mut self) -> PResult<Expr> {
        self.bump(); // "("
        let expr = self.parse_expr()?;
        self.consume(TokenKind::CloseParen, ")")?;
        Ok(Expr::new_group(expr.span, expr))
    }

    /// Parses address of operator ('& expr' or '& mut expr')
    /// self.token = '&' || '&&'
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

    /// Parses index expression ('a[b]')
    /// lhs = 'a', self.token = '['
    fn parse_index(&mut self, lhs: Expr) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // '['
        let index = self.parse_expr()?;
        let span = lo.to(index.span);
        self.consume(TokenKind::CloseSquare, "]")?;
        Ok(Expr::new_index(span, lhs, index))
    }

    /// Dispatches parsing of type based of self.token
    fn parse_type(&mut self) -> PResult<Type> {
        match self.token.kind {
            TokenKind::And | TokenKind::AndAnd => self.parse_ptr_type(),
            TokenKind::Bang => {
                let span = self.token.span;
                self.bump();
                let kind = TypeKind::Never;
                Ok(Type { kind, span })
            }
            TokenKind::OpenParen => {
                let span = self.token.span;
                self.bump();
                let close = self.consume(TokenKind::CloseParen, ") as ending for void type")?;
                let span = span.to(close.span);
                let kind = TypeKind::Void;
                Ok(Type { kind, span })
            }
            TokenKind::OpenSquare => self.parse_arr_type(),
            TokenKind::Ident => {
                let tk = self.token;
                self.bump();
                let span = tk.span;
                let kind = TypeKind::Ident(tk);
                Ok(Type { kind, span })
            }
            _ => Err(PError::new("Expect type exression", self.token.span)),
        }
    }

    /// Parses pointer type ('&type' or '&mut type')
    /// self.token = '&' or '&&'
    fn parse_ptr_type(&mut self) -> PResult<Type> {
        let lo = self.token.span;
        self.eat_and()?;
        let mutab = if self.eat(TokenKind::Mut) {
            Mutability::Mut
        } else {
            Mutability::Const
        };
        let ty = self.parse_type()?;
        let span = lo.to(ty.span);
        let kind = TypeKind::Pointer(mutab, Box::new(ty));
        Ok(Type { kind, span })
    }

    /// Parses array type expression ('[type ; expr]')
    /// self.token = '['
    fn parse_arr_type(&mut self) -> PResult<Type> {
        let open = self.token.span;
        self.bump(); // '['
        let ty = self.parse_type()?;
        self.consume(TokenKind::Semi, "';' after type of array type expression")?;
        let expr = self.parse_expr()?;
        let close = self.consume(TokenKind::CloseSquare, "]")?;
        let span = open.to(close.span);
        Ok(Type::new_arr(ty, expr, span))
    }

    /// Parses 'expr AS type'
    // lhs = expr, self.token = 'as'
    fn parse_as_expr(&mut self, lhs: Expr) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // 'as'
        let ty = self.parse_type()?;
        let span = lo.to(ty.span);
        Ok(Expr::new_as(lhs, ty, span))
    }

    /// Dispather for item parsing
    fn parse_item(&mut self) -> PResult<Item> {
        match self.token.kind {
            TokenKind::Type => self.parse_type_item(),
            TokenKind::Const => self.parse_const_item(),
            _ => Err(PError::new("Expect item", self.token.span)),
        }
    }

    /// Parses type alias item ('type IDENTIFIER = Type ;')
    fn parse_type_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump(); // 'type'
        let ident = self.consume(TokenKind::Ident, "identifier in type item")?;
        self.consume(TokenKind::Eq, "'=' after identifier in type item")?;
        let ty = self.parse_type()?;
        let semi = self.consume(TokenKind::Semi, "';' after type item")?;
        let span = lo.to(semi.span);
        Ok(Item {
            ident,
            span,
            kind: ItemKind::TypeAlias(ty.into()),
        })
    }

    /// Parses const item ('const IDENTIFIER: Type = Expr ;')
    fn parse_const_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump(); // 'const'
        let ident = self.consume(TokenKind::Ident, "identifier in const item")?;
        self.consume(TokenKind::Colon, "':' after identifier in const item")?;
        let ty = self.parse_type()?;
        self.consume(TokenKind::Eq, "=")?;
        let expr = self.parse_expr()?;
        let semi = self.consume(TokenKind::Semi, "';' after item")?;
        let span = lo.to(semi.span);
        Ok(Item {
            ident,
            span,
            kind: ItemKind::Const(ty.into(), expr.into()),
        })
    }
}
