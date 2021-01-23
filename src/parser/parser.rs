use super::ast::*;
use super::error::PError;
use super::lexer::{Token, TokenKind};
use crate::symbol::symbols as sm;

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
            TokenKind::Kw(sm::AS) => AS,
            _ => (0, 0),
        }
    }

    fn can_begin_expr(&self) -> bool {
        match self {
            TokenKind::Number
            | TokenKind::Ident(..)
            | TokenKind::String(..)
            | TokenKind::Minus
            | TokenKind::Bang
            | TokenKind::At
            | TokenKind::OpenParen
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::Kw(sm::RETURN)
            | TokenKind::Kw(sm::BREAK)
            | TokenKind::Kw(sm::CONTINUE) => true,
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

    pub fn check(&self, kind: TokenKind, expect: &str) -> PResult<()> {
        if !self.token.is(kind) {
            Err(PError::new(format!("Expect {}", expect), self.token.span))
        } else {
            Ok(())
        }
    }

    pub fn bump(&mut self) {
        let old = self.token;
        self.token = self.tokens.pop().unwrap_or(old);
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

    fn consume_id(&mut self, expect: &str) -> PResult<Ident> {
        match self.token.kind {
            TokenKind::Ident(ident) => {
                let span = self.token.span;
                self.bump();
                Ok(Ident { ident, span })
            }
            _ => Err(PError::new(format!("Expect {}", expect), self.token.span)),
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
    pub fn parse(mut self) -> PResult<Module> {
        self.bump();
        let mut items = Vec::new();
        while !self.is_end() {
            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(Module { items })
    }

    fn parse_expr(&mut self) -> PResult<Expr> {
        self.parse_expr_with(1)
    }

    /// Parses literals
    /// self.token should be literal
    fn parse_lit(&mut self) -> PResult<Expr> {
        let tk = self.token;
        self.bump();
        Ok(Expr::dummy_lit(tk, tk.span))
    }

    /// Parses unary expression
    /// self.token should be operator
    fn parse_un(&mut self, op: UnOp) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump();
        let expr = self.parse_expr_with(UN)?;
        let span = lo.to(expr.span);
        Ok(Expr::dummy_un(span, op, expr))
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
        Ok(Expr::dummy_ret(span, expr))
    }

    /// Checks is self.token can begin expression and tries to parse it
    fn maybe_parse_expr(&mut self) -> Option<PResult<Expr>> {
        if self.token.kind.can_begin_expr() {
            Some(self.parse_expr())
        } else {
            None
        }
    }

    /// Bumpes self.token and returns expression with token's span and kind
    fn parse_one_token_expr(&mut self, kind: ExprKind) -> PResult<Expr> {
        let span = self.token.span;
        self.bump();
        Ok(Expr::new_dummy(kind, span))
    }

    /// Dispatch and parse prefix expression
    fn parse_prefix_expr(&mut self) -> PResult<Expr> {
        match self.token.kind {
            TokenKind::Number | TokenKind::Ident(..) | TokenKind::String(..) => self.parse_lit(),
            TokenKind::Minus => self.parse_un(UnOp::Neg),
            TokenKind::Bang => self.parse_un(UnOp::Not),
            TokenKind::At => self.parse_un(UnOp::Deref),
            TokenKind::OpenParen => self.parse_grouping(),
            TokenKind::And | TokenKind::AndAnd => self.parse_addrof(),
            TokenKind::Kw(sm::RETURN) => self.parse_return(),
            TokenKind::Kw(sm::BREAK) => self.parse_one_token_expr(ExprKind::Break),
            TokenKind::Kw(sm::CONTINUE) => self.parse_one_token_expr(ExprKind::Continue),
            TokenKind::OpenSquare => self.parse_array_expr(),
            TokenKind::OpenBrace => self.parse_block_expr(),
            TokenKind::Kw(sm::IF) => self.parse_if_expr(),
            TokenKind::Kw(sm::WHILE) => self.parse_while_expr(),
            TokenKind::Kw(sm::LOOP) => self.parse_loop_expr(),
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
            TokenKind::Kw(sm::AS) => self.parse_as_expr(lhs),

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
        Ok(Expr::dummy_bin(span, op, lhs, rhs))
    }

    /// Parses 'a.b'
    /// lhs = 'a', self.token = '.'
    fn parse_field(&mut self, lhs: Expr) -> PResult<Expr> {
        self.bump(); // '.'
        let field = self.consume_id("identifier after dot expression")?;
        let span = lhs.span.to(field.span);
        Ok(Expr::dummy_field(span, lhs, field))
    }

    /// Parses array expression '[a, b, c]'
    /// self.token = '['
    fn parse_array_expr(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // '['
        let exprs = self.parse_comma_list_expr(TokenKind::CloseSquare)?;
        let close = self.consume(TokenKind::CloseSquare, "']' after list")?;
        let span = lo.to(close.span);
        Ok(Expr::dummy_array(span, exprs))
    }

    /// Parses call expression 'a(b)'
    /// func = 'a', self.token = '('
    fn parse_call(&mut self, func: Expr) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // '('
        let params = self.parse_comma_list_expr(TokenKind::CloseParen)?;
        let close = self.consume(TokenKind::CloseParen, "')' after function call")?;
        let span = lo.to(close.span);
        Ok(Expr::dummy_call(func, params, span))
    }
    /// Same as parse_comma_list but specifically for list of expressions
    fn parse_comma_list_expr(&mut self, delim: TokenKind) -> PResult<Vec<Expr>> {
        self.parse_comma_list(delim, |this| this.parse_expr())
    }

    /// Parses expression in parentheses
    /// self.token = "("
    fn parse_grouping(&mut self) -> PResult<Expr> {
        self.bump(); // "("
        let expr = self.parse_expr()?;
        self.consume(TokenKind::CloseParen, ")")?;
        Ok(Expr::dummy_group(expr.span, expr))
    }

    /// Parses address of operator ('& expr' or '& mut expr')
    /// self.token = '&' || '&&'
    fn parse_addrof(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.eat_and()?;
        let mutab = if self.eat(TokenKind::Kw(sm::MUT)) {
            Mutability::Mut
        } else {
            Mutability::Const
        };
        let expr = self.parse_expr_with(UN)?;
        let span = lo.to(expr.span);
        Ok(Expr::dummy_addr_of(span, mutab, expr))
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
        Ok(Expr::dummy_index(span, lhs, index))
    }

    /// Dispatches parsing of type based of self.token
    fn parse_type(&mut self) -> PResult<Type> {
        match self.token.kind {
            TokenKind::And | TokenKind::AndAnd => self.parse_ptr_type(),
            TokenKind::Bang => {
                let span = self.token.span;
                self.bump();
                let kind = TypeKind::Never;
                Ok(Type::new_dummy(kind, span))
            }
            TokenKind::OpenParen => {
                let span = self.token.span;
                self.bump();
                let close = self.consume(TokenKind::CloseParen, ") as ending for void type")?;
                let span = span.to(close.span);
                let kind = TypeKind::Void;
                Ok(Type::new_dummy(kind, span))
            }
            TokenKind::OpenSquare => self.parse_arr_type(),
            TokenKind::Ident(ident) => {
                let span = self.token.span;
                let ident = Ident { ident, span };
                self.bump();
                let kind = TypeKind::Ident(ident);
                Ok(Type::new_dummy(kind, span))
            }
            _ => Err(PError::new("Expect type exression", self.token.span)),
        }
    }

    /// Parses pointer type ('&type' or '&mut type')
    /// self.token = '&' or '&&'
    fn parse_ptr_type(&mut self) -> PResult<Type> {
        let lo = self.token.span;
        self.eat_and()?;
        let mutab = if self.eat(TokenKind::Kw(sm::MUT)) {
            Mutability::Mut
        } else {
            Mutability::Const
        };
        let ty = self.parse_type()?;
        let span = lo.to(ty.span);
        let kind = TypeKind::Pointer(mutab, Box::new(ty));
        Ok(Type::new_dummy(kind, span))
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
        Ok(Type::dummy_arr(ty, expr, span))
    }

    /// Parses 'expr AS type'
    // lhs = expr, self.token = 'as'
    fn parse_as_expr(&mut self, lhs: Expr) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // 'as'
        let ty = self.parse_type()?;
        let span = lo.to(ty.span);
        Ok(Expr::dummy_as(lhs, ty, span))
    }

    /// Dispather for item parsing
    fn parse_item(&mut self) -> PResult<Item> {
        match self.token.kind {
            TokenKind::Kw(sm::TYPE) => self.parse_type_item(),
            TokenKind::Kw(sm::CONST) => self.parse_const_item(),
            TokenKind::Kw(sm::STATIC) => self.parse_static_item(),
            TokenKind::Kw(sm::ENUM) => self.parse_enum_item(),
            TokenKind::Kw(sm::IMPORT) => self.parse_import_item(),
            TokenKind::Kw(sm::STRUCT) => self.parse_struct_item(),
            TokenKind::Kw(sm::FN) => self.parse_fn_item(),
            _ => Err(PError::new("Expect item", self.token.span)),
        }
    }

    /// Parses import item ('import "string literal" ;')
    fn parse_import_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump(); // 'import'
        let ident = if let TokenKind::String(ident) = self.token.kind {
            let span = self.token.span;
            self.bump();
            Ok(Ident { ident, span })
        } else {
            Err(PError::new("expect string literal", self.token.span))
        }?;
        let end = self.consume(TokenKind::Semi, "';' after import item")?;
        let span = lo.to(end.span);
        Ok(Item::new_dummy(ident, ItemKind::Import, span))
    }

    /// Parses type alias item ('type IDENTIFIER = Type ;')
    fn parse_type_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump(); // 'type'
        let ident = self.consume_id("identifier in type item")?;
        self.consume(TokenKind::Eq, "'=' after identifier in type item")?;
        let ty = self.parse_type()?;
        let semi = self.consume(TokenKind::Semi, "';' after type item")?;
        let span = lo.to(semi.span);
        Ok(Item::new_dummy(ident, ItemKind::TypeAlias(ty.into()), span))
    }

    /// Parses const item ('const IDENTIFIER: Type = Expr ;')
    /// self.token = 'const'
    fn parse_const_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump(); // 'const'
        let ident = self.consume_id("identifier in const item")?;
        let (ty, expr) = self.parse_anon_item()?;
        let semi = self.consume(TokenKind::Semi, "';' after item")?;
        let span = lo.to(semi.span);
        Ok(Item::new_dummy(
            ident,
            ItemKind::Const(ty.into(), expr.into()),
            span,
        ))
    }

    /// Parses ': type = expr'
    fn parse_anon_item(&mut self) -> PResult<(Type, Expr)> {
        self.consume(TokenKind::Colon, "':'")?;
        let ty = self.parse_type()?;
        self.consume(TokenKind::Eq, "=")?;
        let expr = self.parse_expr()?;
        Ok((ty, expr))
    }

    /// Parses static item ('static mut? IDENTIFIER: Type = Expr ;')
    /// self.token = 'static'
    fn parse_static_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump(); // 'static'
        let mutab = if self.eat(TokenKind::Kw(sm::MUT)) {
            Mutability::Mut
        } else {
            Mutability::Const
        };
        let ident = self.consume_id("identifier in static item")?;
        let (ty, expr) = self.parse_anon_item()?;
        let semi = self.consume(TokenKind::Semi, "';' after item")?;
        let span = lo.to(semi.span);
        Ok(Item::new_dummy(
            ident,
            ItemKind::Static(mutab, ty.into(), expr.into()),
            span,
        ))
    }

    /// Parses enum item: 'enum IDENTIFIER { (IDENT,) * }'
    fn parse_enum_item(&mut self) -> PResult<Item> {
        use TokenKind::{CloseBrace, OpenBrace};
        let lo = self.token.span;
        self.bump(); // 'enum'
        let ident = self.consume_id("name of enum")?;
        self.consume(OpenBrace, "'{' after enum keyword")?;
        let enums = self.parse_comma_list(CloseBrace, |this| {
            this.consume_id("Identifier in enum item")
        })?;
        let close = self.consume(CloseBrace, "'}'")?;
        let span = lo.to(close.span);
        Ok(Item::new_dummy(ident, ItemKind::Enum(enums), span))
    }

    /// Parses struct item ('struct IDENTIFIER { (IDENTIFIER : TYPE , )* }')
    /// self.token = 'struct'
    fn parse_struct_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump();
        let ident = self.consume_id("identifier")?;
        self.consume(TokenKind::OpenBrace, "'{' after struct name")?;
        let fields =
            self.parse_comma_list(TokenKind::CloseBrace, |this| this.parse_ident_type_pair())?;
        let close = self.consume(TokenKind::CloseBrace, "'}' after struct fields definition")?;
        let span = lo.to(close.span);
        Ok(Item::new_dummy(ident, ItemKind::Struct(fields), span))
    }

    /// Parses function item ('fn IDENTIFIER ( (Param,)* ) block')
    /// self.token = 'fn'
    fn parse_fn_item(&mut self) -> PResult<Item> {
        let lo = self.token.span;
        self.bump(); // 'fn'
        let ident = self.consume_id("identifier")?;
        self.consume(TokenKind::OpenParen, "'(' after function name")?;
        let params =
            self.parse_comma_list(TokenKind::CloseParen, |this| this.parse_ident_type_pair())?;
        self.consume(TokenKind::CloseParen, "')' after function params")?;
        let returns = if self.eat(TokenKind::Arrow) {
            let ty = self.parse_type()?;
            Some(ty)
        } else {
            None
        };
        if !self.token.is(TokenKind::OpenBrace) {
            return Err(PError::new(
                "Expect block after function params",
                self.token.span,
            ));
        }
        let block = self.parse_block()?;
        let span = lo.to(block.span);
        let sig = FnSignature { params, returns };
        Ok(Item::new_dummy(
            ident,
            ItemKind::Fn(sig.into(), block.into()),
            span,
        ))
    }

    /// Parses 'ident : type'
    fn parse_ident_type_pair(&mut self) -> PResult<IdentTypePair> {
        let ident = self.consume_id("identifier")?;
        let lo = ident.span;
        self.consume(TokenKind::Colon, "':'")?;
        let ty = self.parse_type()?;
        let span = lo.to(ty.span);
        Ok(IdentTypePair { ident, ty, span })
    }

    /// Parses comma separated list (maybe with trailling comma) with callback
    /// Open delimeter should be consumed
    /// Doesn't consume the close delimeter
    fn parse_comma_list<T>(
        &mut self,
        delim: TokenKind,
        mut f: impl FnMut(&mut Parser<'a>) -> PResult<T>,
    ) -> PResult<Vec<T>> {
        let mut res = Vec::new();

        if !self.token.is(delim) {
            loop {
                let next = f(self)?;
                res.push(next);
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
        Ok(res)
    }

    /// Parses block and wraps it into expression
    pub fn parse_block_expr(&mut self) -> PResult<Expr> {
        let block = self.parse_block()?;
        let span = block.span;
        let kind = ExprKind::Block(block.into());
        Ok(Expr::new_dummy(kind, span))
    }

    /// Parses block '{ statement* }'
    /// self.token = '{'
    pub fn parse_block(&mut self) -> PResult<Block> {
        let lo = self.token.span;
        self.bump(); // '{'
        let mut stmts = Vec::new();
        while !self.token.is(TokenKind::CloseBrace) {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }
        let span = lo.to(self.token.span);
        self.consume(TokenKind::CloseBrace, "'}'")?;
        Ok(Block { stmts, span })
    }

    /// Dispatch parsing of statement
    pub fn parse_stmt(&mut self) -> PResult<Stmt> {
        let lo = self.token.span;
        match self.token.kind {
            TokenKind::Kw(sm::LET) => self.parse_let_stmt(),
            TokenKind::Semi => {
                self.bump();
                Ok(Stmt::new_dummy(StmtKind::Empty, lo))
            }
            // Parse it as expression and wrap it into statement
            _ => self.parse_expr_stmt(),
        }
    }

    /// Parses let statement ('let mut? IDENTIFIER (: type)? = expr ;')
    pub fn parse_let_stmt(&mut self) -> PResult<Stmt> {
        let lo = self.token.span;
        self.bump(); // 'let'
        let mutab = if self.eat(TokenKind::Kw(sm::MUT)) {
            Mutability::Mut
        } else {
            Mutability::Const
        };
        let ident = self.consume_id("identifier in let expression")?;
        let ty = if self.eat(TokenKind::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.consume(TokenKind::Eq, "'='")?;
        let expr = self.parse_expr()?;
        let close = self.consume(TokenKind::Semi, "';'")?;
        let span = lo.to(close.span);
        Ok(Stmt::dummy_let(ident, ty, expr, mutab, span))
    }

    /// Parses expression and wraps it into statement with or without semicolon
    pub fn parse_expr_stmt(&mut self) -> PResult<Stmt> {
        let expr = self.parse_expr()?;
        let lo = expr.span;
        let (span, kind) = match self.token.kind {
            TokenKind::Semi => {
                let span = lo.to(self.token.span);
                self.bump(); // ';'
                let kind = StmtKind::Semi(expr.into());
                (span, kind)
            }
            TokenKind::CloseBrace => (lo, StmtKind::Expr(expr.into())),

            _ => {
                return Err(PError::new("Expect ';' or '}'", self.token.span));
            }
        };
        Ok(Stmt::new_dummy(kind, span))
    }

    /// Parses if expression ('if expr block (else block)? ')
    /// this.token = 'if'
    fn parse_if_expr(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // 'if'
        let expr = self.parse_expr()?;
        self.check(TokenKind::OpenBrace, "'}' after expresion")?;
        let block = self.parse_block()?;
        let (elseb, span) = if self.eat(TokenKind::Kw(sm::ELSE)) {
            let block = self.parse_block()?;
            let span = block.span;
            (Some(block), lo.to(span))
        } else {
            (None, lo.to(block.span))
        };
        let elseb = elseb.map(Box::new);
        let kind = ExprKind::If(expr.into(), block.into(), elseb);
        Ok(Expr::new_dummy(kind, span))
    }

    /// Parses while expression ('while expr block')
    /// this.token = 'while'
    fn parse_while_expr(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // 'while'
        let expr = self.parse_expr()?;
        self.check(TokenKind::OpenBrace, "'{' after expresion")?;
        let block = self.parse_block()?;
        let span = lo.to(block.span);
        let kind = ExprKind::While(expr.into(), block.into());
        Ok(Expr::new_dummy(kind, span))
    }

    /// Parses loop expression ('loop block')
    /// this.token = 'loop'
    fn parse_loop_expr(&mut self) -> PResult<Expr> {
        let lo = self.token.span;
        self.bump(); // 'loop'
        self.check(TokenKind::OpenBrace, "'{'")?;
        let block = self.parse_block()?;
        let span = lo.to(block.span);
        let kind = ExprKind::Loop(block.into());
        Ok(Expr::new_dummy(kind, span))
    }
}
