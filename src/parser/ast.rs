use super::{lexer::Token, span::Span};

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum UnOp {
    /// '@'
    Deref,
    /// '-'
    Neg,
    /// '!'
    Not,
}

#[derive(Clone, PartialEq, Debug, Copy)]
pub enum BinOp {
    /// '+'
    Add,
    /// '-'
    Sub,
    /// '*'
    Mul,
    /// '*'
    Div,
    /// '%'
    Rem,
    /// '&&'
    And,
    /// '||'
    Or,
    /// '=='
    Eq,
    /// '<'
    Lt,
    /// '<='
    Le,
    /// '!='
    Ne,
    /// '>='
    Ge,
    /// '>'
    Gt,
}

#[derive(Debug)]
pub enum ExprKind {
    Literal(Token),
    UnExpr(UnOp, Box<Expr>),
    BinExpr(BinOp, Box<Expr>, Box<Expr>),
    Grouped(Box<Expr>),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new_lit(tk: Token, span: Span) -> Self {
        Self {
            span,
            kind: ExprKind::Literal(tk),
        }
    }

    pub fn new_un(span: Span, op: UnOp, expr: Expr) -> Self {
        Self {
            span,
            kind: ExprKind::UnExpr(op, Box::new(expr)),
        }
    }

    pub fn new_group(span: Span, expr: Expr) -> Self {
        Self {
            span,
            kind: ExprKind::Grouped(Box::new(expr)),
        }
    }
}
