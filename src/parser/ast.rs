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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Mutability {
    Const,
    Mut,
}

#[derive(Debug)]
pub enum ExprKind {
    /// Literals like number or strings
    Literal(Token),
    /// A unary operation
    UnExpr(UnOp, Box<Expr>),
    /// A binary operation
    BinExpr(BinOp, Box<Expr>, Box<Expr>),
    /// Grouped expression `(expr)`
    Grouped(Box<Expr>),
    /// Access of a struct field `a.b`
    Field(Box<Expr>, Token),
    /// A taking address of expression operation `& mut? expr`
    AddrOf(Mutability, Box<Expr>),
    /// An indexing operation `expr[expr]`.
    Index(Box<Expr>, Box<Expr>),
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

    pub fn new_bin(span: Span, op: BinOp, lhs: Expr, rhs: Expr) -> Self {
        Self {
            span,
            kind: ExprKind::BinExpr(op, Box::new(lhs), Box::new(rhs)),
        }
    }

    pub fn new_field(span: Span, lhs: Expr, name: Token) -> Self {
        Self {
            span,
            kind: ExprKind::Field(Box::new(lhs), name),
        }
    }

    pub fn new_addr_of(span: Span, mutab: Mutability, expr: Expr) -> Self {
        Self {
            span,
            kind: ExprKind::AddrOf(mutab, Box::new(expr)),
        }
    }

    pub fn new_index(span: Span, lhs: Expr, index: Expr) -> Self {
        Self {
            span,
            kind: ExprKind::Index(Box::new(lhs), Box::new(index)),
        }
    }
}
