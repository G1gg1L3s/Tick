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
    /// '='
    Assign,
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
    /// A `return`, with an optional value to be returned.
    Ret(Option<Box<Expr>>),
    /// A `break` from loop
    Break,
    /// A `continue` in loop
    Continue,
    /// A function call
    ///
    /// The first field resolves to the function itself,
    /// and the second field is the list of arguments.
    Call(Box<Expr>, Vec<Expr>),
    /// An `as` expression
    As(Box<Expr>, Box<Type>),
    /// An array (`[a, b, c, d]`)
    Array(Vec<Expr>),
    /// Block ('{ expr* '})
    Block(Box<Block>),
    /// If expression ('if expr block (else block)?')
    If(Box<Expr>, Box<Block>, Option<Box<Block>>),
    /// While expression ('while expr block')
    While(Box<Expr>, Box<Block>),
    /// While expression ('loop block')
    Loop(Box<Block>),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeKind {
    /// Typename like `i32` or `Point`
    Ident(Token),
    /// A pointer `& mut? Type`
    Pointer(Mutability, Box<Type>),
    /// The never type (`!`).
    Never,
    /// The void type (`()`).
    Void,
    /// A fixed length array (`[T; n]`).
    ArrayType(Box<Type>, Box<Expr>),
}

#[derive(Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ItemKind {
    /// A type alias ('type IDENTIFIER = Type ;')
    TypeAlias(Box<Type>),
    /// A const item ('const IDENTIFIER : Type = Expr ;')
    Const(Box<Type>, Box<Expr>),
    /// A static item ('static mut? IDENTIFIER : Type = Expr ;')
    Static(Mutability, Box<Type>, Box<Expr>),
    /// An enum definition ('enum IDENTITER { EnumItems* }')
    Enum(Vec<Token>),
    /// A struct definition ('struct IDENTITER { StructFields* }')
    Struct(Vec<IdentTypePair>),
    /// Function declaration ('fn IDENTITER ( (Param,)* ) block')
    Fn(Box<FnSignature>, Box<Block>),
}

#[derive(Debug)]
pub struct Item {
    pub ident: Token,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct IdentTypePair {
    pub ident: Token,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    /// Trailing semicolon
    Empty,
    /// Let statement ('let mut? Identifier = Expr ;')
    Let(Box<Local>),
    /// Expression without semicolon
    Expr(Box<Expr>),
    /// Expression with semicolon
    Semi(Box<Expr>),
}

#[derive(Debug)]
pub struct Local {
    pub ident: Token,
    pub ty: Option<Type>,
    pub expr: Expr,
    pub mutab: Mutability,
}

#[derive(Debug)]
pub struct FnSignature {
    pub params: Vec<IdentTypePair>,
    pub returns: Option<Type>,
}

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

impl Stmt {
    pub fn new_let(
        ident: Token,
        ty: Option<Type>,
        expr: Expr,
        mutab: Mutability,
        span: Span,
    ) -> Self {
        let local = Local {
            ident,
            ty,
            expr,
            mutab,
        };
        Self {
            kind: StmtKind::Let(local.into()),
            span,
        }
    }
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

    pub fn new_ret(span: Span, expr: Option<Expr>) -> Self {
        Self {
            span,
            kind: ExprKind::Ret(expr.map(Box::new)),
        }
    }

    pub fn new_call(func: Expr, params: Vec<Expr>, span: Span) -> Self {
        Self {
            span,
            kind: ExprKind::Call(Box::new(func), params),
        }
    }

    pub fn new_as(lhs: Expr, ty: Type, span: Span) -> Self {
        Self {
            span,
            kind: ExprKind::As(Box::new(lhs), Box::new(ty)),
        }
    }

    pub fn new_array(span: Span, exprs: Vec<Expr>) -> Self {
        Self {
            span,
            kind: ExprKind::Array(exprs),
        }
    }
}

impl Type {
    pub fn new_arr(ty: Type, expr: Expr, span: Span) -> Self {
        Self {
            span,
            kind: TypeKind::ArrayType(Box::new(ty), Box::new(expr)),
        }
    }
}
