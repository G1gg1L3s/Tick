use super::ast::*;
use super::visitor::*;

const TAB_SIZE: usize = 4;

pub struct DebugFormatter {
    intend: i32,
}

impl DebugFormatter {
    pub fn new() -> Self {
        Self { intend: -1 }
    }

    fn with<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.intend += 1;
        print!("{:i$}", "", i = self.intend as usize * TAB_SIZE);
        f(self);
        self.intend -= 1;
    }
}

impl<'ast> Visitor<'ast> for DebugFormatter {
    fn visit_ident(&mut self, ident: &'ast Ident) {
        self.with(|_| {
            println!("IDENT: {:?}", ident.ident);
        });
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.with(|this| {
            print!("{:?}::", expr.id);
            match expr.kind {
                ExprKind::Literal(token) => {
                    println!("LITERAL {:?}", token.kind);
                }
                ExprKind::Break => {
                    println!("BREAK");
                }
                ExprKind::Continue => {
                    println!("CONTINUE");
                }
                ExprKind::UnExpr(op, _) => {
                    println!("UNARY '{:?}'", op);
                }
                ExprKind::BinExpr(op, _, _) => {
                    println!("BINARY '{:?}'", op);
                }
                ExprKind::Grouped(_) => {
                    println!("GROUP");
                }
                ExprKind::Field(..) => {
                    println!("FIELD");
                }
                ExprKind::AddrOf(mutab, _) => {
                    println!("ADDR_OF '{:?}'", mutab);
                }
                ExprKind::Index(..) => {
                    println!("INDEX");
                }
                ExprKind::Ret(..) => {
                    println!("RETURN");
                }
                ExprKind::Call(..) => {
                    println!("CALL");
                }
                ExprKind::As(..) => {
                    println!("AS");
                }
                ExprKind::Array(..) => {
                    println!("ARRAY");
                }
                ExprKind::Block(..) => {
                    println!("BLOCK");
                }
                ExprKind::If(..) => {
                    println!("IF");
                }
                ExprKind::While(..) => {
                    println!("WHILE");
                }
                ExprKind::Loop(..) => {
                    println!("LOOP");
                }
            }
            walk_expr(this, expr);
        });
    }

    fn visit_type(&mut self, ty: &'ast Type) {
        self.with(|this| {
            print!("{:?}::", ty.id);
            match ty.kind {
                TypeKind::Ident(ref ident) => {
                    println!("TYPE_IDENT: {:?}", ident.ident);
                    return;
                }
                TypeKind::Pointer(mutab, ..) => {
                    println!("POINTER '{:?}'", mutab);
                }
                TypeKind::Never => {
                    println!("NEVER_TYPE");
                }
                TypeKind::Void => {
                    println!("VOID_TYPE");
                }
                TypeKind::ArrayType(..) => {
                    println!("ARRAY_TYPE");
                }
            }
            walk_type(this, ty);
        })
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        print!("{:?}::", stmt.id);
        self.with(|this| {
            match stmt.kind {
                StmtKind::Expr(..) => {
                    println!("EXPR");
                }
                StmtKind::Semi(..) => {
                    println!("SEMI_EXPR");
                }
                StmtKind::Let(..) => {
                    println!("LET");
                }
                StmtKind::Empty => {
                    println!("EMPTY");
                }
            }
            walk_stmt(this, stmt);
        });
    }

    fn visit_block(&mut self, block: &'ast Block) {
        self.with(|this| {
            println!("BLOCK");
            walk_block(this, block);
        });
    }

    fn visit_item(&mut self, item: &'ast Item) {
        self.with(|this| {
            print!("{:?}::", item.id);
            match &item.kind {
                ItemKind::TypeAlias(..) => {
                    println!("TYPE_ALIAS");
                }
                ItemKind::Const(..) => {
                    println!("CONST");
                }
                ItemKind::Static(..) => {
                    println!("STATIC");
                }
                ItemKind::Enum(..) => {
                    println!("ENUM");
                }
                ItemKind::Struct(..) => {
                    println!("STRUCT");
                }
                ItemKind::Fn(..) => {
                    println!("FN");
                }
                ItemKind::Import => {
                    println!("IMPORT");
                }
            }
            walk_item(this, item);
        });
    }
}
