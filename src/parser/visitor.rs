use super::ast::*;

pub trait Visitor<'ast>: Sized {
    fn visit_item(&mut self, item: &'ast Item) {
        walk_item(self, item);
    }
    fn visit_expr(&mut self, expr: &'ast Expr) {
        walk_expr(self, expr);
    }
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_ident(&mut self, _ident: &'ast Ident) {
        /* Leaf */
    }
    fn visit_type(&mut self, ty: &'ast Type) {
        walk_type(self, ty);
    }
    fn visit_block(&mut self, block: &'ast Block) {
        walk_block(self, block);
    }
}

pub fn walk_expr<'a, V: Visitor<'a>>(vis: &mut V, expr: &'a Expr) {
    use ExprKind::*;
    match &expr.kind {
        Literal(..) | Break | Continue => {}
        UnExpr(_, ref expr) => vis.visit_expr(expr),
        BinExpr(_, ref lhs, ref rhs) => {
            vis.visit_expr(lhs);
            vis.visit_expr(rhs);
        }
        Grouped(ref expr) => vis.visit_expr(expr),
        Field(ref expr, ref ident) => {
            vis.visit_expr(expr);
            vis.visit_ident(ident);
        }
        AddrOf(_, ref expr) => vis.visit_expr(expr),
        Index(ref lhs, ref idx) => {
            vis.visit_expr(lhs);
            vis.visit_expr(idx);
        }
        Ret(ref expr) => {
            expr.iter().for_each(|expr| vis.visit_expr(expr));
        }
        Call(ref expr, ref args) => {
            vis.visit_expr(expr);
            args.iter().for_each(|arg| vis.visit_expr(arg));
        }
        As(ref expr, ref ty) => {
            vis.visit_expr(expr);
            vis.visit_type(ty);
        }
        Array(ref exprs) => {
            exprs.iter().for_each(|expr| vis.visit_expr(expr));
        }
        Block(ref block) => vis.visit_block(block),
        If(ref expr, ref then, ref els) => {
            vis.visit_expr(expr);
            vis.visit_block(then);
            els.iter().for_each(|expr| vis.visit_block(expr));
        }
        While(ref expr, ref block) => {
            vis.visit_expr(expr);
            vis.visit_block(block);
        }
        Loop(ref block) => {
            vis.visit_block(block);
        }
    }
}

pub fn walk_type<'a, V: Visitor<'a>>(vis: &mut V, ty: &'a Type) {
    match ty.kind {
        TypeKind::Ident(ref ident) => vis.visit_ident(ident),
        TypeKind::Pointer(_, ref ty) => vis.visit_type(ty),
        TypeKind::Never | TypeKind::Void => {}
        TypeKind::ArrayType(ref ty, ref expr) => {
            vis.visit_type(ty);
            vis.visit_expr(expr);
        }
    }
}

pub fn walk_stmt<'a, V: Visitor<'a>>(vis: &mut V, stmt: &'a Stmt) {
    match stmt.kind {
        StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
            vis.visit_expr(expr);
        }
        StmtKind::Let(ref local) => {
            vis.visit_ident(&local.ident);
            local.ty.iter().for_each(|ty| vis.visit_type(ty));
            vis.visit_expr(&local.expr);
        }
        StmtKind::Empty => {}
    }
}

pub fn walk_block<'a, V: Visitor<'a>>(vis: &mut V, block: &'a Block) {
    block.stmts.iter().for_each(|stmt| vis.visit_stmt(stmt));
}

pub fn walk_item<'a, V: Visitor<'a>>(vis: &mut V, item: &'a Item) {
    vis.visit_ident(&item.ident);
    match &item.kind {
        ItemKind::TypeAlias(ref ty) => vis.visit_type(ty),
        ItemKind::Const(ref ty, ref expr) | ItemKind::Static(_, ref ty, ref expr) => {
            vis.visit_type(ty);
            vis.visit_expr(expr);
        }
        ItemKind::Enum(ref idents) => {
            idents.iter().for_each(|id| vis.visit_ident(id));
        }
        ItemKind::Struct(pairs) => {
            pairs.iter().for_each(|pair| {
                vis.visit_ident(&pair.ident);
                vis.visit_type(&pair.ty);
            });
        }
        ItemKind::Fn(ref sig, ref block) => {
            sig.params.iter().for_each(|pair| {
                vis.visit_ident(&pair.ident);
                vis.visit_type(&pair.ty);
            });
            sig.returns.iter().for_each(|ty| vis.visit_type(ty));
            vis.visit_block(block);
        }
        ItemKind::Import => {},
    }
}
