//! The same visitor for ast but take mutable reference and can mutate in place
//! Basically THE SAME code copy pasted.
//! I really don't know how to stay DRY with this without heavy macros
//! So, I'm going to get WET for now

use super::ast::*;
use crate::walk_all;

pub trait MutVisitor<'ast>: Sized {
    fn visit_item(&mut self, item: &'ast mut Item) {
        walk_item(self, item);
    }
    fn visit_expr(&mut self, expr: &'ast mut Expr) {
        walk_expr(self, expr);
    }
    fn visit_stmt(&mut self, stmt: &'ast mut Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_ident(&mut self, _ident: &'ast mut Ident) {
        /* Leaf */
    }
    fn visit_type(&mut self, ty: &'ast mut Type) {
        walk_type(self, ty);
    }
    fn visit_block(&mut self, block: &'ast mut Block) {
        walk_block(self, block);
    }
    fn visit_module(&mut self, module: &'ast mut Module) {
        walk_all!(self, visit_item, &mut module.items);
    }
}

pub fn walk_expr<'a, V: MutVisitor<'a>>(vis: &mut V, expr: &'a mut Expr) {
    use ExprKind::*;
    match &mut expr.kind {
        Literal(..) | Break | Continue => {}
        UnExpr(_, ref mut expr) => vis.visit_expr(expr),
        BinExpr(_, ref mut lhs, ref mut rhs) => {
            vis.visit_expr(lhs);
            vis.visit_expr(rhs);
        }
        Grouped(ref mut expr) => vis.visit_expr(expr),
        Field(ref mut expr, ref mut ident) => {
            vis.visit_expr(expr);
            vis.visit_ident(ident);
        }
        AddrOf(_, ref mut expr) => vis.visit_expr(expr),
        Index(ref mut lhs, ref mut idx) => {
            vis.visit_expr(lhs);
            vis.visit_expr(idx);
        }
        Ret(ref mut expr) => {
            if let Some(expr) = expr {
                vis.visit_expr(expr)
            }
        }
        Call(ref mut expr, ref mut args) => {
            vis.visit_expr(expr);
            walk_all!(vis, visit_expr, args);
        }
        As(ref mut expr, ref mut ty) => {
            vis.visit_expr(expr);
            vis.visit_type(ty);
        }
        Array(ref mut exprs) => {
            walk_all!(vis, visit_expr, exprs);
        }
        Block(ref mut block) => vis.visit_block(block),
        If(ref mut expr, ref mut then, ref mut els) => {
            vis.visit_expr(expr);
            vis.visit_block(then);
            if let Some(block) = els {
                vis.visit_block(block);
            }
        }
        While(ref mut expr, ref mut block) => {
            vis.visit_expr(expr);
            vis.visit_block(block);
        }
        Loop(ref mut block) => {
            vis.visit_block(block);
        }
    }
}

pub fn walk_type<'a, V: MutVisitor<'a>>(vis: &mut V, ty: &'a mut Type) {
    match ty.kind {
        TypeKind::Ident(ref mut ident) => vis.visit_ident(ident),
        TypeKind::Pointer(_, ref mut ty) => vis.visit_type(ty),
        TypeKind::Never | TypeKind::Void => {}
        TypeKind::ArrayType(ref mut ty, ref mut expr) => {
            vis.visit_type(ty);
            vis.visit_expr(expr);
        }
    }
}

pub fn walk_stmt<'a, V: MutVisitor<'a>>(vis: &mut V, stmt: &'a mut Stmt) {
    match stmt.kind {
        StmtKind::Expr(ref mut expr) | StmtKind::Semi(ref mut expr) => {
            vis.visit_expr(expr);
        }
        StmtKind::Let(ref mut local) => {
            vis.visit_ident(&mut local.ident);
            if let Some(ref mut ty) = local.ty {
                vis.visit_type(ty);
            }
            vis.visit_expr(&mut local.expr);
        }
        StmtKind::Empty => {}
    }
}

pub fn walk_block<'a, V: MutVisitor<'a>>(vis: &mut V, block: &'a mut Block) {
    walk_all!(vis, visit_stmt, &mut block.stmts);
}

pub fn walk_item<'a, V: MutVisitor<'a>>(vis: &mut V, item: &'a mut Item) {
    vis.visit_ident(&mut item.ident);
    match &mut item.kind {
        ItemKind::TypeAlias(ref mut ty) => vis.visit_type(ty),
        ItemKind::Const(ref mut ty, ref mut expr)
        | ItemKind::Static(_, ref mut ty, ref mut expr) => {
            vis.visit_type(ty);
            vis.visit_expr(expr);
        }
        ItemKind::Enum(ref mut idents) => {
            walk_all!(vis, visit_ident, idents);
        }
        ItemKind::Struct(pairs) => {
            pairs.iter_mut().for_each(|pair| {
                vis.visit_ident(&mut pair.ident);
                vis.visit_type(&mut pair.ty);
            });
        }
        ItemKind::Fn(ref mut sig, ref mut block) => {
            sig.params.iter_mut().for_each(|pair| {
                vis.visit_ident(&mut pair.ident);
                vis.visit_type(&mut pair.ty);
            });
            if let Some(ref mut ty) = sig.returns {
                vis.visit_type(ty);
            }
            vis.visit_block(block);
        }
        ItemKind::Import => {}
    }
}
