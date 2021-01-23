use super::ast::*;
use super::mut_visitor::*;

pub struct IdDistributor {
    counter: u32,
}

/// Walks over the ast and replace each node's id with unique one
impl IdDistributor {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    fn next(&mut self) -> NodeId {
        self.counter += 1;
        NodeId::new(self.counter)
    }
}

impl<'ast> MutVisitor<'ast> for IdDistributor {
    fn visit_item(&mut self, item: &'ast mut Item) {
        item.id = self.next();
        walk_item(self, item);
    }
    fn visit_expr(&mut self, expr: &'ast mut Expr) {
        expr.id = self.next();
        walk_expr(self, expr);
    }
    fn visit_stmt(&mut self, stmt: &'ast mut Stmt) {
        stmt.id = self.next();
        walk_stmt(self, stmt);
    }
    fn visit_type(&mut self, ty: &'ast mut Type) {
        ty.id = self.next();
        walk_type(self, ty);
    }
}
