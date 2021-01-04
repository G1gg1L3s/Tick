use super::ast::*;
use super::{lexer::Token, span::Span};

pub trait Visitor<T> {
    fn visit_expr(&mut self, expr: &Expr) -> T {
        let span = expr.span;
        match expr.kind {
            ExprKind::Literal(ref tk) => self.visit_lit(tk, span),
            ExprKind::UnExpr(op, ref un) => self.visit_un(op, un, span),
            ExprKind::BinExpr(op, ref lhs, ref rhs) => self.visit_bin(op, lhs, rhs, span),
            ExprKind::Grouped(ref gr) => self.visit_group(gr, span),
            ExprKind::Field(ref lhs, ref field) => self.visit_field(lhs, field, span),
        }
    }
    fn visit_lit(&mut self, tk: &Token, span: Span) -> T;
    fn visit_un(&mut self, op: UnOp, expr: &Expr, span: Span) -> T;
    fn visit_bin(&mut self, op: BinOp, lhs: &Expr, rhs: &Expr, span: Span) -> T;
    fn visit_group(&mut self, expr: &Expr, span: Span) -> T;
    fn visit_field(&mut self, expr: &Expr, field: &Token, span: Span) -> T;
}

pub struct DebugFormatter<'a> {
    src: &'a str,
    indent: usize,
}

impl<'a> DebugFormatter<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { indent: 0, src }
    }

    fn indent(&self) -> usize {
        const TAB_SIZE: usize = 4;
        self.indent * TAB_SIZE
    }
}

impl<'a> Visitor<()> for DebugFormatter<'a> {
    fn visit_lit(&mut self, tk: &Token, span: Span) -> () {
        let value = span.extract(self.src);
        let indent = self.indent();
        println!(
            "{:indent$}LITERAL: {:?}: '{}'",
            "",
            tk.kind,
            value,
            indent = indent
        );
    }

    fn visit_group(&mut self, expr: &Expr, _: Span) -> () {
        let indent = self.indent();
        println!("{:indent$}GROUP:", "", indent = indent);
        self.indent += 1;
        self.visit_expr(expr);
        self.indent -= 1;
    }

    fn visit_un(&mut self, op: UnOp, expr: &Expr, _: Span) -> () {
        println!("{:indent$}UNARY:", "", indent = self.indent());
        self.indent += 1;
        println!("{:indent$}{:?}", "", op, indent = self.indent());
        self.visit_expr(expr);
        self.indent -= 1;
    }

    fn visit_bin(&mut self, op: BinOp, lhs: &Expr, rhs: &Expr, _: Span) -> () {
        println!("{:indent$}BIN:", "", indent = self.indent());
        self.indent += 1;
        println!("{:indent$}{:?}", "", op, indent = self.indent());
        self.visit_expr(lhs);
        self.visit_expr(rhs);
        self.indent -= 1;
    }

    fn visit_field(&mut self, lhs: &Expr, token: &Token, _: Span) -> () {
        println!("{:indent$}FIELD:", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(lhs);
        let field = token.span.extract(self.src);
        println!("{:indent$}{:?}", "", field, indent = self.indent());
        self.indent -= 1;
    }
}
