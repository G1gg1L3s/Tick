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
            ExprKind::AddrOf(mutab, ref expr) => self.visit_addr_of(mutab, expr, span),
            ExprKind::Index(ref lhs, ref index) => self.visit_index(lhs, index, span),
            ExprKind::Ret(ref expr) => self.visit_ret(expr.as_ref().map(Box::as_ref), span),
            ExprKind::Break => self.visit_break(span),
            ExprKind::Continue => self.visit_continue(span),
            ExprKind::Call(ref func, ref params) => self.visit_call(func, params, span),
            ExprKind::As(ref lhs, ref ty) => self.visit_as(lhs, ty, span),
        }
    }
    fn visit_lit(&mut self, tk: &Token, span: Span) -> T;
    fn visit_un(&mut self, op: UnOp, expr: &Expr, span: Span) -> T;
    fn visit_bin(&mut self, op: BinOp, lhs: &Expr, rhs: &Expr, span: Span) -> T;
    fn visit_group(&mut self, expr: &Expr, span: Span) -> T;
    fn visit_field(&mut self, expr: &Expr, field: &Token, span: Span) -> T;
    fn visit_addr_of(&mut self, mutab: Mutability, expr: &Expr, span: Span) -> T;
    fn visit_index(&mut self, lhs: &Expr, index: &Expr, span: Span) -> T;
    fn visit_ret(&mut self, expr: Option<&Expr>, span: Span) -> T;
    fn visit_break(&mut self, span: Span) -> T;
    fn visit_continue(&mut self, span: Span) -> T;
    fn visit_call(&mut self, func: &Expr, params: &[Expr], span: Span) -> T;
    fn visit_as(&mut self, lhs: &Expr, ty: &Type, span: Span) -> T;

    fn visit_type(&mut self, ty: &Type) -> T {
        let span = ty.span;
        match ty.kind {
            TypeKind::Ident(tk) => self.visit_type_ident(&tk, span),
            TypeKind::Pointer(mutab, ref ty) => self.visit_type_ptr(mutab, ty, span),
            TypeKind::Never => self.visit_type_never(span),
            TypeKind::Void => self.visit_type_void(span),
            TypeKind::ArrayType(ref ty, ref expr) => self.visit_type_arr(ty, expr, span),
        }
    }
    fn visit_type_ident(&mut self, tk: &Token, span: Span) -> T;
    fn visit_type_ptr(&mut self, mutab: Mutability, ty: &Type, span: Span) -> T;
    fn visit_type_never(&mut self, span: Span) -> T;
    fn visit_type_void(&mut self, span: Span) -> T;
    fn visit_type_arr(&mut self, ty: &Type, expr: &Expr, span: Span) -> T;
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

    fn visit_addr_of(&mut self, mutab: Mutability, expr: &Expr, _: Span) -> () {
        println!("{:indent$}ADDRESS_OF:", "", indent = self.indent());
        self.indent += 1;
        println!("{:indent$}{:?}", "", mutab, indent = self.indent());
        self.visit_expr(expr);
        self.indent -= 1;
    }

    fn visit_index(&mut self, lhs: &Expr, index: &Expr, _: Span) -> () {
        println!("{:indent$}INDEX:", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(lhs);
        self.visit_expr(index);
        self.indent -= 1;
    }

    fn visit_ret(&mut self, expr: Option<&Expr>, _: Span) -> () {
        println!("{:indent$}RETURN:", "", indent = self.indent());
        self.indent += 1;
        if let Some(expr) = expr {
            self.visit_expr(expr);
        } else {
            println!("{:indent$}()", "", indent = self.indent());
        }
        self.indent -= 1;
    }

    fn visit_break(&mut self, _: Span) -> () {
        println!("{:indent$}BREAK", "", indent = self.indent());
    }

    fn visit_continue(&mut self, _: Span) -> () {
        println!("{:indent$}CONTINUE", "", indent = self.indent());
    }

    fn visit_call(&mut self, func: &Expr, params: &[Expr], _: Span) -> () {
        println!("{:indent$}CALL:", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(func);
        params.iter().for_each(|expr| self.visit_expr(expr));
        self.indent -= 1;
    }

    fn visit_type_ident(&mut self, _: &Token, span: Span) -> () {
        let value = span.extract(self.src);
        let indent = self.indent();
        println!("{:indent$}{}", "", value, indent = indent);
    }
    fn visit_type_ptr(&mut self, mutab: Mutability, ty: &Type, _: Span) -> () {
        println!("{:indent$}{:?}:", "", mutab, indent = self.indent());
        self.indent += 1;
        self.visit_type(ty);
        self.indent -= 1;
    }

    fn visit_type_never(&mut self, _: Span) -> () {
        println!("{:indent$} !", "", indent = self.indent());
    }

    fn visit_type_void(&mut self, _: Span) -> () {
        println!("{:indent$} ()", "", indent = self.indent());
    }

    fn visit_type_arr(&mut self, ty: &Type, expr: &Expr, _: Span) -> () {
        println!("{:indent$}ARRAY_TYPE", "", indent = self.indent());
        self.indent += 1;
        self.visit_type(ty);
        self.visit_expr(expr);
        self.indent -= 1;
    }

    fn visit_as(&mut self, lhs: &Expr, ty: &Type, _: Span) -> () {
        println!("{:indent$}AS", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(lhs);
        self.visit_type(ty);
        self.indent -= 1;
    }
}
