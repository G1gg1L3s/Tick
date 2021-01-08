// Ugly visitor, just for debugging

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
            ExprKind::Array(ref exprs) => self.visit_array(exprs, span),
            ExprKind::Block(ref block) => self.visit_block(block),
            ExprKind::If(ref expr, ref block, ref elseb) => {
                self.visit_if(expr, block, elseb.as_ref().map(Box::as_ref))
            }
            ExprKind::While(ref expr, ref block) => self.visit_while(expr, block),
            ExprKind::Loop(ref block) => self.visit_loop(block),
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
    fn visit_array(&mut self, exprs: &[Expr], span: Span) -> T;
    fn visit_block(&mut self, block: &Block) -> T;
    fn visit_if(&mut self, expr: &Expr, block: &Block, elseblock: Option<&Block>) -> T;
    fn visit_while(&mut self, expr: &Expr, block: &Block) -> T;
    fn visit_loop(&mut self, block: &Block) -> T;

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

    fn visit_item(&mut self, item: &Item) -> T {
        let span = item.span;
        let ident = &item.ident;
        match item.kind {
            ItemKind::TypeAlias(ref ty) => self.visit_type_item(ident, ty, span),
            ItemKind::Const(ref ty, ref expr) => self.visit_const_item(ident, ty, expr),
            ItemKind::Static(mutab, ref ty, ref expr) => {
                self.visit_static_item(ident, mutab, ty, expr)
            }
            ItemKind::Enum(ref enums) => self.visit_enum(ident, enums),
            ItemKind::Struct(ref fields) => self.visit_struct(ident, fields),
        }
    }

    fn visit_type_item(&mut self, ident: &Token, ty: &Type, span: Span) -> T;
    fn visit_const_item(&mut self, ident: &Token, ty: &Type, expr: &Expr) -> T;
    fn visit_static_item(&mut self, ident: &Token, mutab: Mutability, ty: &Type, expr: &Expr) -> T;
    fn visit_enum(&mut self, ident: &Token, enums: &[Token]) -> T;
    fn visit_struct(&mut self, ident: &Token, fields: &[StructField]) -> T;

    fn visit_stmt(&mut self, stmt: &Stmt) -> T {
        match stmt.kind {
            StmtKind::Let(ref local) => self.visit_let_stmt(local),
            StmtKind::Empty => self.visit_empty_stmt(),
            StmtKind::Expr(ref expr) => self.visit_expr_stmt(expr),
            StmtKind::Semi(ref expr) => self.visit_semi_stmt(expr),
        }
    }

    fn visit_empty_stmt(&mut self) -> T;
    fn visit_let_stmt(&mut self, local: &Local) -> T;
    fn visit_expr_stmt(&mut self, expr: &Expr) -> T;
    fn visit_semi_stmt(&mut self, expr: &Expr) -> T;
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

    fn visit_stuct_field(&mut self, field: &StructField) {
        println!("{:indent$}STRUCT_FIELD:", "", indent = self.indent());
        self.indent += 1;
        self.visit_lit(&field.ident, field.ident.span);
        self.visit_type(&field.ty);
        self.indent -= 1;
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

    fn visit_array(&mut self, exprs: &[Expr], _: Span) -> () {
        println!("{:indent$}ARRAY:", "", indent = self.indent());
        self.indent += 1;
        exprs.iter().for_each(|expr| self.visit_expr(expr));
        self.indent -= 1;
    }

    fn visit_type_ident(&mut self, _: &Token, span: Span) -> () {
        let value = span.extract(self.src);
        let indent = self.indent();
        println!("{:indent$}{}", "", value, indent = indent);
    }
    fn visit_type_ptr(&mut self, mutab: Mutability, ty: &Type, _: Span) -> () {
        println!("{:indent$}{:?}_PTR:", "", mutab, indent = self.indent());
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

    fn visit_block(&mut self, block: &Block) -> () {
        println!("{:indent$}BLOCK", "", indent = self.indent());
        self.indent += 1;
        block.stmts.iter().for_each(|s| self.visit_stmt(s));
        self.indent -= 1;
    }

    fn visit_if(&mut self, expr: &Expr, block: &Block, elseb: Option<&Block>) -> () {
        println!("{:indent$}IF", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(expr);
        self.visit_block(block);
        elseb.iter().for_each(|&b| self.visit_block(b));
        self.indent -= 1;
    }

    fn visit_while(&mut self, expr: &Expr, block: &Block) -> () {
        println!("{:indent$}WHILE", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(expr);
        self.visit_block(block);
        self.indent -= 1;
    }

    fn visit_loop(&mut self, block: &Block) -> () {
        println!("{:indent$}LOOP", "", indent = self.indent());
        self.indent += 1;
        self.visit_block(block);
        self.indent -= 1;
    }

    fn visit_empty_stmt(&mut self) -> () {
        println!("{:indent$}EMPTY", "", indent = self.indent());
    }

    fn visit_let_stmt(&mut self, local: &Local) -> () {
        println!("{:indent$}LET", "", indent = self.indent());
        self.indent += 1;
        self.visit_lit(&local.ident, local.ident.span);
        if let Some(ref ty) = local.ty {
            self.visit_type(ty);
        } else {
            println!("{:indent$}DERIVE_TYPE", "", indent = self.indent());
        }
        self.visit_expr(&local.expr);
        self.indent -= 1;
    }
    fn visit_expr_stmt(&mut self, expr: &Expr) -> () {
        println!("{:indent$}EXPR_STMT", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(expr);
        self.indent -= 1;
    }
    fn visit_semi_stmt(&mut self, expr: &Expr) -> () {
        println!("{:indent$}SEMI_EXPR_STMT", "", indent = self.indent());
        self.indent += 1;
        self.visit_expr(expr);
        self.indent -= 1;
    }

    fn visit_type_item(&mut self, ident: &Token, ty: &Type, _: Span) -> () {
        println!("{:indent$}TYPE_ALIAS", "", indent = self.indent());
        self.indent += 1;
        self.visit_lit(ident, ident.span);
        self.visit_type(ty);
        self.indent -= 1;
    }

    fn visit_const_item(&mut self, ident: &Token, ty: &Type, expr: &Expr) -> () {
        println!("{:indent$}CONST_ITEM", "", indent = self.indent());
        self.indent += 1;
        self.visit_lit(ident, ident.span);
        self.visit_type(ty);
        self.visit_expr(expr);
        self.indent -= 1;
    }

    fn visit_static_item(
        &mut self,
        ident: &Token,
        mutab: Mutability,
        ty: &Type,
        expr: &Expr,
    ) -> () {
        println!(
            "{:indent$}{:?}_STATIC_ITEM",
            "",
            mutab,
            indent = self.indent()
        );
        self.indent += 1;
        self.visit_lit(ident, ident.span);
        self.visit_type(ty);
        self.visit_expr(expr);
        self.indent -= 1;
    }

    fn visit_enum(&mut self, ident: &Token, enums: &[Token]) -> () {
        println!("{:indent$}ENUM", "", indent = self.indent());
        self.indent += 1;
        self.visit_lit(ident, ident.span);

        enums
            .iter()
            .for_each(|item| self.visit_lit(item, item.span));
        self.indent -= 1;
    }

    fn visit_struct(&mut self, ident: &Token, fields: &[StructField]) -> () {
        println!("{:indent$}STRUCT_DECLARATION", "", indent = self.indent());
        self.indent += 1;
        self.visit_lit(ident, ident.span);
        fields.iter().for_each(|f| self.visit_stuct_field(f));
        self.indent -= 1;
    }
}
