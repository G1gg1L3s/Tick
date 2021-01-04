#![feature(iter_advance_by)]
use crate::parser::{
    lexer::Lexer,
    parser::Parser,
    visitor::{DebugFormatter, Visitor},
};

mod parser;

fn main() {
    let file = std::env::args().nth(1).expect("expect filename");
    let input = std::fs::read_to_string(file).unwrap();
    let (tokens, _) = Lexer::new(&input).tokenize();
    let ast = Parser::new(&input, tokens).parse().unwrap();
    let mut formatter = DebugFormatter::new(&input);
    formatter.visit_expr(&ast);
}
