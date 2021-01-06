#![feature(iter_advance_by)]
use crate::parser::{
    lexer::Lexer,
    parser::Parser,
    visitor::{DebugFormatter, Visitor},
};

mod parser;

fn main() {
    let file = "test.tk"; //std::env::args().nth(1).expect("expect filename");
    let input = std::fs::read_to_string(&file).unwrap();
    let (tokens, _) = Lexer::new(&input).tokenize();
    let items = Parser::new(&input, tokens).parse();
    match items {
        Ok(items) => {
            let mut formatter = DebugFormatter::new(&input);
            items.iter().for_each(|item| formatter.visit_item(item));
        }
        Err(err) => {
            let line_num = input[..err.span.lo as usize].lines().count() - 1;
            let line = input.lines().nth(line_num).unwrap();
            let content = err.span.extract(&input);
            let content_in_chars = content.chars().count().max(1);
            let prefix_in_chars = line.chars().count() - content_in_chars;
            let prefix = " ".repeat(prefix_in_chars + 1);
            let mark = "~".repeat(content_in_chars);
            println!(
                "Error at {}:{}:{}: {}",
                file, line_num, prefix_in_chars, err.msg
            );
            println!("{}", line);
            println!("{}{}", prefix, mark);
        }
    }
}
