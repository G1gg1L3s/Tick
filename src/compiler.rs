use super::parser::symbol::StringInterner;

pub struct Compiler {
    pub interner: StringInterner,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            interner: StringInterner::new(),
        }
    }
}
