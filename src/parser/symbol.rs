use std::{cell::RefCell, collections::HashMap};
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Symbol(u32);

impl Symbol {
    const fn new(i: u32) -> Self {
        Self(i)
    }
}

pub struct StringInterner {
    names: HashMap<String, u32>,
    strings: Vec<String>,
}

thread_local! {
    static STRING_INTERNER: RefCell<StringInterner> = RefCell::new(StringInterner::prefilled());
}

pub fn with_interner<T, F>(f: F) -> T
where
    F: FnOnce(&mut StringInterner) -> T,
{
    STRING_INTERNER.with(|interner| f(&mut *interner.borrow_mut()))
}

impl From<&str> for Symbol {
    fn from(string: &str) -> Symbol {
        with_interner(|interner| interner.insert(string))
    }
}

impl From<Symbol> for String {
    fn from(symb: Symbol) -> String {
        with_interner(|interner| interner.lookup(symb).to_string())
    }
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            strings: Vec::new(),
        }
    }

    pub fn prefilled() -> Self {
        let mut res = Self::new();
        res.pre_fill();
        res
    }

    // Two allocations, ok for now but revisit in future
    pub fn insert(&mut self, string: &str) -> Symbol {
        let index = match self.names.get(string) {
            Some(&index) => index,
            None => {
                let index = self.strings.len() as u32;
                self.strings.push(string.to_string());
                self.names.insert(string.to_string(), index);
                index
            }
        };
        Symbol(index)
    }

    pub fn lookup<'a>(&'a self, symb: Symbol) -> &'a str {
        self.strings[symb.0 as usize].as_str()
    }
}

macro_rules! symbols {
    ($($id:ident = $string:expr,)*) => {
        enum Symbols {
            $($id,)*
        }

        pub mod symbols {
            use super::{Symbols, Symbol};
            $(pub const $id: Symbol = Symbol::new(Symbols::$id as u32);)*
        }

        impl StringInterner {
            pub fn pre_fill(&mut self) {
                $(self.insert($string);)*
            }
        }
    };
}

symbols! {
    As = "as",
    If = "if",
    Else = "else",
    While = "while",
    Loop = "loop",
    Fn = "fn",
    Struct = "struct",
    Import = "import",
    Const = "const",
    Static = "static",
    Enum = "enum",
    Mut = "mut",
    Type = "type",
    Return = "return",
    Break = "break",
    Continue = "continue",
    Let = "let",
}
