use std::{cell::RefCell, collections::HashMap};
use typed_arena::Arena;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Symbol(u32);

impl Symbol {
    const fn new(i: u32) -> Self {
        Self(i)
    }
}

pub struct StringInterner {
    arena: Arena<u8>,
    names: HashMap<&'static str, u32>,
    strings: Vec<&'static str>,
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
    fn new() -> Self {
        Self {
            names: HashMap::new(),
            strings: Vec::new(),
            arena: Arena::new(),
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
                let string = self.arena.alloc_str(string);
                let string: &'static str = unsafe { &*(string as *const str) };
                self.strings.push(string);
                self.names.insert(string, index);
                index
            }
        };
        Symbol(index)
    }

    pub fn lookup(&self, symb: Symbol) -> &'static str {
        self.strings[symb.0 as usize]
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
    AS = "as",
    IF = "if",
    ELSE = "else",
    WHILE = "while",
    LOOP = "loop",
    FN = "fn",
    STRUCT = "struct",
    IMPORT = "import",
    CONST = "const",
    STATIC = "static",
    ENUM = "enum",
    MUT = "mut",
    TYPE = "type",
    RETURN = "return",
    BREAK = "break",
    CONTINUE = "continue",
    LET = "let",
}
