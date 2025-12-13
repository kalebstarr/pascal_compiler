use crate::ast::Type;
use std::collections::HashMap;

struct Symbol {
    symbol_type: Type,
    is_function: bool,
}

enum TypeError {}

pub struct TypeChecker {
    symbol_tables: Vec<HashMap<String, Symbol>>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    fn new() -> Self {
        let mut t = TypeChecker {
            symbol_tables: Vec::new(),
            errors: Vec::new(),
        };
        t.push_scope();
        t
    }

    fn push_scope(&mut self) {
        self.symbol_tables.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.symbol_tables.pop();
    }
}
