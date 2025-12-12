use std::collections::HashMap;
use crate::ast::Type;

struct Symbol {
    symbol_type: Type,
    is_function: bool,
}

enum TypeError {
}

pub struct TypeChecker {
    symbol_tables: Vec<HashMap<String, Symbol>>,
    errors: Vec<TypeError>,
}

impl TypeChecker {
    fn new() -> Self {
        TypeChecker { symbol_tables: vec![HashMap::new()], errors: vec![] }
    }
}
