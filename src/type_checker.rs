use crate::ast::{FunctionDeclaration, Header, Program, Statement, Type, VariableDeclaration};
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
    pub fn new() -> Self {
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

    pub fn check_program(&mut self, program: &Program) {
        self.check_header(&program.header);

        for var in &program.variables {
            self.check_variable(&var);
        }

        for func in &program.functions {
            self.check_function(&func);
        }

        for stmt in &program.main {
            self.check_statement(&stmt);
        }
    }

    fn check_header(&mut self, header: &Header) {
        todo!()
    }

    fn check_variable(&mut self, variable: &VariableDeclaration) {
        todo!()
    }

    fn check_function(&mut self, function: &FunctionDeclaration) {
        todo!()
    }

    fn check_statement(&mut self, statement: &Statement) {
        todo!()
    }
}
