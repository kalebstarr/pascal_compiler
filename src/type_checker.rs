use crate::ast::{FunctionDeclaration, Header, Program, Statement, Type, VariableDeclaration};
use std::{collections::HashMap, path::Path};

struct Symbol {
    symbol_type: Type,
    is_function: bool,
}

enum TypeError {
    HeaderError(String),
}

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

    pub fn check_program(&mut self, program: &Program, program_path: &Path) {
        self.check_header(&program.header, &program_path);

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

    fn check_header(&mut self, header: &Header, program_path: &Path) {
        println!("{:?}", program_path);

        // TODO: Redo better
        if let Some(file_os_str) = program_path.file_stem()
            && let Some(file_str) = file_os_str.to_str()
        {
            if file_str != header.identifier {
                self.errors.push(TypeError::HeaderError(String::from(
                    "Header does not match program name",
                )));
            }
        } else {
            self.errors.push(TypeError::HeaderError(String::from(
                "Could not read file name",
            )));
        }
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

#[cfg(test)]
mod type_checker_tests {
    use super::*;

    #[test]
    fn header() {
        let header = Header{ identifier: String::from("Something") };
        let path = Path::new("Something.pas");
        let mut checker = TypeChecker::new();

        checker.check_header(&header, &path);

        assert!(checker.errors.is_empty());
    }
}
