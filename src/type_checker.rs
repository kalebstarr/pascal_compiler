use crate::ast::{FunctionDeclaration, Header, Program, Statement, Type, VariableDeclaration};
use std::{collections::HashMap, path::Path};

struct Symbol {
    symbol_type: Type,
    is_function: bool,
}

#[derive(PartialEq)]
enum TypeError {
    HeaderError(String),
    VariableError(String),
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

    fn symbol_exists(&mut self, entry: &String) -> bool {
        self.symbol_tables.iter().any(|map| map.contains_key(entry))
    }

    fn insert_in_current_scope(&mut self, key: String, value: Symbol) {
        if let Some(map) = self.symbol_tables.iter_mut().last() {
            map.insert(key, value);
        }
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
        if self.symbol_exists(&variable.identifier) {
            self.errors.push(TypeError::VariableError(format!(
                "Variable already exists: {}",
                variable.identifier
            )));
        } else {
            self.insert_in_current_scope(
                variable.identifier.clone(),
                Symbol {
                    symbol_type: variable.typ.clone(),
                    is_function: false,
                },
            );
        }
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
        let header = Header {
            identifier: String::from("Something"),
        };
        let path = Path::new("Something.pas");
        let mut checker = TypeChecker::new();

        checker.check_header(&header, &path);

        assert!(checker.errors.is_empty());
    }

    #[test]
    fn symbol_exists() {
        let mut table_1 = HashMap::new();
        table_1.insert(
            String::from("var_1"),
            Symbol {
                symbol_type: Type::Integer,
                is_function: false,
            },
        );
        let mut table_2 = HashMap::new();
        table_2.insert(
            String::from("var_2"),
            Symbol {
                symbol_type: Type::Boolean,
                is_function: true,
            },
        );

        let mut checker = TypeChecker {
            symbol_tables: vec![table_1, table_2],
            errors: Vec::new(),
        };

        assert!(checker.symbol_exists(&String::from("var_1")));
        assert!(checker.symbol_exists(&String::from("var_2")));
        assert!(!checker.symbol_exists(&String::from("Does not exist")));
    }

    #[test]
    fn variable() {
        let mut table = HashMap::new();
        table.insert(
            String::from("var_1"),
            Symbol {
                symbol_type: Type::Integer,
                is_function: false,
            },
        );

        let mut checker = TypeChecker {
            symbol_tables: vec![table, HashMap::new()],
            errors: Vec::new(),
        };

        checker.check_variable(&VariableDeclaration {
            identifier: String::from("var_1"),
            typ: Type::Integer,
            expr: None,
        });
        checker.check_variable(&VariableDeclaration {
            identifier: String::from("var_2"),
            typ: Type::Integer,
            expr: None,
        });

        assert!(
            checker
                .errors
                .contains(&TypeError::VariableError(String::from(
                    "Variable already exists: var_1"
                )))
        );
        assert!(
            !checker
                .errors
                .contains(&TypeError::VariableError(String::from(
                    "Variable already exists: var_2"
                )))
        );
        assert!(checker.symbol_exists(&String::from("var_2")));
    }
}
