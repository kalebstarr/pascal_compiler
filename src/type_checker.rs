use crate::ast::{
    BinaryOp, Expr, FunctionCall, FunctionDeclaration, Header, Literal, Program, Statement, Type,
    UnaryOp, VariableDeclaration,
};
use std::{collections::HashMap, path::Path};

#[derive(Debug, Clone, PartialEq)]
enum Symbol {
    Var {
        typ: Type,
    },
    Func {
        params: Vec<Type>,
        ret: Option<Type>,
    },
}

#[derive(Debug, PartialEq)]
pub enum TypeError {
    HeaderError(String),
    VariableError(String),
    ExprError(String),
}

pub struct TypeChecker {
    symbol_tables: Vec<HashMap<String, Symbol>>,
    pub errors: Vec<TypeError>,
}

// TODO: Improve error printing and error messages
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

    fn symbol_exists(&mut self, entry: &str) -> bool {
        self.symbol_tables.iter().any(|map| map.contains_key(entry))
    }

    fn symbol_exists_in_current_scope(&self, entry: &str) -> bool {
        self.symbol_tables
            .last()
            .is_some_and(|map| map.contains_key(entry))
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbol_tables
            .iter()
            .rev()
            .find_map(|map| map.get(name))
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

        self.declare_functions(&program.functions);
        for func in &program.functions {
            self.check_function(&func);
        }

        for stmt in &program.main {
            self.check_statement(&stmt);
        }
    }

    fn check_header(&mut self, header: &Header, program_path: &Path) {
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
        if self.symbol_exists_in_current_scope(variable.identifier.as_str()) {
            self.errors.push(TypeError::VariableError(format!(
                "Variable already exists: {}",
                variable.identifier
            )));
        } else {
            self.insert_in_current_scope(
                variable.identifier.clone(),
                Symbol::Var {
                    typ: variable.typ.clone(),
                },
            );
        }

        if let Some(expr) = &variable.expr {
            let expr_type = self.check_expr(&expr);
            if let Some(typ) = expr_type {
                if !match (&variable.typ, &typ) {
                    (Type::Double, Type::Integer) => true,
                    (a, b) => a == b,
                } {
                    self.errors.push(TypeError::VariableError(format!(
                        "Type mismatch for {}. Expected {:?}, found {:?}",
                        variable.identifier, &variable.typ, &typ
                    )));
                }
            }
        }
    }

    fn declare_functions(&mut self, functions: &[FunctionDeclaration]) {
        for f in functions {
            if self.symbol_exists_in_current_scope(&f.identifier.as_str()) {
                self.errors.push(TypeError::VariableError(format!(
                    "Function already exists: {}",
                    f.identifier
                )));
                continue;
            }

            let params = f.parameter_list.iter().map(|p| p.typ.clone()).collect();
            self.insert_in_current_scope(
                f.identifier.clone(),
                Symbol::Func {
                    params,
                    ret: f.return_type.clone(),
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

    fn check_expr(&mut self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Literal(lit) => Some(match lit {
                Literal::Integer(_) => Type::Integer,
                Literal::Double(_) => Type::Double,
                Literal::String(_) => Type::String,
                Literal::Boolean(_) => Type::Boolean,
            }),
            Expr::Identifier(id) => {
                let Some(sym) = self.lookup_symbol(id.as_str()) else {
                    self.errors
                        .push(TypeError::ExprError(format!("Unknown identifier: {}", id)));
                    return None;
                };

                match sym {
                    Symbol::Var { typ } => Some(typ.clone()),
                    Symbol::Func { .. } => {
                        self.errors.push(TypeError::ExprError(format!(
                            "Identifer {} is a function",
                            id
                        )));
                        None
                    }
                }
            }
            Expr::FunctionCall(func_call) => {
                for arg in &func_call.arguments {
                    self.check_expr(&arg);
                }

                let Some(sym) = self.lookup_symbol(func_call.identifier.as_str()) else {
                    self.errors.push(TypeError::ExprError(format!(
                        "Unknown function: {}",
                        func_call.identifier
                    )));
                    return None;
                };

                match sym {
                    Symbol::Var { typ } => Some(typ.clone()),
                    Symbol::Func { .. } => {
                        self.errors.push(TypeError::ExprError(format!(
                            "{} is not a function",
                            func_call.identifier
                        )));
                        None
                    }
                }
            }
            Expr::Unary(op, expr) => {
                let inner = self.check_expr(&expr);
                let Some(inner) = inner else {
                    return None;
                };

                match op {
                    UnaryOp::Not => {
                        if inner != Type::Boolean {
                            self.errors.push(TypeError::ExprError(format!(
                                "Expected boolean for unary operator not. Found {:?}",
                                inner
                            )));
                            return None;
                        } else {
                            Some(Type::Boolean)
                        }
                    }
                    UnaryOp::Pos | UnaryOp::Neg => {
                        if !Self::is_num(&inner) {
                            self.errors.push(TypeError::ExprError(format!(
                                "Expected numeric value for unary operator {:?}. Found {:?}",
                                op, inner
                            )));
                            return None;
                        } else {
                            Some(inner)
                        }
                    }
                }
            }
            Expr::Binary(left, op, right) => {
                let left_inner = self.check_expr(&left);
                let right_inner = self.check_expr(&right);

                let (Some(left_inner), Some(right_inner)) = (left_inner, right_inner) else {
                    return None;
                };

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul => {
                        if Self::is_num(&left_inner) && Self::is_num(&right_inner) {
                            Some(Self::num_result(&left_inner, &right_inner))
                        } else {
                            self.errors.push(TypeError::ExprError(format!(
                                "{:?} expected numeric. Found {:?} and {:?}",
                                op, left_inner, right_inner
                            )));
                            None
                        }
                    }
                    BinaryOp::Div => {
                        if Self::is_num(&left_inner) && Self::is_num(&right_inner) {
                            Some(Type::Double)
                        } else {
                            self.errors.push(TypeError::ExprError(format!(
                                "'/' expected numeric. Found {:?} and {:?}",
                                left_inner, right_inner
                            )));
                            None
                        }
                    }
                    BinaryOp::Mod => {
                        if left_inner == Type::Integer && right_inner == Type::Integer {
                            Some(Type::Integer)
                        } else {
                            self.errors.push(TypeError::ExprError(format!(
                                "'mod' expected integer. Found {:?} and {:?}",
                                left_inner, right_inner
                            )));
                            None
                        }
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        if left_inner == Type::Boolean && right_inner == Type::Boolean {
                            Some(Type::Boolean)
                        } else {
                            self.errors.push(TypeError::ExprError(format!(
                                "{:?} expected boolean. Found {:?} and {:?}",
                                op, left_inner, right_inner
                            )));
                            None
                        }
                    }
                    BinaryOp::Eq | BinaryOp::Neq => {
                        if Self::eq_comp(&left_inner, &right_inner) {
                            Some(Type::Boolean)
                        } else {
                            self.errors.push(TypeError::ExprError(format!(
                                "{:?} cannot compare. Found {:?} and {:?}",
                                op, left_inner, right_inner
                            )));
                            None
                        }
                    }
                    BinaryOp::Lt | BinaryOp::Gt | BinaryOp::Le | BinaryOp::Ge => {
                        if Self::order_comp(&left_inner, &right_inner) {
                            Some(Type::Boolean)
                        } else {
                            self.errors.push(TypeError::ExprError(format!(
                                "{:?} cannot compare. Found {:?} and {:?}",
                                op, left_inner, right_inner
                            )));
                            None
                        }
                    }
                }
            }
        }
    }

    fn is_num(typ: &Type) -> bool {
        matches!(typ, Type::Integer | Type::Double)
    }

    fn num_result(left: &Type, right: &Type) -> Type {
        if *left == Type::Double || *right == Type::Double {
            Type::Double
        } else {
            Type::Integer
        }
    }

    fn eq_comp(left: &Type, right: &Type) -> bool {
        if Self::is_num(left) && Self::is_num(right) {
            true
        } else {
            *left == *right
        }
    }

    fn order_comp(left: &Type, right: &Type) -> bool {
        Self::is_num(left) && Self::is_num(right)
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
        table_1.insert(String::from("var_1"), Symbol::Var { typ: Type::Integer });
        let mut table_2 = HashMap::new();
        table_2.insert(String::from("var_2"), Symbol::Var { typ: Type::Boolean });

        let mut checker = TypeChecker {
            symbol_tables: vec![table_1, table_2],
            errors: Vec::new(),
        };

        assert!(checker.symbol_exists("var_1"));
        assert!(checker.symbol_exists("var_2"));
        assert!(!checker.symbol_exists("Does not exist"));
    }

    // TODO: Test with expr
    #[test]
    fn variable_without_expr() {
        let mut table = HashMap::new();
        table.insert(String::from("var_1"), Symbol::Var { typ: Type::Integer });

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

    #[test]
    fn check_expr_literal() {
        let mut checker = TypeChecker {
            symbol_tables: vec![HashMap::new(), HashMap::new()],
            errors: Vec::new(),
        };
        let expr = Expr::Literal(Literal::Integer(1));

        assert_eq!(checker.check_expr(&expr).unwrap(), Type::Integer);
    }

    #[test]
    fn check_expr_identifier() {
        let mut table = HashMap::new();
        table.insert(String::from("id_1"), Symbol::Var { typ: Type::Integer });
        table.insert(
            String::from("id_3"),
            Symbol::Func { params: Vec::new(), ret: Some(Type::Integer) }
        );
        let mut checker = TypeChecker {
            symbol_tables: vec![table, HashMap::new()],
            errors: Vec::new(),
        };
        let id_1 = Expr::Identifier(String::from("id_1"));
        let id_2 = Expr::Identifier(String::from("id_2"));
        let id_3 = Expr::Identifier(String::from("id_3"));

        assert_eq!(checker.check_expr(&id_1).unwrap(), Type::Integer);
        assert_eq!(checker.check_expr(&id_2), None);
        assert!(checker.errors.contains(&TypeError::ExprError(String::from(
            "Unknown identifier: id_2"
        ))));
        assert_eq!(checker.check_expr(&id_3), None);
        assert!(checker.errors.contains(&TypeError::ExprError(String::from(
            "Identifer id_3 is a function"
        ))));
    }
}
