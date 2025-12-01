// TODO: Flesh out
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub header: Header,
    pub variables: Vec<Variable>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Header {
    Identifier(String),
}

// TODO: Flesh out
#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Integer(String, Box<Expr>),
    Double(String, Box<Expr>),
    String(String, Box<Expr>),
    Boolean(String, Box<Expr>),
}

// TODO: Flesh out
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub param_list: Vec<Param>,
    pub return_type: String,
    pub statements: Vec<Statement>,
}

// TODO: Implement
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
}

// TODO: Implement
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(i32),
    Var(String),
    Op(Box<Expr>, Opcode, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
}
