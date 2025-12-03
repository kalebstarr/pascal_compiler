#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub header: Header,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Header {
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Integer(i32),
    Double(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
    Op(Box<Expr>, Opcode, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Mul,
    Div,
    Mod,
    Add,
    Sub,

    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Neq,

    And,
    Or,
    Not
}
