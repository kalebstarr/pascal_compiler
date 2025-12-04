#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub header: Header,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Header {
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    Double(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(String),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Pos,
    Neg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub identifier: String,
    pub t: String,
    pub expr: Option<Box<Expr>>,
}
