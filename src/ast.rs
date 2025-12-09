#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub header: Header,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Header {
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub identifier: String,
    pub typ: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub identifier: String,
    pub typ: Type,
    pub expr: Option<Box<Expr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Double,
    Boolean,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VariableAssignment(VariableAssignment),
    Block(Vec<Statement>),
    IfElse(IfElse),
    While(While),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableAssignment {
    pub identifier: String,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfElse {
    pub expr: Box<Expr>,
    pub if_statement: Box<Statement>,
    pub else_statement: Option<Box<Statement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub expr: Box<Expr>,
    pub statement: Box<Statement>,
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
