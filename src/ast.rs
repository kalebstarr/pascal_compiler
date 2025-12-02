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
