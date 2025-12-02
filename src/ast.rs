#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub header: Header,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Header {
    Identifier(String),
}
