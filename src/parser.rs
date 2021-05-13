
use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum ExprType {
    Integer(i32),
    Identifier(String),
    SExp(Box<Expr>, Box<Expr>),
}

impl ExprType {
    pub fn to_expr(self) -> Expr {
        Expr { expr_type: self }
    }
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    expr_type: ExprType
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    message: String
}

impl ParseError {
    pub fn new(message: &str) -> ParseError {
        ParseError { message: String::from(message) }
    }
}

pub fn parse(token_list: Vec<Token>) -> Result<Vec<Expr>, ParseError> {
    Err(ParseError::new("Not yet implemented"))
}

