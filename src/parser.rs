
use crate::lexer::{
    Token,
    TokenType,
};

#[derive(Debug, PartialEq)]
pub enum ExprData {
    Integer(i32),
    Identifier(String),
    Lambda(Vec<Expr>, Vec<Expr>, i32 /* env */),
    List(Vec<Expr>),
    DottedList(Vec<Expr>, Box<Expr>),
    Nil,
}

impl ExprData {
    pub fn from_token(token: Token) -> ParseResult {
        match token.token_type {
            TokenType::Integer(int) => Ok(ExprData::Integer(int).to_expr()),
            TokenType::Identifier(ident) => Ok(ExprData::Identifier(ident).to_expr()),
            other => Err(ParseError::new(&format!("Attempt to convert invalid token to expr: {:?}", other)))
        }
    }
}

impl ExprData {
    pub fn to_expr(self) -> Expr {
        Expr { expr_type: self }
    }
}

#[derive(Debug, PartialEq)]
pub struct Expr {
    expr_type: ExprData
}

impl Expr {
    pub fn form_list(mut expr_vector: Vec<Expr>) -> Expr {
        let mut list_exprs = vec![];
        let iter = expr_vector.drain(..);
        for expr in iter {
            list_exprs.push(expr);
        }

        ExprData::List(list_exprs).to_expr()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParseError {
    message: String
}

type ParseResult = Result<Expr, ParseError>;

impl ParseError {
    pub fn new(message: &str) -> ParseError {
        ParseError { message: String::from(message) }
    }
}

pub fn parse(token_list: Vec<Token>) -> ParseResult {
    Err(ParseError::new("Not yet implemented"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_single_elements() {
        assert_eq!(Ok(ExprData::Integer(58).to_expr()),
                   parse(vec![TokenType::Integer(58).to_token()]));

        assert_eq!(Ok(ExprData::Identifier(String::from("ident")).to_expr()),
                   parse(vec![TokenType::Identifier(String::from("ident")).to_token()]));
    }

    #[test]
    fn parses_lists() {
        assert_eq!(Ok(Expr::form_list(vec![ExprData::Integer(289).to_expr()])),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Integer(289).to_token(),
                              TokenType::CloseBrace.to_token()]));

        assert_eq!(Ok(Expr::form_list(vec![ExprData::Identifier(String::from("ident")).to_expr()])),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Identifier(String::from("ident")).to_token(),
                              TokenType::CloseBrace.to_token()]));

        assert_eq!(Ok(Expr::form_list(vec![ExprData::Identifier(String::from("add")).to_expr(),
                                           ExprData::Integer(5).to_expr(),
                                           ExprData::Integer(10).to_expr()])),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Identifier(String::from("add")).to_token(),
                              TokenType::Integer(5).to_token(),
                              TokenType::Integer(10).to_token(),
                              TokenType::CloseBrace.to_token()]));
    }
}

