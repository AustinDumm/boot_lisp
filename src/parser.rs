
use std::iter::Peekable;

use crate::lexer::{
    Token,
    TokenType,
};

#[derive(Debug, PartialEq)]
pub struct Env {}

#[derive(Debug, PartialEq)]
pub enum ExprData {
    Integer(i32),
    Identifier(String),
    Lambda(Vec<Expr>, Vec<Expr>, Env),
    List(Vec<Expr>),
    DottedList(Vec<Expr>, Box<Expr>),
    Nil,
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
    let mut token_stream = token_list.iter().peekable();
    parse_item(&mut token_stream)
}

fn parse_item<'a, I>(token_stream: &mut Peekable<I>) -> ParseResult
where I: Iterator<Item = &'a Token> {
    if let Some(token) = token_stream.next() {
        match &token.token_type {
            TokenType::OpenBrace => parse_list(token_stream),
            TokenType::Integer(value) => Ok(ExprData::Integer(*value).to_expr()),
            TokenType::Identifier(value) => Ok(ExprData::Identifier(value.clone()).to_expr()),
            TokenType::CloseBrace => Err(ParseError::new("Close parenthesis found without matching open")),
            TokenType::Dot => Err(ParseError::new("Dot found outside of list")),
        }
    } else {
        Err(ParseError::new("Not yet implemented"))
    }
}

fn parse_list<'a, I>(token_stream: &mut Peekable<I>) -> ParseResult
where I: Iterator<Item = &'a Token> {
    let mut list_items: Vec<Expr> = vec![];

    while let Some(token) = token_stream.peek() {
        match token.token_type {
            TokenType::CloseBrace => {
                token_stream.next();
                return Ok(ExprData::List(list_items).to_expr())
            },
            TokenType::Dot => {
                token_stream.next();
                let final_element = parse_item(token_stream)?;
                return Ok(ExprData::DottedList(list_items, Box::new(final_element)).to_expr())
            },
            _ => {
                list_items.push(parse_item(token_stream)?)
            }
        }
    }

    Err(ParseError::new(&format!("Unexpected end to token stream found while parsing list: {:?}", list_items)))
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

