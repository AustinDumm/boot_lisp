
use std::iter::Peekable;

use std::vec::IntoIter;

use crate::lexer::{
    Token,
    TokenType,
};

use crate::env::{
    Env,
};

#[derive(Debug, Clone)]
pub enum ExprData {
    Integer(i32),
    Identifier(String),
    Lambda(Box<Expr>, Vec<Expr>, Env),
    List(IntoIter<Expr>),
    DottedList(IntoIter<Expr>, Box<Expr>),
    Nil,
}

impl PartialEq for ExprData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ExprData::Integer(this), ExprData::Integer(other)) => this == other,
            (ExprData::Identifier(this), ExprData::Identifier(other)) => this == other,
            (ExprData::Lambda(this_args, this_body, this_env), ExprData::Lambda(other_args, other_body, other_env)) 
                => this_args == other_args &&
                   this_body == other_body &&
                   this_env == other_env,
            (ExprData::List(this), ExprData::List(other)) => this.clone().eq(other.clone()),
            (ExprData::DottedList(this_list, this_last), ExprData::DottedList(other_list, other_last))
                => this_list.clone().eq(other_list.clone()) &&
                   *this_last == *other_last,
            (ExprData::Nil, ExprData::Nil) => true,
            (_, _) => false,
        }
    }
}

impl ExprData {
    pub fn to_expr(self) -> Expr {
        Expr { expr_data: self }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub expr_data: ExprData
}

impl Expr {
    pub fn form_list(mut expr_vector: Vec<Expr>) -> Expr {
        let mut list_exprs = vec![];
        let iter = expr_vector.drain(..);
        for expr in iter {
            list_exprs.push(expr);
        }

        ExprData::List(list_exprs.into_iter()).to_expr()
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
                return Ok(ExprData::List(list_items.into_iter()).to_expr())
            },
            TokenType::Dot => {
                token_stream.next();
                let final_element = parse_item(token_stream)?;
                return Ok(ExprData::DottedList(list_items.into_iter(), Box::new(final_element)).to_expr())
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

