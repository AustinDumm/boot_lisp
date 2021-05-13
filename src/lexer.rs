
use std::iter::Peekable;

use std::convert::TryInto;

#[derive(Debug)]
pub enum TokenType {
    OpenBrace,
    CloseBrace,
    Identifier(String),
    Integer(i32),
}

impl TokenType {
    pub fn as_token(self) -> Token {
        Token { token_type: self }
    }

    pub fn is_open_brace(test: &char) -> bool {
        return *test == '(';
    }

    pub fn is_close_brace(test: &char) -> bool {
        return *test == ')';
    }
}

#[derive(Debug)]
pub struct Token {
    token_type: TokenType,
}

#[derive(Debug)]
pub struct LexError {
    message: String,
}

impl LexError {
    pub fn new(message: &str) -> LexError {
        LexError { message: String::from(message) }
    }
}

pub type LexResult = Result<Token, LexError>;

pub fn lex(program: String) -> Result<Vec<Token>, LexError> {
    let mut stream = program.chars().peekable();
    let mut token_list: Vec<Token> = vec![];
    while let Some(top_char) = stream.peek() {
        match top_char {
            c if TokenType::is_open_brace(c) => {
                token_list.push(TokenType::OpenBrace.as_token());
                stream.next();
            }
            c if TokenType::is_close_brace(c) => {
                token_list.push(TokenType::CloseBrace.as_token());
                stream.next();
            }
            '-' => {
                token_list.push(lex_minus(&mut stream)?);
            }
            c if c.is_digit(10) => {
                token_list.push(lex_digit(&mut stream, false)?);
            }
            c if c.is_whitespace() => {
                stream.next();
            }
            _ => {
                return Err(LexError::new("Identifiers not currently handled"));
            }
        }
    }

    Ok(token_list)
}

fn lex_minus<I>(stream: &mut Peekable<I>) -> LexResult
where I: Iterator<Item = char> {
    stream.next();
    match stream.peek() {
        Some(c) if c.is_digit(10) => 
            lex_digit(stream, true),
        _ =>
            Err(LexError::new("Unhandled: - has an identifier")),
    }
}

fn lex_digit<I>(stream: &mut Peekable<I>, is_negative: bool) -> LexResult
where I: Iterator<Item = char> {
    let base: u32 = 10;
    let mut result: i32 = 0;
    while let Some(peeked) = stream.peek() {
        match peeked {
            c if c.is_digit(base) => {
                let digit: i32 = c.to_digit(base).unwrap().try_into().unwrap();
                result = result * (base as i32) + digit;
                stream.next();
            }
            c if c.is_whitespace() => {
                break;
            }
            c => {
                return Err(LexError::new(&format!("Unexpected character found while lexing number: {}", c)));
            }
        }
    }

    return Ok(TokenType::Integer(result * if is_negative { -1 } else { 1 }).as_token());
}

