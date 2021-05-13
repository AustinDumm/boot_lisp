
use std::iter::Peekable;

use std::convert::TryInto;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
}

#[derive(Debug, PartialEq)]
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
                token_list.push(lex_identifier(&mut stream)?);
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

    Ok(TokenType::Integer(result * if is_negative { -1 } else { 1 }).as_token())
}

fn lex_identifier<I>(stream: &mut Peekable<I>) -> LexResult
where I: Iterator<Item = char> {
    let mut identifier_name = String::new();
    while let Some(peeked) = stream.peek() {
        match peeked {
            c if c.is_whitespace() =>
                break,
            c => {
                identifier_name.push(*c);
                stream.next();
            }
        }
    }

    Ok(TokenType::Identifier(identifier_name).as_token())
}


#[cfg(test)]
mod lexing_tests {
    use super::*;

    #[test]
    fn lexes_parenthesis() {
        assert_eq!(lex(String::from("( ( ) ( ) ) (()())")),
                Ok(vec![TokenType::OpenBrace.as_token(),
                        TokenType::OpenBrace.as_token(),
                        TokenType::CloseBrace.as_token(),
                        TokenType::OpenBrace.as_token(),
                        TokenType::CloseBrace.as_token(),
                        TokenType::CloseBrace.as_token(),
                        TokenType::OpenBrace.as_token(),
                        TokenType::OpenBrace.as_token(),
                        TokenType::CloseBrace.as_token(),
                        TokenType::OpenBrace.as_token(),
                        TokenType::CloseBrace.as_token(),
                        TokenType::CloseBrace.as_token()]))
    }

    #[test]
    fn lexes_integers() {
        assert_eq!(lex(String::from("5612")),
                Ok(vec![TokenType::Integer(5612).as_token()]));

        assert_eq!(lex(String::from("-29451")),
                Ok(vec![TokenType::Integer(-29451).as_token()]));

        assert_eq!(lex(String::from("492 178 42")),
                Ok(vec![TokenType::Integer(492).as_token(),
                        TokenType::Integer(178).as_token(),
                        TokenType::Integer(42).as_token()]));

        assert_eq!(lex(String::from("-2 -49 -382")),
                Ok(vec![TokenType::Integer(-2).as_token(),
                        TokenType::Integer(-49).as_token(),
                        TokenType::Integer(-382).as_token()]));

        assert_eq!(lex(String::from("45 -19 42 -40")),
                Ok(vec![TokenType::Integer(45).as_token(),
                        TokenType::Integer(-19).as_token(),
                        TokenType::Integer(42).as_token(),
                        TokenType::Integer(-40).as_token()]));
    }

    #[test]
    fn lexes_identifiers() {
        assert_eq!(lex(String::from("ident")),
                Ok(vec![TokenType::Identifier(String::from("ident")).as_token()]));

        assert_eq!(lex(String::from("-")),
                Ok(vec![TokenType::Identifier(String::from("-")).as_token()]));

        assert_eq!(lex(String::from("thing - another")),
                Ok(vec![TokenType::Identifier(String::from("thing")).as_token(),
                        TokenType::Identifier(String::from("-")).as_token(),
                        TokenType::Identifier(String::from("another")).as_token()]));
    }
}

