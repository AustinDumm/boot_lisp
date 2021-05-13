
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
    pub fn to_token(self) -> Token {
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
    pub token_type: TokenType,
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
                token_list.push(TokenType::OpenBrace.to_token());
                stream.next();
            }
            c if TokenType::is_close_brace(c) => {
                token_list.push(TokenType::CloseBrace.to_token());
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
                token_list.push(lex_identifier(&mut stream, "")?);
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
            lex_identifier(stream, "-"),
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

    Ok(TokenType::Integer(result * if is_negative { -1 } else { 1 }).to_token())
}

fn lex_identifier<I>(stream: &mut Peekable<I>, initial_name: &str) -> LexResult
where I: Iterator<Item = char> {
    let mut identifier_name = String::from(initial_name);
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

    Ok(TokenType::Identifier(identifier_name).to_token())
}


#[cfg(test)]
mod lexing_tests {
    use super::*;

    #[test]
    fn lexes_parenthesis() {
        assert_eq!(lex(String::from("( ( ) ( ) ) (()())")),
                Ok(vec![TokenType::OpenBrace.to_token(),
                        TokenType::OpenBrace.to_token(),
                        TokenType::CloseBrace.to_token(),
                        TokenType::OpenBrace.to_token(),
                        TokenType::CloseBrace.to_token(),
                        TokenType::CloseBrace.to_token(),
                        TokenType::OpenBrace.to_token(),
                        TokenType::OpenBrace.to_token(),
                        TokenType::CloseBrace.to_token(),
                        TokenType::OpenBrace.to_token(),
                        TokenType::CloseBrace.to_token(),
                        TokenType::CloseBrace.to_token()]))
    }

    #[test]
    fn lexes_integers() {
        assert_eq!(lex(String::from("5612")),
                Ok(vec![TokenType::Integer(5612).to_token()]));

        assert_eq!(lex(String::from("-29451")),
                Ok(vec![TokenType::Integer(-29451).to_token()]));

        assert_eq!(lex(String::from("492 178 42")),
                Ok(vec![TokenType::Integer(492).to_token(),
                        TokenType::Integer(178).to_token(),
                        TokenType::Integer(42).to_token()]));

        assert_eq!(lex(String::from("-2 -49 -382")),
                Ok(vec![TokenType::Integer(-2).to_token(),
                        TokenType::Integer(-49).to_token(),
                        TokenType::Integer(-382).to_token()]));

        assert_eq!(lex(String::from("45 -19 42 -40")),
                Ok(vec![TokenType::Integer(45).to_token(),
                        TokenType::Integer(-19).to_token(),
                        TokenType::Integer(42).to_token(),
                        TokenType::Integer(-40).to_token()]));
    }

    #[test]
    fn lexes_identifiers() {
        assert_eq!(lex(String::from("ident")),
                Ok(vec![TokenType::Identifier(String::from("ident")).to_token()]));

        assert_eq!(lex(String::from("-")),
                Ok(vec![TokenType::Identifier(String::from("-")).to_token()]));

        assert_eq!(lex(String::from("thing - another")),
                Ok(vec![TokenType::Identifier(String::from("thing")).to_token(),
                        TokenType::Identifier(String::from("-")).to_token(),
                        TokenType::Identifier(String::from("another")).to_token()]));
    }
}

