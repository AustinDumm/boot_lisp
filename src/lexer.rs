
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

pub type LexResult = Result<Vec<Token>, LexError>;

pub fn lex(program: String) -> LexResult {
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
            c if c.is_digit(10) => {
                return Err(LexError::new("Digits not currently handled"));
            }
            '-' => {
                return Err(LexError::new("Negative sign not currently handled"));
            }
            _ => {
                return Err(LexError::new("Identifiers not currently handled"));
            }
        }
    }

    Ok(vec![])
}

