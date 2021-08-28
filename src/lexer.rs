
use std::iter::Peekable;

use std::convert::TryInto;

/// Enum type contains one of the support TokenTypes that is lexed from the input stream
/// - OpenBrace
///     - ```[\(]```
///     - Marks the beginning of the list when parsed
/// - Close Brace
///     - ```[\)]```
///     - Marks the end of a list when parsed
/// - Dot
///     - ```[\.]```
///     - Used to define explicit pairs
/// - Identifier
///     - ```[a-zA-Z_]+[.]*```
///     - General type used to bind to values
/// - Integer
///     - ```[-]?[0-9]```
///     - Whole number parsed to be positive or negative if preceeded by a "-" character
/// - Quote
///     - ```[']```
///     - Marks an expression to be treated as a data literal rather than expression to evaluate
///
/// Tokens are the smallest meaningful piece of a valid program in boot lisp. The TokenType enum
/// holds the data for each of the types of tokens that can be in a program. These are generated
/// from a character stream of a program by the [lex] function
///
/// ```
/// # use TokenType;
/// let open_brace = TokenType::OpenBrace;
/// let close_brace = TokenType::CloseBrace;
/// let dot = TokenType::Dot;
/// let integer = TokenType::Integer(52);
/// let identifier = TokenType::ident_from("test-ident"));
///
/// ```

#[derive(Debug, PartialEq)]
pub enum TokenType {
    OpenBrace,
    CloseBrace,
    Dot,
    Identifier(String),
    Bool(bool),
    Integer(i32),
    Quote,
}

impl TokenType {
    /// Converts TokenType enum value to Token Struct
    ///
    /// Used for convenience when generating Tokens from TokenTypes
    ///
    /// ```
    /// # use TokenType;
    /// let token_type = TokenType::Integer(182);
    /// let token = token_type.to_token();
    ///
    /// assert_eq!(token.token_type, token_type);
    /// ```
    pub fn to_token(self) -> Token {
        Token { token_type: self }
    }

    /// Generates an identifier from an &str
    ///
    /// Used for convenience when generating Identifiers from &str literals
    /// 
    /// ```
    /// # use TokenType;
    /// let identifier = TokenType::ident_from("test-ident"));
    /// let identifier2 = TokenType::ident_from("test-ident");
    ///
    /// assert_eq!(identifier, identifier2);
    /// ```

    pub fn ident_from(string: &str) -> TokenType {
        TokenType::Identifier(String::from(string))
    }

    /// Tests whether character matches open brace character
    ///
    /// ```
    /// # use TokenType;
    /// assert!(TokenType::is_open_brace(&'('));
    /// assert!(!TokenType::is_open_brace(&'b'));
    /// ```
    pub fn is_open_brace(test: &char) -> bool {
        return *test == '(';
    }

    /// Tests whether character matches close brace character
    ///
    /// ```
    /// # use TokenType;
    /// assert!(TokenType::is_close_brace(&')'));
    /// assert!(!TokenType::is_close_brace(&'w'));
    /// ```
    pub fn is_close_brace(test: &char) -> bool {
        return *test == ')';
    }

    /// Tests whether character is delimiter. A delimiter is a character that can end a token such
    /// as whitespace or a brace
    ///
    /// ```
    /// assert!(TokenType::is_delimiter(&'('));
    /// assert!(TokenType::is_delimiter(&')'));
    /// assert!(TokenType::is_delimiter(&' '));
    /// assert!(!TokenType::is_delimiter(&'b'));
    pub fn is_delimiter(test: &char) -> bool {
        TokenType::is_open_brace(test) ||
            TokenType::is_close_brace(test) ||
            test.is_whitespace()
    }
}

/// Stores TokenType data and other important data retrieved while lexing a character stream
///
/// Token stores metadata as well as the TokenType to be used by the parser.
/// This struct will hold debugging information such a row and col for the beginning character of
/// the token.
///
/// ```
/// assert_eq!(Token { token_type: TokenType::Integer(182) },
///         TokenType::Integer(182).to_expr());
/// ```
#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
}

/// Error type for any failed lexing. Contains a message string to store information about the
/// error
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

/// Takes in a program and lexes into valid Program Tokens
///
/// Converts program to a character stream with one character lookahead. Uses the one-lookahead
/// stream to convert into a vector of tokens. Returns a LexError if any tokens are ill-formed or
/// erroneous.
///
/// ```
/// assert_eq!(lex(String::from("(-123 . identifier)")),
///            Ok(vec![TokenType::OpenBrace.to_token(),
///                    TokenType::Integer(-123).to_token(),
///                    TokenType::Dot.to_token(),
///                    TokenType::ident_from("identifier").to_token(),
///                    TokenType::CloseBrace.to_token()]));
/// ```
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
            '.' => {
                token_list.push(TokenType::Dot.to_token());
                stream.next();
            }
            '\'' => {
                token_list.push(TokenType::Quote.to_token());
                stream.next();
            }
            '#' => {
                token_list.push(lex_octothorpe(&mut stream)?);
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

/// Lexes a peekable character stream whose current first character is a "-"
///
/// This character can lex to either a negative integer if followed by a digit character or can lex
/// to an identifier if followed by a non-numeric character or a space (signifying the end of the
/// identifier)
///
/// ```
/// assert_eq!(lex_minus(String::from("-456").chars().peekable()),
///            Ok(TokenType::Integer(-456).to_token()));
///
/// assert_eq!(lex_minus(String::from("- ").chars().peekable()),
///            Ok(TokenType::ident_from("-").to_token()));
///
/// assert_eq!(lex_minus(String::from("-ident").chars().peekable()),
///            Ok(TokenType::ident_from("-ident").to_token()));
/// ```
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

/// Lexes a peekable character stream whose current first character is a digit
///
/// A digit can currently only lex as part of an integer meaning any character other than a digit
/// or whitespace will cause an error which returns Err(LexError) containing a message of the
/// erring character. Function takes in a flag if the number to be lexed should be treated as
/// negative due to an already-lexed and thrown-away "-" character preceeding it. "-" characters
/// are handled in [lex_minus].
///
/// ```
/// assert_eq!(lex_digit(String::from("582").chars().peekable(), false),
///            Ok(TokenType::Integer(582).to_token()));
///
/// assert_eq!(lex_digit(String::from("284").chars().peekable(), true),
///            Ok(TokenType::Integer(-284).to_token()));
///
/// assert_eq!(lex_digit(String::from("a").chars().peekable(), true),
///            Err(LexError::new(String::from("Unexpected character found while lexing number: a"))));
/// ```
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
            c if TokenType::is_delimiter(&c) => {
                break;
            }
            c => {
                return Err(LexError::new(&format!("Unexpected character found while lexing number: {}", c)));
            }
        }
    }

    Ok(TokenType::Integer(result * if is_negative { -1 } else { 1 }).to_token())
}

/// Lexes a peekable character stream whose current first character is that to be used in an
/// identifier beginning with the string in argument "initial_name"
///
/// An identifier can contain any character provided the identifier does not begin with a digit nor
/// begin with a "-" followed by digits. The initial character(s) for the identifier is passed into
/// this function using the argument: "initial_name". This is used when lexing an identifier
/// beginning with a "-" whose "-" character has already been removed from the character stream as
/// part of identifying if "-" is part of an identifier or an integer.
///
/// ```
/// assert_eq!(lex_identifier(String:from("identifier").chars().peekable(), ""),
///            Ok(TokenType::ident_from("identifier").to_token()));
///
/// assert_eq!(lex_identifier(String::from("minus").chars().peekable(), "-"),
///            Ok(TokenType::ident_from("-minus").to_token()));
///
/// assert_eq!(lex_identifier(String::from("").chars().peekable(), "-"),
///            Ok(TokenType::ident_from("-").to_token()));
/// ```
fn lex_identifier<I>(stream: &mut Peekable<I>, initial_name: &str) -> LexResult
where I: Iterator<Item = char> {
    let mut identifier_name = String::from(initial_name);
    while let Some(peeked) = stream.peek() {
        match peeked {
            c if TokenType::is_delimiter(&c) =>
                break,
            c => {
                identifier_name.push(*c);
                stream.next();
            }
        }
    }

    Ok(TokenType::Identifier(identifier_name).to_token())
}

fn lex_octothorpe<I>(stream: &mut Peekable<I>) -> LexResult
where I: Iterator<Item = char> {
    // Drop octothorpe
    stream.next();
    let result = 
        match stream.next() {
            Some('t') =>
                Ok(TokenType::Bool(true).to_token()),
            Some('f') =>
                Ok(TokenType::Bool(false).to_token()),
            Some(other) =>
                Err(LexError::new(&format!("Invalid character found after #: {}", other))),
            None =>
                Err(LexError::new(&format!("End of stream found after #"))),
        };

    if let Some(peeked) = stream.peek() {
        if !TokenType::is_delimiter(peeked) {
            Err(LexError::new(&format!("Invalid character found after boolean value: {}", peeked)))
        } else {
            result
        }
    } else {
        result
    }
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
                Ok(vec![TokenType::ident_from("ident").to_token()]));

        assert_eq!(lex(String::from("-")),
                Ok(vec![TokenType::ident_from("-").to_token()]));

        assert_eq!(lex(String::from("thing - another")),
                Ok(vec![TokenType::ident_from("thing").to_token(),
                        TokenType::ident_from("-").to_token(),
                        TokenType::ident_from("another").to_token()]));
    }

    #[test]
    fn lexes_dot() {
        assert_eq!(lex(String::from(".")),
                Ok(vec![TokenType::Dot.to_token()]));

        assert_eq!(lex(String::from("1 . 2")),
                Ok(vec![TokenType::Integer(1).to_token(),
                        TokenType::Dot.to_token(),
                        TokenType::Integer(2).to_token()]));
    }

    #[test]
    fn lexes_quote() {
        assert_eq!(lex(String::from("'")),
                   Ok(vec![TokenType::Quote.to_token()]));
    }

    #[test]
    fn lexes_bool() {
        assert_eq!(lex(String::from("#t")),
                   Ok(vec![TokenType::Bool(true).to_token()]));

        assert_eq!(lex(String::from("#f")),
                   Ok(vec![TokenType::Bool(false).to_token()]));
    }
}

