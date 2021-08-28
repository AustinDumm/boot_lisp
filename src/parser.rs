
use std::iter::Peekable;

use std::vec::IntoIter;

use crate::lexer::{
    Token,
    TokenType,
};

use crate::call_stack::{
    StackFrame,
    CallStack,
};

use crate::env::{
    Env,
};

/// Enum type holding all different options for expressions in boot lisp.
/// - Integer
///     - Type contains a 32 bit integer. Positive or negative whole number
///     - Value belongs in set **Z**
/// - Identifier
///     - Type contains a string used to bind in the environment to other expression values
/// - Lambda
///     - Type representing an anonymous function from a list of expressions to a single expression
///     - First stored value is a binding expression which can be:
///         - Identifier - used to bind all passed parameters as a list to a single identifier
///         - List - used to bind each passed parameter to a single identifier
///         - Dotted List - used to bind each of the identifiers in the list to the matching
///         parameter in order with all remaining parameters bound to the dotted value identifier
///     - Second stored value is the body expression representing the evaluation from the bound
///     parameters to the output expression of the lambda function
///     - Third stored value is the active environment in the declaration of the lambda function.
///     The body is evaluated with respect to this environment extended by the bound parameters to
///     the argument identifiers. This environment is used to provide Lexical Scoping of the
///     evaluation of lambda functions
/// - List
///     - A representation of an S-Expression list.
///     - S-Expression is either:
///         - Nil
///         - A pair where (Expr, S-Expression)
/// - Dotted List
///     - A representation of an S-Expression list where the final right value is a
///     non-S-Expression.
/// - Nil
///     - A representation of the end of a well-formed S-Expression list.
/// - Quote
///     - A quoted expression is one that is treated as data. That is, not to be evaluated but
///     treated as pure data by other evaluated code.
#[derive(Clone)]
pub enum ExprData {
    Integer(i32),
    Identifier(String),
    Lambda(Box<Expr>, Box<Expr>, Env),
    Function(String, fn(StackFrame, &mut CallStack) -> StackFrame),
    List(IntoIter<Expr>),
    DottedList(IntoIter<Expr>, Box<Expr>),
    Nil,
    Quote(Box<Expr>),
}

impl std::fmt::Debug for ExprData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ExprData::*;
        match self {
            Integer(ref i) => i.fmt(f),
            Identifier(ref s) => s.fmt(f),
            Lambda(ref a, ref b, ref e) => { a.fmt(f)?; b.fmt(f)?; e.fmt(f) },
            Function(name, _) => write!(f, "<Built-In Function:{}>", name),
            List(ref iter) => iter.fmt(f),
            DottedList(ref iter, ref e) => { iter.fmt(f)?; e.fmt(f) },
            Nil => write!(f, "Nil"),
            Quote(ref e) => e.fmt(f),
        }
    }
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
            (ExprData::Quote(first), ExprData::Quote(second)) => first == second,
            (_, _) => false,
        }
    }
}

impl ExprData {
    /// Convenience function to convert the raw data about an expression to one containing all
    /// needed metadata for debugging and other purposes.
    ///
    /// ```
    /// assert_eq!(Expr { expr_data: ExprData::Integer(183) },
    ///            ExprData::Integer(183).to_expr());
    /// ```
    pub fn to_expr(self) -> Expr {
        Expr { expr_data: self }
    }

    /// Convenience function to create an identifier ExprData from a string literal
    ///
    /// ```
    /// assert_eq!(ExprData::Identifier(String::from("identifier")),
    ///            ExprData::ident_from("identifier"));
    /// ```
    pub fn ident_from(ident: &str) -> ExprData {
        ExprData::Identifier(String::from(ident))
    }
}

/// Data type storing both expression data and any important metadata for debugging purposes
#[derive(Debug, PartialEq, Clone)]
pub struct Expr {
    pub expr_data: ExprData
}

impl Expr {
    /// Convenience function for generating an S-Expression list from a vector of ExprDatas
    ///
    /// ```
    /// assert_eq!(ExprData::List(vec![ExprData::Integer(52).to_expr(),
    ///                                ExprData::ident_from("ident").to_expr()].into_iter()).to_expr(),
    ///            Expr::form_list(vec![ExprData::Integer(52),
    ///                                 ExprData::ident_from("ident")]));
    /// ```
    pub fn form_list(mut expr_vector: Vec<ExprData>) -> Expr {
        let mut list_exprs = vec![];
        let iter = expr_vector.drain(..);
        for expr_data in iter {
            list_exprs.push(expr_data.to_expr());
        }

        ExprData::List(list_exprs.into_iter()).to_expr()
    }
}

/// Error type containing message describing the cause of a parsing error
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

/// Convert a flat list of tokens into an Abstract Syntax Tree structure of expressions. 
///
/// boot lisp AST structure is made up of expressions representing primitive values or recursive
/// pairs of other expressions both as unstructured pairs and more strictly-structured S-Expressions.
///
/// S-Expression = Nil |
///                (Expression, S-Expression)
///
/// A brace-bound sequence of tokens represents an expression of type List, equivalent to a
/// well-formed S-Expression sequence with the left-expression in each pair being the parsed token
/// elements of the list. This indicates that parsing is an inherently recursive process.
///
/// ```
/// assert_eq!(parse(vec![TokenType::OpenBrace.to_token(),
///                       TokenType::ident_from("ident").to_token(),
///                       TokenType::Integer(182).to_token(),
///                       TokenType::OpenBrace.to_token(),
///                       TokenType::ident_from("ifier").to_token(),
///                       TokenType::Integer(592).to_token(),
///                       TokenType::CloseBrace.to_token(),
///                       TokenType::CloseBrace.to_token()]),
///            Ok(Expr::form_list(vec![ExprData::ident_from("ident"),
///                                    ExprData::Integer(182),
///                                    Expr::form_list(vec![ExprData::ident_from("ifier"),
///                                                         ExprData::Integer(592)]).expr_data])));
/// ```
pub fn parse(token_list: Vec<Token>) -> ParseResult {
    let mut token_stream = token_list.iter().peekable();
    parse_item(&mut token_stream)
}

/// Convert a peekable iterator of tokens into an Abstract Syntax Tree structure of expressions
///
/// Helper function for publicly accessible [parse] function
fn parse_item<'a, I>(token_stream: &mut Peekable<I>) -> ParseResult
where I: Iterator<Item = &'a Token> {
    if let Some(token) = token_stream.next() {
        match &token.token_type {
            TokenType::OpenBrace => parse_list(token_stream),
            TokenType::Integer(value) => Ok(ExprData::Integer(*value).to_expr()),
            TokenType::Identifier(value) => Ok(ExprData::Identifier(value.clone()).to_expr()),
            TokenType::CloseBrace => Err(ParseError::new("Close parenthesis found without matching open")),
            TokenType::Dot => Err(ParseError::new("Dot found outside of list")),
            TokenType::Quote => Ok(ExprData::Quote(Box::new(parse_item(token_stream)?)).to_expr()),
        }
    } else {
        Err(ParseError::new("Not yet implemented"))
    }
}

/// Parse a peekable iterator of tokens as a list. Intended to be called once the opening brace of
/// a list has been popped off of the passed-in iterator
fn parse_list<'a, I>(token_stream: &mut Peekable<I>) -> ParseResult
where I: Iterator<Item = &'a Token> {
    let mut list_items: Vec<Expr> = vec![];

    while let Some(token) = token_stream.peek() {
        match token.token_type {
            TokenType::CloseBrace => {
                token_stream.next();
                if list_items.len() == 0 {
                    return Ok(ExprData::Nil.to_expr())
                } else {
                    return Ok(ExprData::List(list_items.into_iter()).to_expr())
                }
            },
            TokenType::Dot => {
                token_stream.next();
                let final_element = parse_item(token_stream)?;
                if final_element.expr_data == ExprData::Nil {
                    return Ok(ExprData::List(list_items.into_iter()).to_expr())
                } else {
                    return Ok(ExprData::DottedList(list_items.into_iter(), Box::new(final_element)).to_expr())
                }
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
        assert_eq!(Ok(Expr::form_list(vec![ExprData::Integer(289)])),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Integer(289).to_token(),
                              TokenType::CloseBrace.to_token()]));

        assert_eq!(Ok(Expr::form_list(vec![ExprData::Identifier(String::from("ident"))])),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Identifier(String::from("ident")).to_token(),
                              TokenType::CloseBrace.to_token()]));

        assert_eq!(Ok(Expr::form_list(vec![ExprData::Identifier(String::from("add")),
                                           ExprData::Integer(5),
                                           ExprData::Integer(10)])),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Identifier(String::from("add")).to_token(),
                              TokenType::Integer(5).to_token(),
                              TokenType::Integer(10).to_token(),
                              TokenType::CloseBrace.to_token()]));
    }

    #[test]
    fn parses_quote_tokens() {
        assert_eq!(Ok(ExprData::Quote(Box::new(Expr::form_list(vec![ExprData::Integer(1), ExprData::Integer(2)]))).to_expr()),
                   parse(vec![TokenType::Quote.to_token(),
                              TokenType::OpenBrace.to_token(),
                              TokenType::Integer(1).to_token(),
                              TokenType::Integer(2).to_token(),
                              TokenType::CloseBrace.to_token()]));
    }

    #[test]
    fn parses_dotted_lists() {
        assert_eq!(Ok(ExprData::DottedList(vec![ExprData::Integer(512).to_expr(),
                                                ExprData::ident_from("ident").to_expr()].into_iter(),
                                           Box::new(ExprData::Integer(667).to_expr())).to_expr()),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Integer(512).to_token(),
                              TokenType::ident_from("ident").to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::Integer(667).to_token(),
                              TokenType::CloseBrace.to_token()]));

        assert_eq!(Ok(ExprData::List(vec![ExprData::Integer(951).to_expr(),
                                          ExprData::ident_from("blah").to_expr()].into_iter()).to_expr()),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Integer(951).to_token(),
                              TokenType::ident_from("blah").to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::OpenBrace.to_token(),
                              TokenType::CloseBrace.to_token()]));
    }
}

