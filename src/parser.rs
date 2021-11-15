
use std::iter::Peekable;

use std::vec::IntoIter;

use crate::lexer::{
    Token,
    TokenType,
    BootLispError,
    ErrorType,
};

use crate::call_stack::{
    StackFrame,
    CallStack,
};

use crate::env::{
    Env,
};

use crate::default_env;

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
/// - Function
///     - Type representing a built-in function from a list of expressions to a single expression
///     - Stored as a function pointer to a Rust function that takes in
///         - Accumulator - Register storing the result of an evaluation
///         - Current Frame - The current evaluation frame for this function evaluation
///         - Frame Stack - All frames waiting for evaluation
///     - Returns a new current frame and an updated accumulator and frame stack
///     - Allows functions to have full control over evaluation order and data
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
#[derive(Clone)]
pub enum ExprData {
    Bool(bool),
    Integer(i32),
    Identifier(String),
    Lambda(Box<Expr>, Box<Expr>, Env),
    Function(String, fn(&mut Option<Expr>, Option<StackFrame>, &mut CallStack) -> Option<StackFrame>),
    List(IntoIter<Expr>),
    DottedList(IntoIter<Expr>, Box<Expr>),
    Nil,
}

impl std::fmt::Debug for ExprData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::fmt::Display for ExprData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use ExprData::*;
        match self {
            Bool(true) => "#t".fmt(f),
            Bool(false) => "#f".fmt(f),
            Integer(ref i) => i.fmt(f),
            Identifier(ref s) => s.fmt(f),
            Lambda(_, _, _) => write!(f, "<Lambda>"),
            Function(name, _) => write!(f, "<Built-In Function: {}>", name),
            List(ref iter) => {
                let count = iter.clone().count();
                write!(f, "(")?;
                for (index, item) in iter.clone().enumerate() {
                    write!(f, "{}", item)?;
                    
                    if index != count - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            },
            DottedList(ref iter, ref e) => {
                write!(f, "(")?;
                for item in iter.clone() {
                    write!(f, "{} ", item)?;
                }
                write!(f, ". {})", e)
            },
            Nil => write!(f, "Nil"),
        }
    }
}

impl PartialEq for ExprData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ExprData::Bool(this), ExprData::Bool(other)) => this == other,
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

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.expr_data)
    }
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

    pub fn quoted(self) -> Expr {
        ExprData::List(
            vec![
                ExprData::Function("quote".to_string(), default_env::quote).to_expr(),
                self
            ].into_iter()).to_expr()
    }
}

type ParseResult = Result<Expr, BootLispError>;

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
            TokenType::Bool(value) => Ok(ExprData::Bool(*value).to_expr()),
            TokenType::Integer(value) => Ok(ExprData::Integer(*value).to_expr()),
            TokenType::Identifier(value) => Ok(ExprData::Identifier(value.clone()).to_expr()),
            TokenType::CloseBrace => Err(BootLispError::new(ErrorType::Parse,
                                                            "Close parenthesis found without matching open")),
            TokenType::Dot => Err(BootLispError::new(ErrorType::Parse,
                                                     "Dot found outside of list")),
            TokenType::Quote => 
                Ok(ExprData::List(vec![ExprData::Identifier("quote".to_string()).to_expr(),
                                       parse_item(token_stream)?].into_iter()).to_expr()),
            TokenType::Quasiquote =>
                Ok(ExprData::List(vec![ExprData::Identifier("quasiquote".to_string()).to_expr(),
                                       parse_item(token_stream)?].into_iter()).to_expr()),
            TokenType::Unquote =>
                Ok(ExprData::List(vec![ExprData::Identifier("unquote".to_string()).to_expr(),
                                       parse_item(token_stream)?].into_iter()).to_expr()),
            TokenType::UnquoteSplicing =>
                Ok(ExprData::List(vec![ExprData::Identifier("unquote-splicing".to_string()).to_expr(),
                                       parse_item(token_stream)?].into_iter()).to_expr()),
        }
    } else {
        Err(BootLispError::new(ErrorType::Parse,
                               "Not yet implemented"))
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
                match final_element.expr_data {
                    ExprData::Nil =>
                        return Ok(ExprData::List(list_items.into_iter()).to_expr()),
                    ExprData::List(iter) => {
                        let mut collected: Vec<Expr> = iter.collect();
                        list_items.append(&mut collected);
                        return Ok(ExprData::List(list_items.into_iter()).to_expr())
                    },
                    ExprData::DottedList(iter, end) => {
                        let mut collected: Vec<Expr> = iter.collect();
                        list_items.append(&mut collected);
                        return Ok(ExprData::DottedList(list_items.into_iter(), end).to_expr())
                    },
                    _ => {
                        if Some(Token { token_type: TokenType::CloseBrace }).as_ref() != token_stream.next() {
                            panic!("Dotted item must be last in list")
                        }

                        return Ok(ExprData::DottedList(list_items.into_iter(), Box::new(final_element)).to_expr())
                    }
                }
            },
            _ => {
                list_items.push(parse_item(token_stream)?)
            }
        }
    }

    Err(BootLispError::new(ErrorType::Parse,
                           &format!("Unexpected end to token stream found while parsing list: {:?}", list_items)))
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

        assert_eq!(Ok(ExprData::Bool(true).to_expr()),
                   parse(vec![TokenType::Bool(true).to_token()]));
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

        assert_eq!(Ok(ExprData::DottedList(vec![ExprData::Integer(1).to_expr(),
                                                ExprData::Integer(2).to_expr(),
                                                ExprData::Integer(3).to_expr()].into_iter(),
                                           Box::new(ExprData::Integer(4).to_expr())).to_expr()),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Integer(1).to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::OpenBrace.to_token(),
                              TokenType::Integer(2).to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::OpenBrace.to_token(),
                              TokenType::Integer(3).to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::Integer(4).to_token(),
                              TokenType::CloseBrace.to_token(),
                              TokenType::CloseBrace.to_token(),
                              TokenType::CloseBrace.to_token()]));

        assert_eq!(Ok(ExprData::List(vec![ExprData::Integer(1).to_expr(),
                                          ExprData::Integer(2).to_expr(),
                                          ExprData::Integer(3).to_expr()].into_iter()).to_expr()),
                   parse(vec![TokenType::OpenBrace.to_token(),
                              TokenType::Integer(1).to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::OpenBrace.to_token(),
                              TokenType::Integer(2).to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::OpenBrace.to_token(),
                              TokenType::Integer(3).to_token(),
                              TokenType::Dot.to_token(),
                              TokenType::OpenBrace.to_token(),
                              TokenType::CloseBrace.to_token(),
                              TokenType::CloseBrace.to_token(),
                              TokenType::CloseBrace.to_token(),
                              TokenType::CloseBrace.to_token()]));
    }
}

