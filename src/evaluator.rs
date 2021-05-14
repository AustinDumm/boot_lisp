
use crate::parser::{
    Expr,
    ExprData,
};

pub struct EvalError {
    pub message: String,
}

impl EvalError {
    pub fn new(message: &str) -> EvalError {
        EvalError { message: String::from(message) }
    }
}

type EvalResult = Result<Expr, EvalError>;

pub fn eval(expr: Expr) -> EvalResult {
    Err(EvalError::new("Not yet implemented"))
}

