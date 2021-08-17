
use crate::parser::{
    Expr,
    ExprData,
};

use crate::env::{
    Env,
};

use crate::call_stack::{
    ExprQueue,
    StackFrame,
    CallStack,
    EvaluationEnvironment,
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

pub fn eval(expr: Expr, env: Env) -> EvalResult {
    
    let mut accumulator: Option<Expr> = None;
    let mut next_expr: ExprQueue = ExprQueue::Expr(expr);
    let mut current_env: Env = env;
    let mut current_rib: Vec<Expr> = vec![];
    let mut stack: Vec<StackFrame> = vec![];

    loop {
        match next_expr {
            ExprQueue::Expr(expr) => {
                panic!("Unhandled expr type for evaluation")
            },
            ExprQueue::Queue(queue) => {
                panic!("Unhandled expr queue for evaluation")
            },
        }
    }

    Err(EvalError::new("Not yet implemented"))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env() {
        
    }
}

