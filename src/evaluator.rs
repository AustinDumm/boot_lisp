
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

#[derive(Debug)]
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
    
    let mut eval_env = EvaluationEnvironment::new(expr, env);

    loop {
        if let Some(ref active_frame) = eval_env.active_frame {
            match &active_frame.expr_queue {
                ExprQueue::Expr(expr) => {
                    match expr.expr_data {
                        ExprData::Integer(value) => {
                            eval_env.accumulator = Some(ExprData::Integer(value).to_expr());
                            eval_env.pop_frame();
                        }
                        _ => {
                            panic!("Unhandled Expr type for evaluation")
                        }
                    }
                },
                ExprQueue::Queue(queue) => {
                    panic!("Unhandled expr queue for evaluation")
                },
            }
        } else {
            if let Some(result) = eval_env.accumulator {
                return Ok(result)
            } else {
                return Err(EvalError::new("Failed to find result in accumulator at end of evaluation"))
            }
        }
    }

    Err(EvalError::new("Not yet implemented"))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn evaluates_single_integer() {
        assert_eq!(
            eval(ExprData::Integer(52).to_expr(), Env::new()).expect("Failed to evaluate"),
            ExprData::Integer(52).to_expr()
        );
    }
}

