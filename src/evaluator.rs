
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
                    match &expr.expr_data {
                        ExprData::Integer(value) => {
                            eval_env.accumulator = Some(ExprData::Integer(*value).to_expr());
                            eval_env.pop_frame();
                        }
                        ExprData::Nil => {
                            eval_env.accumulator = Some(ExprData::Nil.to_expr());
                            eval_env.pop_frame();
                        }
                        ExprData::Identifier(name) => {
                            if let Some(expr_lock) = active_frame.env.get(&name) {
                                eval_env.accumulator = Some(expr_lock.read().unwrap().clone());
                                eval_env.pop_frame()
                            } else {
                                return Err(EvalError::new(&format!("Failed to lookup value for identifier: {}", name)))
                            }
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

    use std::sync::{
        Arc,
        RwLock,
    };

    #[test]
    fn evaluates_single_integer() {
        assert_eq!(
            eval(ExprData::Integer(52).to_expr(), Env::new()).expect("Failed to evaluate"),
            ExprData::Integer(52).to_expr()
        );
    }

    #[test]
    fn evaluates_single_nil() {
        assert_eq!(
            eval(ExprData::Nil.to_expr(), Env::new()).expect("Failed to evaluate"),
            ExprData::Nil.to_expr()
        );
    }

    #[test]
    fn evaluates_single_ident_lookup() {
        assert_eq!(
            eval(
                ExprData::Identifier(String::from("test")).to_expr(),
                Env::containing(vec![
                    (String::from("test"), Arc::new(RwLock::new(ExprData::Integer(645).to_expr())))
                ].into_iter().collect())).expect("Failed to evaluate"),
            ExprData::Integer(645).to_expr());
    }
}

