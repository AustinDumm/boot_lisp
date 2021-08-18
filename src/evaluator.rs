
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
};

#[derive(Debug, PartialEq)]
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
    
    let mut call_stack = CallStack::new();
    let mut accumulator: Option<Expr> = None;
    let mut active_frame: Option<StackFrame> = Some(StackFrame::new(ExprQueue::Expr(expr), env, vec![]));

    loop {
        if let Some(_active_frame) = active_frame {
            match _active_frame.expr_queue {
                ExprQueue::Expr(expr) => {
                    match expr.expr_data {
                        ExprData::Integer(_) |
                        ExprData::Nil |
                        ExprData::Lambda(_, _, _) => {
                            accumulator = Some(expr);
                            active_frame = call_stack.pop_frame();
                        }
                        ExprData::Identifier(name) => {
                            if let Some(expr_lock) = _active_frame.env.get(&name) {
                                accumulator = Some(expr_lock.read().unwrap().clone());
                                active_frame = call_stack.pop_frame()
                            } else {
                                return Err(EvalError::new(&format!("Failed to lookup value for identifier: {}", name)))
                            }
                        }
                        ExprData::DottedList(_, _) => {
                            return Err(EvalError::new("Cannot evaluate dotted list"));
                        }
                        ExprData::List(list) => {
                            active_frame = Some(StackFrame::new(ExprQueue::Queue(list.into_iter()),
                                                                                 _active_frame.env.clone(),
                                                                                 vec![]));
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
            if let Some(result) = accumulator {
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

    #[test]
    fn evaluates_single_lambda() {
        let env = Env::new();
        assert_eq!(
            eval(
                ExprData::Lambda(vec![ExprData::Identifier(String::from("test")).to_expr()],
                                 vec![ExprData::Integer(521).to_expr()],
                                 env.clone()).to_expr(),
                Env::new()).expect("Failed to evaluate"),
                ExprData::Lambda(vec![ExprData::Identifier(String::from("test")).to_expr()],
                                 vec![ExprData::Integer(521).to_expr()],
                                 env.clone()).to_expr()
        );
    }

    #[test]
    fn fails_to_evaluate_dotted_list() {
        assert_eq!(eval(ExprData::DottedList(vec![ExprData::Integer(2421).to_expr()],
                                             Box::new(ExprData::Identifier(String::from("testing")).to_expr())).to_expr(), Env::new()).unwrap_err(),
                   EvalError::new("Cannot evaluate dotted list"));
    }

    #[test]
    fn evaluates_identity_application() {
        assert_eq!(
            eval(ExprData::List(
                    vec![
                        ExprData::Lambda(vec![ExprData::Identifier(String::from("x")).to_expr()],
                                         vec![ExprData::Identifier(String::from("x")).to_expr()],
                                         Env::new()).to_expr(),
                        ExprData::Integer(823).to_expr()]).to_expr(),
                 Env::new()).expect("Failed to evaluate"),
            ExprData::Integer(823).to_expr()
        );
    }
}

