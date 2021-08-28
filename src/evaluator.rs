
use crate::parser::{
    Expr,
    ExprData,
};

use crate::env::{
    Env,
};

use crate::call_stack::{
    StackFrame,
    CallStack,
};


/// Error type to be returned when evaluation fails. Contains a message describing the cause of the
/// evaluation failure
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

/// Iteratively evaluates arbitrary boot lisp expressions
///
/// Evaluates iteratively rather than relying on host language's call stack in order to provide
/// future support for first-class continuations
pub fn eval(expr: Expr, env: Env) -> EvalResult {
    
    let mut call_stack = CallStack::new();
    let mut accumulator: Option<Expr> = None;
    let mut active_frame: Option<StackFrame> = Some(StackFrame::new(expr, env, vec![]));

    loop {
        match active_frame {
            // If there is no active frame, the accumulator must contain the final expr
            None => {
                if let Some(result) = accumulator {
                    return Ok(result)
                } else {
                    return Err(EvalError::new("Failed to find result in accumulator at end of evaluation"))
                }
            },
            
            // If the active frame expr is a primitive, set the accumulator to the primitive and
            // pop the current stack frame as it is no longer needed. This "passes" the current
            // frame's value up to the previous frame for further use
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::Bool(_) },
                    env: _,
                    rib: _,
                }
            ) |
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::Integer(_) }, 
                    env: _, 
                    rib: _ 
                }
            ) |
            Some(
                StackFrame { 
                    expr: Expr { expr_data: ExprData::Nil },
                    env: _,
                    rib: _
                }
            ) |
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::Lambda(_, _, _) },
                    env: _,
                    rib: _
                }
            ) |
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::Function(_, _) },
                    env: _,
                    rib: _
                }
            ) => {
                accumulator = Some(active_frame.unwrap().expr);
                active_frame = call_stack.pop_frame();
            },

            // If the active frame expr is a quoted expression, set the accumulator to the
            // contained expr and pop the current stack frame as it is no longer needed. This
            // "passes" the contained expression up to the previous frame without evaluation to
            // accomplish the intention of a quoted expr
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::Quote(quoted_expr) },
                    env: _,
                    rib: _
                }
            ) => {
                accumulator = Some(*quoted_expr);
                active_frame = call_stack.pop_frame();
            },

            // If the active frame expr is an identifier, use the current environment to lookup the
            // expr bound to that identifier. Set the accumulator to the bound expr and pop the
            // current frame as it is no longer needed. This "passes" the bound expr up to the
            // previous frame for further use
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::Identifier(name) },
                    env,
                    rib: _
                }
            ) => {
                if let Some(expr_lock) = env.get(&name) {
                    accumulator = Some(expr_lock.read().unwrap().clone());
                    active_frame = call_stack.pop_frame()
                } else {
                    return Err(EvalError::new(&format!("Failed to lookup value for identifier: {}", name)))
                }
            },

            // Dotted lists cannot be evaluated as they hold no meaning in evaluation. Return error
            // with a description of this failure
            // If the dotted list ends with a nil, it is effectively a well-formed S-Exp list,
            // convert to such and continue
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::DottedList(list, end) },
                    env,
                    rib,
                }
            ) => {
                if end.expr_data == ExprData::Nil {
                    active_frame = Some(StackFrame {
                                            expr: ExprData::List(list).to_expr(),
                                            env: env,
                                            rib: rib
                    });
                } else {
                    return Err(EvalError::new("Cannot evaluate dotted list"));
                }
            },

            // If the current frame expr is a list, need to execute the steps necessary for
            // function argument evaluation and application
            Some(
                StackFrame {
                    expr: Expr { expr_data: ExprData::List(mut list) },
                    env,
                    mut rib 
                }
            ) => {
                if let Some(acc_expr) = accumulator {
                    // If the current frame expr is a list and the accumulator is Some, an intermediate
                    // evaluation has occurred and is waiting in the accumulator. Append the expr in the
                    // accumulator to this frame's rib and set the accumulator to None then loop to
                    // continue evaluation

                    rib.push(acc_expr);
                    accumulator = None;
                    active_frame = Some(
                        StackFrame::new(ExprData::List(list).to_expr(),
                                        env,
                                        rib));
                } else {
                    // If the current frame expr is a list and the accumulator is None, we are ready to
                    // evaluate the next item in the list.
                    // If there is a "next" item:
                    //      Push this frame onto the stack
                    //      Create a new stack frame with the "next" item as its expr and set as
                    //          active_frame
                    // If there is no next item in the list:
                    //      Execute function application:
                    //          Retrieve first item as the applicable
                    //          Create a new env extending the applicable's environment with the bindings
                    //              from the applicable and the remaining rib exprs
                    //          Create a new frame containing the applicable's body and the new env
                    //          Set the active_frame to this new frame
                    //          Loop to continue evaluation
                    if let Some(Expr { expr_data: ExprData::Function(_, fn_ptr) }) = rib.first() {
                        active_frame = Some(
                            fn_ptr(StackFrame::new(ExprData::List(list).to_expr(),
                                                   env,
                                                   rib), 
                                   &mut call_stack));
                    } else if let Some(next_expr) = list.next() {
                        call_stack.push_frame(StackFrame::new(ExprData::List(list).to_expr(),
                                                              env.clone(),
                                                              rib));
                        active_frame = Some(StackFrame::new(next_expr,
                                                            env.clone(),
                                                            vec![]));
                    } else {
                        let mut rib_iter = rib.into_iter();
                        if let Some(Expr { expr_data: ExprData::Lambda(args, body, closure_env) }) =
                            rib_iter.next() {
                                let new_env = closure_env.extend((*args).clone(), 
                                                                 ExprData::List(rib_iter).to_expr());
                                active_frame = Some(StackFrame::new(*body, new_env, vec![]));
                        } else {
                            return Err(EvalError::new("Application attempted with non-applicable first element in list"));
                        }
                    }
                }
            },
        }
    }
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
                ExprData::ident_from("test").to_expr(),
                Env::containing(vec![
                    (String::from("test"), ExprData::Integer(645).to_expr())
                ].into_iter().collect())).expect("Failed to evaluate"),
            ExprData::Integer(645).to_expr());
    }

    #[test]
    fn evaluates_single_lambda() {
        let env = Env::new();
        assert_eq!(
            eval(
                ExprData::Lambda(Box::new(ExprData::List(vec![ExprData::Identifier(String::from("test")).to_expr()].into_iter()).to_expr()),
                                 Box::new(ExprData::Integer(521).to_expr()),
                                 env.clone()).to_expr(),
                Env::new()).expect("Failed to evaluate"),
                ExprData::Lambda(Box::new(ExprData::List(vec![ExprData::Identifier(String::from("test")).to_expr()].into_iter()).to_expr()),
                                 Box::new(ExprData::Integer(521).to_expr()),
                                 env.clone()).to_expr()
        );
    }

    #[test]
    fn fails_to_evaluate_dotted_list() {
        assert_eq!(eval(ExprData::DottedList(vec![ExprData::Integer(2421).to_expr()].into_iter(),
                                             Box::new(ExprData::Identifier(String::from("testing")).to_expr())).to_expr(), Env::new()).unwrap_err(),
                   EvalError::new("Cannot evaluate dotted list"));
    }

    #[test]
    fn evaluates_identity_application() {
        assert_eq!(
            eval(ExprData::List(
                    vec![
                        ExprData::Lambda(Box::new(ExprData::List(vec![ExprData::Identifier(String::from("x")).to_expr()].into_iter()).to_expr()),
                                         Box::new(ExprData::Identifier(String::from("x")).to_expr()),
                                         Env::new()).to_expr(),
                        ExprData::Integer(823).to_expr()].into_iter()).to_expr(),
                 Env::new()).expect("Failed to evaluate"),
            ExprData::Integer(823).to_expr()
        );
    }

    #[test]
    fn evaluates_quoted_expr() {
        assert_eq!(
            eval(ExprData::Quote(Box::new(Expr::form_list(vec![ExprData::Integer(5), ExprData::Integer(8)]))).to_expr(),
                 Env::new()).expect("Failed to evaluate"),
            Expr::form_list(vec![ExprData::Integer(5), ExprData::Integer(8)]));
    }

    #[test]
    fn evaluates_addition_function() {
    use crate::default_env;
        assert_eq!(
            eval(Expr::form_list(vec![ExprData::ident_from("+"),
                                      ExprData::Integer(1),
                                      ExprData::Integer(2),
                                      ExprData::Integer(3)]),
                 default_env::default_env()).expect("Failed to evaluate"),
            ExprData::Integer(6).to_expr());
    }
}

