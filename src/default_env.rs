
use std::vec::IntoIter;
use std::process;

use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};

use crate::env::Env;

fn eval_arguments_excepting<F>(accumulator: &mut Option<Expr>,
                               frame: Option<StackFrame>,
                               stack: &mut CallStack,
                               exception_indices: Vec<usize>,
                               return_fn: F) -> Option<StackFrame>
where F: FnOnce(&mut Option<Expr>, IntoIter<Expr>, &mut CallStack) -> Option<StackFrame> {
    if let Some(frame) = frame {
        if let Expr { expr_data: ExprData::List(mut list) } = frame.expr {
            if let Some(next_expr) = list.next() {
                if exception_indices.contains(&(frame.rib.len() - 1)) {
                    let mut new_rib = frame.rib;
                    new_rib.push(next_expr);
                    Some(StackFrame::new(ExprData::List(list).to_expr(),
                                         frame.env.clone(),
                                         new_rib))
                } else {
                    stack.push_frame(StackFrame::new(ExprData::List(list).to_expr(),
                                                     frame.env.clone(),
                                                     frame.rib));
                    Some(StackFrame::new(next_expr,
                                         frame.env.clone(),
                                         vec![]))
                }
            } else {
                // Throw away the Function expr that starts the list
                let mut iter = frame.rib.into_iter();
                iter.next();

                return_fn(accumulator, iter, stack)
            }
        } else {
            panic!("Non-list expr type given to Function evaluation")
        }
    } else {
        panic!("No frame found for arguments evaluation")
    }
}

fn eval_arguments<F>(accumulator: &mut Option<Expr>,
                     frame: Option<StackFrame>,
                     stack: &mut CallStack,
                     return_fn: F) -> Option<StackFrame>
where F: FnOnce(&mut Option<Expr>, IntoIter<Expr>, &mut CallStack) -> Option<StackFrame> {
    eval_arguments_excepting(accumulator,
                             frame,
                             stack,
                             vec![],
                             return_fn)
}

fn eval_arguments_and_apply<F>(accumulator: &mut Option<Expr>,
                               frame: Option<StackFrame>,
                               stack: &mut CallStack,
                               application: F) -> Option<StackFrame>
where F: FnOnce(IntoIter<Expr>) -> Expr {
    eval_arguments(accumulator,
                   frame,
                   stack,
                   |accumulator, iter, stack| {
                       *accumulator = Some(application(iter));
                       stack.pop_frame()
                   })
}

fn eval_arguments_excepting_and_apply<F>(accumulator: &mut Option<Expr>,
                                         frame: Option<StackFrame>,
                                         stack: &mut CallStack,
                                         exception_indices: Vec<usize>,
                                         application: F) -> Option<StackFrame>
where F: FnOnce(IntoIter<Expr>) -> Expr {
    eval_arguments_excepting(accumulator,
                             frame,
                             stack,
                             exception_indices,
                             |accumulator, iter, stack| {
                                 *accumulator = Some(application(iter));
                                 stack.pop_frame()
                             })
}

fn eval_arguments_and_compare<F>(accumulator: &mut Option<Expr>,
                                 frame: Option<StackFrame>,
                                 stack: &mut CallStack,
                                 cmp: F) -> Option<StackFrame>
where F: Fn(&Expr, &Expr) -> bool {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let Some(mut current_expr) = iter.next() {
                                     while let Some(next_expr) = iter.next() {
                                         if cmp(&current_expr, &next_expr) {
                                             current_expr = next_expr
                                         } else {
                                             return ExprData::Bool(false).to_expr()
                                         }
                                     }
                                     ExprData::Bool(true).to_expr()
                                 } else {
                                     panic!("Mismatched arity. Minimum of 1 argument must be given to comparison function")
                                 }
                             })
}

//=============== Arithmetic Functions ===============
fn add(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| {
                                         match expr {
                                             Expr { expr_data: ExprData::Integer(value) } => value,
                                             _ => panic!("Integer type must be used for addition"),
                                         }
                                     }).reduce(|acc, val| { acc + val }).unwrap()
                                 ).to_expr()
                             })
}

fn sub(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| -> i32 {
                                         match expr {
                                             Expr { expr_data: ExprData::Integer(value) } => value,
                                             _ => panic!("Integer type must be used for subtraction"),
                                         }
                                     }).reduce(|acc, val| { acc - val }).unwrap()
                                 ).to_expr()
                             })
}

fn mul(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| -> i32 {
                                         match expr {
                                             Expr { expr_data: ExprData::Integer(value) } => value,
                                             _ => panic!("Integer type must be used for subtraction"),
                                         }
                                     }).reduce(|acc, val| { acc * val }).unwrap()
                                 ).to_expr()
                             })
}

fn div(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| -> i32 {
                                         match expr {
                                             Expr { expr_data: ExprData::Integer(value) } => value,
                                             _ => panic!("Integer type must be used for subtraction"),
                                         }
                                     }).reduce(|acc, val| { acc / val }).unwrap()
                                 ).to_expr()
                             })
}

fn modulo(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| -> i32 {
                                         match expr {
                                             Expr { expr_data: ExprData::Integer(value) } => value,
                                             _ => panic!("Integer type must be used for subtraction"),
                                         }
                                     }).reduce(|acc, val| { acc % val }).unwrap()
                                 ).to_expr()
                             })
}

//=============== Bitwise Functions ===============

fn bit_and(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| {
                                         match expr.expr_data {
                                             ExprData::Integer(integer) => integer,
                                             _ => panic!("bitwise and must take integer"),
                                         }
                                     }).reduce(|a, b| { a & b }).unwrap()
                                 ).to_expr()
                             })
}

fn bit_or(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| {
                                         match expr.expr_data {
                                             ExprData::Integer(integer) => integer,
                                             _ => panic!("bitwise and must take integer"),
                                         }
                                     }).reduce(|a, b| { a | b }).unwrap()
                                 ).to_expr()
                             })
}

fn bit_xor(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::Integer(
                                     iter.map(|expr| {
                                         match expr.expr_data {
                                             ExprData::Integer(integer) => integer,
                                             _ => panic!("bitwise and must take integer"),
                                         }
                                     }).reduce(|a, b| { a ^ b }).unwrap()
                                 ).to_expr()
                             })
}

fn bit_not(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 ExprData::Integer(
                                     if let (Some(Expr { expr_data: ExprData::Integer(integer) }), None) = (iter.next(), iter.next()) {
                                         !integer
                                     } else {
                                         panic!("bitwise not must take single, integer argument")
                                     }
                                 ).to_expr()
                             })
}

//=============== Comparison Functions ===============

fn lt(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_compare(accumulator,
                               frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs < rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn gt(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_compare(accumulator,
                               frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs > rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn leq(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_compare(accumulator,
                               frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs <= rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn geq(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_compare(accumulator,
                               frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs >= rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn eq(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_compare(accumulator,
                               frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs == rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

//=============== Lambda Creation ===============

fn build_lambda(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    if let Some(active_frame) = frame {
        match active_frame.expr.expr_data {
            ExprData::List(mut iter) => {
                let args_list = iter.next().unwrap();
                let body = iter.next().unwrap();
                *accumulator = Some(ExprData::Lambda(Box::new(args_list),
                                                     Box::new(body),
                                                     active_frame.env.clone()).to_expr());
                stack.pop_frame()
            },
            _ => panic!("Lambda must take list of information"),
        }
    } else {
        panic!("No frame found during evaluation of lambda")
    }
}

//=============== Conditional Evaluation ===============

fn if_impl(_accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    if let Some(active_frame) = frame {
        let mut rib_iter = active_frame.rib.iter();
        // Drop function value from iter
        rib_iter.next();

        if let Some(Expr { expr_data: conditional_data }) = rib_iter.next() {
            // Conditional has been evaluated
            match (conditional_data, active_frame.expr.expr_data) {
                (ExprData::Bool(true), ExprData::List(mut iter)) => {
                    let true_expr = iter.next().unwrap();

                    Some(StackFrame::new(true_expr,
                                         active_frame.env.clone(),
                                         vec![]))
                },
                (ExprData::Bool(false), ExprData::List(mut iter)) => {
                    iter.next();
                    let false_expr = iter.next().unwrap();

                    Some(StackFrame::new(false_expr,
                                         active_frame.env.clone(),
                                         vec![]))
                },
                (ExprData::Bool(_), other) => {
                    panic!("if expressions must contain list. Found: {:?}", other)
                },
                (other, _) => {
                    panic!("First argument to an if expression must be boolean. Found: {:?}", other)
                },
            }
        } else {
            // Conditional needs evaluation
            match active_frame.expr.expr_data {
                ExprData::List(mut iter) => {
                    let conditional_expr = iter.next().unwrap();

                    stack.push_frame(StackFrame::new(ExprData::List(iter).to_expr(),
                                                     active_frame.env.clone(),
                                                     active_frame.rib));

                    Some(StackFrame::new(conditional_expr,
                                         active_frame.env.clone(),
                                         vec![]))
                }
                other => panic!("if expressions must be passed arguments via list. Found: {:?}", other),
            }
        }
    } else {
        panic!("No stack frame found for \"if\" evaluation")
    }
}

//=============== List Manipulation ===============

fn cons(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(first), Some(rest), None) = (iter.next(), iter.next(), iter.next()) {
                                     match rest.expr_data {
                                         ExprData::List(rest) => {
                                             let mut array = vec![first];
                                             array.extend(rest);
                                             ExprData::List(array.into_iter()).to_expr()
                                         },
                                         ExprData::Nil => ExprData::List(vec![first].into_iter()).to_expr(),
                                         _ => ExprData::DottedList(vec![first].into_iter(), Box::new(rest)).to_expr(),
                                     }
                                 } else {
                                     panic!("Invalid number of arguments provided to cons. Requires 2")
                                 }
                             })
}

fn first(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::List(mut iter) => {
                                             iter.next().expect("No item found in list while evaluating 'first'")
                                         },
                                         _ => panic!("'first' must be given list")
                                     }
                                 } else {
                                     panic!("Invalid number of arguments provided to first. Requires 1")
                                 }
                             })
}

fn rest(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::List(mut iter) => {
                                             iter.next();
                                             if iter.clone().peekable().peek().is_none() {
                                                 ExprData::Nil.to_expr()
                                             } else {
                                                 ExprData::List(iter).to_expr()
                                             }
                                         },
                                         _ => panic!("'rest' must be given a list")
                                     }
                                 } else {
                                     panic!("Invalid number of arguments provided to rest. Requires 1")
                                 }
                             })
}

fn list_impl(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::List(iter).to_expr()
                             })
}

fn append_impl(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    fn concatenate_lists(iter: IntoIter<Expr>) -> Expr {
        let mut iter = iter.peekable();

        let mut append_vec: Vec<Expr> = vec![];
        while let Some(next_expr) = iter.next() {
            if iter.peek().is_none() {
                match next_expr.expr_data {
                    ExprData::List(_) => (),
                    ExprData::Nil => return ExprData::List(append_vec.into_iter()).to_expr(),
                    ExprData::DottedList(list, last) => {
                        append_vec.extend(list);
                        return ExprData::DottedList(append_vec.into_iter(), last).to_expr()
                    }
                    _ => return ExprData::DottedList(append_vec.into_iter(), Box::new(next_expr)).to_expr(),
                }
            }
            
            if let ExprData::List(iter) = next_expr.expr_data {
                append_vec.extend(iter);
            } else {
                panic!("Non-list provided as non-last element to append")
            }
        }

        ExprData::List(append_vec.into_iter()).to_expr()
    }

    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 concatenate_lists(iter)
                             })
}

//=============== Quote Functions ===============

pub fn quote(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    if let Some(active_frame) = frame {
        if let Expr { expr_data: ExprData::List(list) } = &active_frame.expr {
            let mut list = list.clone();
            if let (Some(quoted_item), None) = (list.next(), list.next()) {
                *accumulator = Some(quoted_item);
                stack.pop_frame()
            } else {
                panic!("Invalid number of arguments provided to quote")
            }
        } else {
            panic!("No arguments provided to quote")
        }
    } else {
        panic!("No frame found for evaluation of quoted expression")
    }
}

pub fn quasiquote(_accumulator: &mut Option<Expr>, frame: Option<StackFrame>, _stack: &mut CallStack) -> Option<StackFrame> {
    fn apply_quasiquote(expr: Expr, depth: usize) -> Expr {
        match expr.expr_data {
            ExprData::List(mut iter) => {
                match iter.next() {
                    Some(Expr { expr_data: ExprData::Identifier(identifier) }) 
                        if identifier.as_str() == "quasiquote" => {
                            let mut expr_list = vec![ExprData::Identifier("cons".to_string()).to_expr()];
                            expr_list.push(ExprData::Identifier("quasiquote".to_string()).to_expr().quoted());
                            if iter.clone().peekable().peek().is_some() {
                                expr_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth + 1));
                            } else {
                                expr_list.push(apply_quasiquote(ExprData::Nil.to_expr(), depth + 1));
                            }
                            ExprData::List(expr_list.into_iter()).to_expr()
                    },
                    Some(Expr { expr_data: ExprData::Identifier(identifier) })
                        if identifier.as_str() == "unquote" || identifier.as_str() == "unquote-splicing" => {
                            if depth > 0 {
                                let mut expr_list = vec![ExprData::Identifier("cons".to_string()).to_expr()];
                                expr_list.push(ExprData::Identifier(identifier).to_expr().quoted());
                                if iter.clone().peekable().peek().is_some() {
                                    expr_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth - 1));
                                } else {
                                    expr_list.push(apply_quasiquote(ExprData::Nil.to_expr(), depth - 1));
                                }
                                ExprData::List(expr_list.into_iter()).to_expr()
                            } else if let (Some(next), None, true) = (iter.next(), iter.next(), identifier.as_str() == "unquote") {
                                next
                            } else {
                                panic!("Illegal use of {} in quasiquoted expression", identifier)
                            }
                    },
                    Some(expr) => {
                        let mut expr_list = vec![ExprData::Identifier("append".to_string()).to_expr()];
                        expr_list.push(apply_quasiquote_list(expr, depth));
                        if iter.clone().peekable().peek().is_some() {
                            expr_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth));
                        }
                        ExprData::List(expr_list.into_iter()).to_expr()
                    },
                    None => panic!("Unexpected end to expression list found while parsing quasiquote")
                }
            },
            expr => expr.to_expr().quoted()
        }
    }

    fn apply_quasiquote_list(expr: Expr, depth: usize) -> Expr {
        match expr.expr_data {
            ExprData::List(mut iter) => {
                match iter.next() {
                    Some(Expr { expr_data: ExprData::Identifier(identifier) })
                        if identifier.as_str() == "quasiquote" => {
                            let mut expr_list = vec![ExprData::Identifier("list".to_string()).to_expr()];
                            let mut inner_list = vec![ExprData::Identifier("cons".to_string()).to_expr(),
                                                      ExprData::Identifier("quasiquote".to_string()).to_expr().quoted()];
                            if iter.clone().peekable().peek().is_some() {
                                inner_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth + 1));
                            } else {
                                inner_list.push(apply_quasiquote(ExprData::Nil.to_expr(), depth + 1));
                            }
                            expr_list.push(ExprData::List(inner_list.into_iter()).to_expr());
                            ExprData::List(expr_list.into_iter()).to_expr()
                    },
                    Some(Expr { expr_data: ExprData::Identifier(identifier) })
                        if identifier.as_str() == "unquote" || identifier.as_str() == "unquote-splicing" => {
                            if depth > 0 {
                                let mut expr_list = vec![ExprData::Identifier("list".to_string()).to_expr()];
                                let mut inner_list = vec![ExprData::Identifier("cons".to_string()).to_expr(),
                                                          ExprData::Identifier(identifier).to_expr().quoted()];
                                if iter.clone().peekable().peek().is_some() {
                                    inner_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth - 1));
                                } else {
                                    inner_list.push(apply_quasiquote(ExprData::Nil.to_expr(), depth - 1));
                                }
                                expr_list.push(ExprData::List(inner_list.into_iter()).to_expr());
                                ExprData::List(expr_list.into_iter()).to_expr()
                            } else if identifier.as_str() == "unquote" {
                                let mut expr_list = vec![ExprData::Identifier("list".to_string()).to_expr()];
                                expr_list.extend(iter);
                                ExprData::List(expr_list.into_iter()).to_expr()
                            } else {
                                let mut expr_list = vec![ExprData::Identifier("append".to_string()).to_expr()];
                                expr_list.extend(iter);
                                ExprData::List(expr_list.into_iter()).to_expr()
                            }
                    },
                    Some(expr) => {
                        let mut expr_data = vec![ExprData::Identifier("list".to_string()).to_expr()];
                        let mut inner_list = vec![ExprData::Identifier("append".to_string()).to_expr()];
                        inner_list.push(apply_quasiquote_list(expr, depth));
                        if iter.clone().peekable().peek().is_some() {
                            inner_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth));
                        } else {
                            inner_list.push(apply_quasiquote(ExprData::Nil.to_expr(), depth));
                        }
                        expr_data.push(ExprData::List(inner_list.into_iter()).to_expr());
                        ExprData::List(expr_data.into_iter()).to_expr()
                    },
                    None => panic!("Unexpected end to list while applying quasiquoted list")
                }
            },
            expr => ExprData::List(vec![expr.to_expr()].into_iter()).to_expr().quoted(),
        }
    }

    match frame {
        Some(
            StackFrame { expr: Expr { expr_data: ExprData::List(mut iter) },
                         env,
                         rib: _ }) =>  {
            if let (Some(expr), None) = (iter.next(), iter.next()) {
                let quasiquoted_expr = apply_quasiquote(expr, 0);
                Some(StackFrame::new(quasiquoted_expr, env.clone(), vec![]))
            } else {
                panic!("Error: Invalid arity to quasiquote. Quasiquote takes only one argument")
            }
        },
        _ => panic!("Unexpected expr of non-list type for quasiquote")
    }
}

fn unquote(_accumulator: &mut Option<Expr>, _frame: Option<StackFrame>, _stack: &mut CallStack) -> Option<StackFrame> {
    panic!("Unexpected unquote found outside of quasiquoted expression")
}

fn unquote_splicing(_accumulator: &mut Option<Expr>, _frame: Option<StackFrame>, _stack: &mut CallStack) -> Option<StackFrame> {
    panic!("Unexpected unquote-splicing found outside of quasiquoted list expression")
}

//=============== Environment Manipulation ===============

fn set(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let env = frame.as_ref().unwrap().env.clone();
    eval_arguments_excepting_and_apply(accumulator,
                                       frame,
                                       stack,
                                       vec![0],
                                       |mut iter| {
                                           if let (Some(Expr { expr_data: ExprData::Identifier(identifier) }),
                                                   Some(expr),
                                                   None) = (iter.next(), iter.next(), iter.next()) {
                                               let success = env.set(identifier, expr);
                                               ExprData::Bool(success).to_expr()
                                           } else {
                                               panic!("Unexpected argument list passed to set!")
                                           }
                                       })
}

fn create(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let env = frame.as_ref().unwrap().env.clone();
    eval_arguments_excepting_and_apply(accumulator,
                                       frame,
                                       stack,
                                       vec![0],
                                       |mut iter| {
                                           if let (Some(Expr { expr_data: ExprData::Identifier(identifier) }),
                                                   Some(expr),
                                                   None) = (iter.next(), iter.next(), iter.next()) {
                                               env.create(identifier, expr);
                                               ExprData::Bool(true).to_expr()
                                           } else {
                                               panic!("Unexpected argument list passed to create!")
                                           }
                                       })

}

//=============== Evaluation Control ===============

fn begin(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 let mut iter = iter.peekable();
                                 let mut value = iter.next();
                                 while iter.peek().is_some() {
                                     value = iter.next();
                                 }
                                 value.expect("Unexpected end to list found when evaluating begin")
                             })
}

fn eval(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let env = frame.as_ref().unwrap().env.clone();
    eval_arguments(accumulator,
                   frame,
                   stack,
                   |_, mut iter, _| {
                       if let (Some(expr), None) = (iter.next(), iter.next()) {
                           Some(StackFrame {
                                    expr,
                                    env, 
                                    rib: vec![]
                           })
                       } else {
                           panic!("Incorrect number of arguments provided to eval. Expected 1")
                       }
                   })
}

fn exit(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 let first = iter.next();
                                 if first.is_none() {
                                     process::exit(0);
                                 } else if let (Some(Expr { expr_data: ExprData::Integer(value) }), None) = (first, iter.next()) {
                                     process::exit(value);
                                 } else {
                                     panic!("Incorrect arguments list provided to exit. Expected 1 Integer")
                                 }
                             })
}

//=============== Environment Creation ===============
pub fn default_env() -> Env {
    Env::containing(
        vec![
            ("+".to_string(), ExprData::Function("+".to_string(), add).to_expr()),
            ("-".to_string(), ExprData::Function("-".to_string(), sub).to_expr()),
            ("*".to_string(), ExprData::Function("*".to_string(), mul).to_expr()),
            ("/".to_string(), ExprData::Function("/".to_string(), div).to_expr()),
            ("%".to_string(), ExprData::Function("%".to_string(), modulo).to_expr()),

            ("bit-and".to_string(), ExprData::Function("bit-and".to_string(), bit_and).to_expr()),
            ("bit-or".to_string(), ExprData::Function("bit-or".to_string(), bit_or).to_expr()),
            ("bit-xor".to_string(), ExprData::Function("bit-xor".to_string(), bit_xor).to_expr()),
            ("bit-not".to_string(), ExprData::Function("bit-not".to_string(), bit_not).to_expr()),

            ("<".to_string(), ExprData::Function("<".to_string(), lt).to_expr()),
            (">".to_string(), ExprData::Function(">".to_string(), gt).to_expr()),
            ("<=".to_string(), ExprData::Function("<=".to_string(), leq).to_expr()),
            (">=".to_string(), ExprData::Function(">=".to_string(), geq).to_expr()),
            ("=".to_string(), ExprData::Function("=".to_string(), eq).to_expr()),

            ("cons".to_string(), ExprData::Function("cons".to_string(), cons).to_expr()),
            ("first".to_string(), ExprData::Function("first".to_string(), first).to_expr()),
            ("rest".to_string(), ExprData::Function("rest".to_string(), rest).to_expr()),
            ("list".to_string(), ExprData::Function("list".to_string(), list_impl).to_expr()),
            ("append".to_string(), ExprData::Function("append".to_string(), append_impl).to_expr()),

            ("quote".to_string(), ExprData::Function("quote".to_string(), quote).to_expr()),
            ("quasiquote".to_string(), ExprData::Function("quasiquote".to_string(), quasiquote).to_expr()),
            ("unquote".to_string(), ExprData::Function("unquote".to_string(), unquote).to_expr()),

            ("set!".to_string(), ExprData::Function("set!".to_string(), set).to_expr()),
            ("create!".to_string(), ExprData::Function("create!".to_string(), create).to_expr()),

            ("begin".to_string(), ExprData::Function("begin".to_string(), begin).to_expr()),
            ("eval".to_string(), ExprData::Function("eval".to_string(), eval).to_expr()),
            ("exit".to_string(), ExprData::Function("exit".to_string(), exit).to_expr()),

            ("lambda".to_string(), ExprData::Function("lambda".to_string(), build_lambda).to_expr()),
            ("if".to_string(), ExprData::Function("if".to_string(), if_impl).to_expr()),
        ].into_iter().collect()
    )
}

