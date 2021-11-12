
use std::vec::IntoIter;

use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};

use crate::env::{
    Env,
};


fn eval_arguments_and_apply<F>(accumulator: &mut Option<Expr>,
                               frame: Option<StackFrame>,
                               stack: &mut CallStack,
                               application: F) -> Option<StackFrame>
where F: FnOnce(IntoIter<Expr>) -> Expr {
    if let Some(frame) = frame {
        if let Expr { expr_data: ExprData::List(mut list) } = frame.expr {
            if let Some(next_expr) = list.next() {
                stack.push_frame(StackFrame::new(ExprData::List(list).to_expr(),
                                                 frame.env.clone(),
                                                 frame.rib));
                Some(StackFrame::new(next_expr,
                                     frame.env.clone(),
                                     vec![]))
            } else {
                // Throw away the Function expr that starts the list
                let mut iter = frame.rib.into_iter();
                iter.next();

                *accumulator = Some(application(iter));
                stack.pop_frame()
            }
        } else {
            panic!("Non-list expr type given to Function evaluation")
        }
    } else {
        panic!("No frame found for arguments evaluation")
    }
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


//=============== Comparison Functions ===============

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
                    _ => return ExprData::DottedList(append_vec.into_iter(), Box::new(next_expr)).to_expr(),
                }
            }
            
            if let ExprData::List(iter) = next_expr.expr_data {
                append_vec.extend(iter);
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
    // If list, output (list . (#recursive quasiquote eval of every item in list))
    //      If first item is "unquote", return the next item in the list
    //      Else, return '(list . ("recursive quasiquote eval of every item in list"))
    // Else, return 'item
    fn apply_quasiquote(expr: Expr) -> Expr {
        match expr.expr_data {
            ExprData::List(mut iter) => {
                let mut peekable = iter.clone().peekable();
                if Some(&Expr { expr_data: ExprData::Identifier("unquote".to_string()) }) == peekable.peek() {
                    peekable.next();
                    peekable.next().expect("Failed to find expression following unquote")
                } else {
                    let mut result_list: Vec<Expr> = vec![];
                    result_list.push(ExprData::Function("list".to_string(), list_impl).to_expr());
                    while let Some(expr) = iter.next() {
                        result_list.push(apply_quasiquote(expr));
                    }

                    ExprData::List(result_list.into_iter()).to_expr()
                }
            },
            _ => expr.quoted()
        }
    }

    match frame {
        Some(
            StackFrame { expr: Expr { expr_data: ExprData::List(mut iter) },
                         env,
                         rib: _ }) =>  {
            if let (Some(expr), None) = (iter.next(), iter.next()) {
                let quasiquoted_expr = apply_quasiquote(expr);
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

//=============== Environment Creation ===============
pub fn default_env() -> Env {
    Env::containing(
        vec![
            ("+".to_string(), ExprData::Function("+".to_string(), add).to_expr()),
            ("-".to_string(), ExprData::Function("-".to_string(), sub).to_expr()),
            ("*".to_string(), ExprData::Function("*".to_string(), mul).to_expr()),
            ("/".to_string(), ExprData::Function("/".to_string(), div).to_expr()),
            ("%".to_string(), ExprData::Function("%".to_string(), modulo).to_expr()),

            ("<".to_string(), ExprData::Function("<".to_string(), lt).to_expr()),
            (">".to_string(), ExprData::Function(">".to_string(), gt).to_expr()),
            ("<=".to_string(), ExprData::Function("<=".to_string(), leq).to_expr()),
            (">=".to_string(), ExprData::Function(">=".to_string(), geq).to_expr()),
            ("=".to_string(), ExprData::Function("=".to_string(), eq).to_expr()),

            ("cons".to_string(), ExprData::Function("cons".to_string(), cons).to_expr()),
            ("list".to_string(), ExprData::Function("list".to_string(), list_impl).to_expr()),
            ("append".to_string(), ExprData::Function("append".to_string(), append_impl).to_expr()),

            ("quote".to_string(), ExprData::Function("quote".to_string(), quote).to_expr()),
            ("quasiquote".to_string(), ExprData::Function("quasiquote".to_string(), quasiquote).to_expr()),
            ("unquote".to_string(), ExprData::Function("unquote".to_string(), unquote).to_expr()),

            ("lambda".to_string(), ExprData::Function("lambda".to_string(), build_lambda).to_expr()),
            ("if".to_string(), ExprData::Function("if".to_string(), if_impl).to_expr()),
        ].into_iter().collect()
    )
}

