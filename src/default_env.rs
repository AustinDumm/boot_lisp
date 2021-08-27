
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


fn eval_arguments_and_apply<F>(frame: StackFrame, 
                               stack: &mut CallStack, 
                               application: F) -> StackFrame
                               where F: FnOnce(IntoIter<Expr>) -> Expr {
    if let Expr { expr_data: ExprData::List(mut list) } = frame.expr {
        if let Some(next_expr) = list.next() {
            stack.push_frame(StackFrame::new(ExprData::List(list).to_expr(),
                                             frame.env.clone(),
                                             frame.rib));
            StackFrame::new(next_expr,
                            frame.env.clone(),
                            vec![])
        } else {
            // Throw away the Function expr that starts the list
            let mut iter = frame.rib.into_iter();
            iter.next();

            let result = application(iter);
            StackFrame::new(result, frame.env, vec![])
        }
    } else {
        panic!("Non-list expr type given to Function evaluation")
    }
}

//=============== Arithmetic Functions ===============
fn add(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_apply(frame,
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

fn sub(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_apply(frame,
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

fn mul(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_apply(frame,
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

fn div(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_apply(frame,
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

fn modulo(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_apply(frame,
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


//=============== Environment Creation ===============
pub fn default_env() -> Env {
    Env::containing(
        vec![
            ("+".to_string(), ExprData::Function("+".to_string(), add).to_expr()),
            ("-".to_string(), ExprData::Function("-".to_string(), sub).to_expr()),
            ("*".to_string(), ExprData::Function("*".to_string(), mul).to_expr()),
            ("/".to_string(), ExprData::Function("/".to_string(), div).to_expr()),
            ("%".to_string(), ExprData::Function("%".to_string(), modulo).to_expr()),
        ].into_iter().collect()
    )
}

