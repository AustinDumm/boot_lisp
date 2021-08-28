
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


//=============== Comparison Functions ===============

fn eval_arguments_and_compare<F>(frame: StackFrame, stack: &mut CallStack, cmp: F) -> StackFrame
where F: Fn(&Expr, &Expr) -> bool {
    eval_arguments_and_apply(frame,
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

fn lt(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_compare(frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs < rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn gt(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_compare(frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs > rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn leq(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_compare(frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs <= rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn geq(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_compare(frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs >= rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

fn eq(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    eval_arguments_and_compare(frame,
                               stack,
                               |lhs, rhs| {
                                   match (&lhs.expr_data, &rhs.expr_data) {
                                       (ExprData::Integer(lhs), ExprData::Integer(rhs)) => lhs == rhs,
                                       (_, _) => panic!("Integer type must be used for comparison")
                                   }
                               })
}

//=============== Lambda Creation ===============

fn build_lambda(frame: StackFrame, _stack: &mut CallStack) -> StackFrame {
    match frame.expr.expr_data {
        ExprData::List(mut iter) => {
            let args_list = iter.next().unwrap();
            let body = iter.next().unwrap();
            StackFrame::new(ExprData::Lambda(Box::new(args_list),
                                             Box::new(body),
                                             frame.env.clone()).to_expr(),
                            frame.env,
                            vec![])
        },
        _ => panic!("Lambda must take list of information"),
    }
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

            ("lambda".to_string(), ExprData::Function("lambda".to_string(), build_lambda).to_expr()),
        ].into_iter().collect()
    )
}

