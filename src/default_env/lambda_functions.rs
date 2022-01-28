
use super::{
    env_utilities::*,
    eval_functions,
};
use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn build_lambda(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    if let Some(active_frame) = frame {
        match active_frame.expr.expr_data {
            ExprData::List(mut iter) => {
                let args_list = iter.next().unwrap();
                let body_iter = iter;
                let body = vec![ExprData::Function("begin".to_string(), eval_functions::begin).to_expr()]
                    .into_iter()
                    .chain(body_iter)
                    .collect::<Vec<Expr>>()
                    .into_iter();
                *accumulator = Some(ExprData::Lambda(Box::new(args_list),
                                                     Box::new(ExprData::List(body).to_expr()),
                                                     active_frame.env.clone()).to_expr());
                stack.pop_frame()
            },
            _ => panic!("Lambda must take list of information"),
        }
    } else {
        panic!("No frame found during evaluation of lambda")
    }
}

pub fn build_void(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if iter.next().is_none() {
                                     ExprData::Void.to_expr()
                                 } else {
                                     panic!("void function takes no arguments to create void value")
                                 }
                             })
}

