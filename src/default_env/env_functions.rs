
use super::env_utilities::*;
use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn set(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

pub fn create(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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
                                               ExprData::Void.to_expr()
                                           } else {
                                               panic!("Unexpected argument list passed to create!")
                                           }
                                       })

}

