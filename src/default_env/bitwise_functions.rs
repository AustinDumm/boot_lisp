
use super::env_utilities::*;

use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn bit_and(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

pub fn bit_or(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

pub fn bit_xor(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

pub fn bit_not(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

