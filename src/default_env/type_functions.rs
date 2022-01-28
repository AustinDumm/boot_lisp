
use super::env_utilities::*;

use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};



pub fn is_void(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::Void => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_bool expects only one argument")
                                 }
                             })
}

pub fn is_bool(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::Bool(_) => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_bool expects only one argument")
                                 }
                             })
}

pub fn is_integer(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::Integer(_) => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_integer expects only one argument")
                                 }
                             })
}

pub fn is_identifier(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::Identifier(_) => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_identifier expects only one argument")
                                 }
                             })
}


pub fn is_lambda(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::Lambda(_,_,_) => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_lambda expects only one argument")
                                 }
                             })
}

pub fn is_function(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::Function(_,_) => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_list expects only one argument")
                                 }
                             })
}

pub fn is_applicable(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::Lambda(_,_,_) |
                                         ExprData::Function(_,_) => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_list expects only one argument")
                                 }
                             })
}


pub fn is_list(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::List(_) |
                                         ExprData::DottedList(_,_) => ExprData::Bool(true),
                                         _ => ExprData::Bool(false),
                                     }.to_expr()
                                 } else {
                                     panic!("is_list expects only one argument")
                                 }
                             })
}

pub fn is_nil(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::List(mut iter) =>
                                             ExprData::Bool(iter.next().is_none()).to_expr(),
                                         _ => ExprData::Bool(false).to_expr(),
                                     }
                                 } else {
                                     panic!("nil? expects only one argument")
                                 }
                             })
}

