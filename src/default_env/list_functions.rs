
use super::env_utilities::*;
use std::vec::IntoIter;

use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn cons(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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
                                         _ => ExprData::DottedList(vec![first].into_iter(), Box::new(rest)).to_expr(),
                                     }
                                 } else {
                                     panic!("Invalid number of arguments provided to cons. Requires 2")
                                 }
                             })
}

pub fn first(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::List(mut iter) |
                                             ExprData::DottedList(mut iter, _) => {
                                             iter.next().expect("No item found in list while evaluating 'first'")
                                         },
                                         _ => panic!("'first' must be given list")
                                     }
                                 } else {
                                     panic!("Invalid number of arguments provided to first. Requires 1")
                                 }
                             })
}

pub fn rest(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(expr), None) = (iter.next(), iter.next()) {
                                     match expr.expr_data {
                                         ExprData::List(mut iter) => {
                                             iter.next();
                                             ExprData::List(iter).to_expr()
                                         },
                                         ExprData::DottedList(mut iter, rest) => {
                                             iter.next();
                                             ExprData::DottedList(iter, rest).to_expr()
                                         }
                                         _ => panic!("'rest' must be given a list")
                                     }
                                 } else {
                                     panic!("Invalid number of arguments provided to rest. Requires 1")
                                 }
                             })
}

pub fn list_impl(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |iter| {
                                 ExprData::List(iter).to_expr()
                             })
}

pub fn append_impl(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    fn concatenate_lists(iter: IntoIter<Expr>) -> Expr {
        let mut iter = iter.peekable();

        let mut append_vec: Vec<Expr> = vec![];
        while let Some(next_expr) = iter.next() {
            if iter.peek().is_none() {
                match next_expr.expr_data {
                    ExprData::List(_) => (),
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
                panic!("Non-list provided as non-last element to append: {}", next_expr.expr_data)
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

