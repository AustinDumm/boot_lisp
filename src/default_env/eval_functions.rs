
use super::env_utilities::*;
use std::process;

use std::fs;
use std::path::Path;

use crate::lexer;
use crate::parser::{
    self,
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn begin(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

pub fn read(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> { 
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(Expr { expr_data: ExprData::StringLiteral(string) }), None) = (iter.next(), iter.next()) {
                                     let path = Path::new(&string);
                                     let file_contents = fs::read_to_string(path).expect("Failed to open file");
                                     let result_expr =
                                         parser::parse(
                                             lexer::lex(file_contents).expect("Failed lexing file contents")
                                         ).expect("Failed parsing file contents");

                                     ExprData::List(result_expr.into_iter()).to_expr()
                                 } else {
                                     panic!("read must take only single argument");
                                 }
                             })
}

pub fn eval(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

pub fn apply(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let env = frame.as_ref().unwrap().env.clone();
    eval_arguments(accumulator,
                   frame,
                   stack,
                   |_, iter, _| {
                       let mut new_list = vec![];
                       let mut iter = iter.peekable();
                       while let Some(expr) = iter.next() {
                           if iter.peek().is_none() {
                               if let ExprData::List(final_list) = expr.expr_data {
                                   new_list.extend(final_list)
                               } else {
                                   panic!("apply must be given a list as its final argument")
                               }
                           } else {
                               new_list.push(expr)
                           }
                       }

                       Some(StackFrame { expr: ExprData::List(vec![].into_iter()).to_expr(),
                                         env,
                                         rib: new_list })
                   })
}

pub fn exit(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
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

