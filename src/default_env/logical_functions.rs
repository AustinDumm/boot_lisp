
use super::env_utilities::*;

use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn and(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let frame = frame.expect("No frame found when evaluating and");
    if frame.rib.len() == 1 {
        // Last item is this function, need to evaluate next boolean
        if let ExprData::List(mut iter) = frame.expr.expr_data {
            if let Some(expr) = iter.next() {
                let next_frame = StackFrame::new(expr,
                                                 frame.env.clone(),
                                                 vec![]);

                stack.push_frame(StackFrame::new(ExprData::List(iter).to_expr(),
                                                 frame.env.clone(),
                                                 frame.rib));

                Some(next_frame)
            } else {
                *accumulator = Some(ExprData::Bool(true).to_expr());
                stack.pop_frame()
            }
        } else {
            panic!("and must be given a list as arguments")
        }
    } else {
        // Last item in rib is an expr. Check to see if we need to short-circuit the and
        let last_rib_expr = frame.rib.last().unwrap();
        match &last_rib_expr.expr_data {
            ExprData::Bool(false) => {
                *accumulator = Some(ExprData::Bool(false).to_expr());
                stack.pop_frame()
            },
            ExprData::Bool(true) => {
                let mut rib = frame.rib;
                rib.remove(rib.len() - 1);

                Some(StackFrame::new(frame.expr,
                                     frame.env,
                                     rib))
            }
            expr => panic!("and expects values of boolean type. Found: {}", expr)
        }
    }
}

pub fn or(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let frame = frame.expect("No frame found when evaluating or");
    if frame.rib.len() == 1 {
        // Last item is this function, need to evaluate next boolean
        if let ExprData::List(mut iter) = frame.expr.expr_data {
            if let Some(expr) = iter.next() {
                let next_frame = StackFrame::new(expr,
                                                 frame.env.clone(),
                                                 vec![]);

                stack.push_frame(StackFrame::new(ExprData::List(iter).to_expr(),
                                                 frame.env.clone(),
                                                 frame.rib));

                Some(next_frame)
            } else {
                *accumulator = Some(ExprData::Bool(false).to_expr());
                stack.pop_frame()
            }
        } else {
            panic!("or must be given a list as arguments")
        }
    } else {
        // Last item in rib is an expr. Check to see if we need to short-circuit the and
        let last_rib_expr = frame.rib.last().unwrap();
        match &last_rib_expr.expr_data {
            ExprData::Bool(true) => {
                *accumulator = Some(ExprData::Bool(true).to_expr());
                stack.pop_frame()
            },
            ExprData::Bool(false) => {
                let mut rib = frame.rib;
                rib.remove(rib.len() - 1);

                Some(StackFrame::new(frame.expr,
                                     frame.env,
                                     rib))
            }
            expr => panic!("and expects values of boolean type. Found: {}", expr)
        }
    }}

pub fn xor(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 if let (Some(Expr { expr_data: ExprData::Bool(first) }),
                                         Some(Expr { expr_data: ExprData::Bool(second) }),
                                         None) = (iter.next(), iter.next(), iter.next()) {
                                     ExprData::Bool((first || second) && !(first && second)).to_expr()
                                 } else {
                                     panic!("xor must be given two boolean arguments")
                                 }
                             })
}

pub fn not(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    eval_arguments_and_apply(accumulator,
                             frame,
                             stack,
                             |mut iter| {
                                 ExprData::Bool(
                                     if let (Some(Expr { expr_data: ExprData::Bool(boolean) }), None) = (iter.next(), iter.next()) {
                                         !boolean
                                     } else {
                                         panic!("not must take single, boolean argument")
                                     }
                                 ).to_expr()
                             })
}
