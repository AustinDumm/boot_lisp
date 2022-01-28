
use super::{
    eval_functions,
    env_utilities::*,
};

use crate::call_stack::{
    StackFrame,
    CallStack,
};

use crate::{
    Expr,
    ExprData,
};

pub fn prompt(_accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let frame = frame.expect("No frame found while evaluating 'prompt'");

    stack.push_frame(StackFrame::new_prompt_frame(ExprData::nil().to_expr(),
                                                  frame.env.clone(),
                                                  vec![ExprData::Function("begin".to_string(), eval_functions::begin).to_expr()]));

    if let ExprData::List(mut iter) = frame.expr.expr_data {
        if let (Some(expr), None) = (iter.next(), iter.next()) {
            Some(StackFrame::new(expr,
                                 frame.env.clone(),
                                 vec![]))
        } else {
            panic!("Invalid number of arguments provided to 'prompt'. Expected: 1")
        }
    } else {
        panic!("Unexpected non-list found while evaluating 'prompt'")
    }
    }

pub fn call_with_control(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    let env = frame.as_ref().unwrap().env.clone();
    eval_arguments(accumulator,
                   frame,
                   stack,
                   |_, mut iter, stack| {
                       let mut continuation_stack: Vec<StackFrame> = vec![];
                       while let Some(frame) = stack.pop_frame() {
                           if frame.is_prompt {
                               break;
                           }

                           continuation_stack.push(frame);
                       }

                       let continuation_expr = ExprData::Continuation(CallStack::from(continuation_stack.into_iter().rev().collect())).to_expr();

                       if let (Some(lambda_expr), None) = (iter.next(), iter.next()) {
                           let continuation_call = ExprData::List(vec![lambda_expr, continuation_expr].into_iter()).to_expr();

                           Some(StackFrame::new(continuation_call,
                                                env,
                                                vec![]))
                       } else {
                           panic!("Invalid number of arguments provided to call/control")
                       }
                   })
}

