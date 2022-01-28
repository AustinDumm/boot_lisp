
use std::vec::IntoIter;

use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn eval_arguments_excepting<F>(accumulator: &mut Option<Expr>,
                               frame: Option<StackFrame>,
                               stack: &mut CallStack,
                               exception_indices: Vec<usize>,
                               return_fn: F) -> Option<StackFrame>
where F: FnOnce(&mut Option<Expr>, IntoIter<Expr>, &mut CallStack) -> Option<StackFrame> {
    if let Some(frame) = frame {
        if let Expr { expr_data: ExprData::List(mut list) } = frame.expr {
            if let Some(next_expr) = list.next() {
                if exception_indices.contains(&(frame.rib.len() - 1)) {
                    let mut new_rib = frame.rib;
                    new_rib.push(next_expr);
                    Some(StackFrame::new(ExprData::List(list).to_expr(),
                                         frame.env.clone(),
                                         new_rib))
                } else {
                    stack.push_frame(StackFrame::new(ExprData::List(list).to_expr(),
                                                     frame.env.clone(),
                                                     frame.rib));
                    Some(StackFrame::new(next_expr,
                                         frame.env.clone(),
                                         vec![]))
                }
            } else {
                // Throw away the Function expr that starts the list
                let mut iter = frame.rib.into_iter();
                iter.next();

                return_fn(accumulator, iter, stack)
            }
        } else {
            panic!("Non-list expr type given to Function evaluation")
        }
    } else {
        panic!("No frame found for arguments evaluation")
    }
}

pub fn eval_arguments<F>(accumulator: &mut Option<Expr>,
                     frame: Option<StackFrame>,
                     stack: &mut CallStack,
                     return_fn: F) -> Option<StackFrame>
where F: FnOnce(&mut Option<Expr>, IntoIter<Expr>, &mut CallStack) -> Option<StackFrame> {
    eval_arguments_excepting(accumulator,
                             frame,
                             stack,
                             vec![],
                             return_fn)
}

pub fn eval_arguments_and_apply<F>(accumulator: &mut Option<Expr>,
                               frame: Option<StackFrame>,
                               stack: &mut CallStack,
                               application: F) -> Option<StackFrame>
where F: FnOnce(IntoIter<Expr>) -> Expr {
    eval_arguments(accumulator,
                   frame,
                   stack,
                   |accumulator, iter, stack| {
                       *accumulator = Some(application(iter));
                       stack.pop_frame()
                   })
}

pub fn eval_arguments_excepting_and_apply<F>(accumulator: &mut Option<Expr>,
                                         frame: Option<StackFrame>,
                                         stack: &mut CallStack,
                                         exception_indices: Vec<usize>,
                                         application: F) -> Option<StackFrame>
where F: FnOnce(IntoIter<Expr>) -> Expr {
    eval_arguments_excepting(accumulator,
                             frame,
                             stack,
                             exception_indices,
                             |accumulator, iter, stack| {
                                 *accumulator = Some(application(iter));
                                 stack.pop_frame()
                             })
}

pub fn eval_arguments_and_compare<F>(accumulator: &mut Option<Expr>,
                                 frame: Option<StackFrame>,
                                 stack: &mut CallStack,
                                 cmp: F) -> Option<StackFrame>
where F: Fn(&Expr, &Expr) -> bool {
    eval_arguments_and_apply(accumulator,
                             frame,
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
