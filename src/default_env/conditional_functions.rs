
use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};

pub fn if_impl(_accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    if let Some(active_frame) = frame {
        let mut rib_iter = active_frame.rib.iter();
        // Drop function value from iter
        rib_iter.next();

        if let Some(Expr { expr_data: conditional_data }) = rib_iter.next() {
            // Conditional has been evaluated
            match (conditional_data, active_frame.expr.expr_data) {
                (ExprData::Bool(true), ExprData::List(mut iter)) => {
                    let true_expr = iter.next().unwrap();

                    Some(StackFrame::new(true_expr,
                                         active_frame.env.clone(),
                                         vec![]))
                },
                (ExprData::Bool(false), ExprData::List(mut iter)) => {
                    iter.next();
                    let false_expr = iter.next().unwrap();

                    Some(StackFrame::new(false_expr,
                                         active_frame.env.clone(),
                                         vec![]))
                },
                (ExprData::Bool(_), other) => {
                    panic!("if expressions must contain list. Found: {:?}", other)
                },
                (other, _) => {
                    panic!("First argument to an if expression must be boolean. Found: {:?}", other)
                },
            }
        } else {
            // Conditional needs evaluation
            match active_frame.expr.expr_data {
                ExprData::List(mut iter) => {
                    let conditional_expr = iter.next().unwrap();

                    stack.push_frame(StackFrame::new(ExprData::List(iter).to_expr(),
                                                     active_frame.env.clone(),
                                                     active_frame.rib));

                    Some(StackFrame::new(conditional_expr,
                                         active_frame.env.clone(),
                                         vec![]))
                }
                other => panic!("if expressions must be passed arguments via list. Found: {:?}", other),
            }
        }
    } else {
        panic!("No stack frame found for \"if\" evaluation")
    }
}

