
use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};

use crate::env::{
    Env,
};

fn add(frame: StackFrame, stack: &mut CallStack) -> StackFrame {
    if let Expr { expr_data: ExprData::List(mut list) } = frame.expr {
        if let Some(next_expr) = list.next() {
            stack.push_frame(StackFrame::new(ExprData::List(list).to_expr(),
                                             frame.env.clone(),
                                             frame.rib));
            StackFrame::new(next_expr,
                            frame.env.clone(),
                            vec![])
        } else {
            // Throw away the Function expr that starts the list
            let mut iter = frame.rib.into_iter();
            iter.next();
            let result = 
                iter.map(|expr| {
                        match expr {
                            Expr { expr_data: ExprData::Integer(value) } => value,
                            _ => panic!("Integer type must be used for addition"),
                        }
                    }).sum();

            StackFrame::new(ExprData::Integer(result).to_expr(),
                            frame.env,
                            vec![])
        }
    } else {
        panic!("Non-list expr type given to add function")
    }
}

pub fn default_env() -> Env {
    Env::containing(
        vec![
            (String::from("+"), ExprData::Function(String::from("+"), add).to_expr())
        ].into_iter().collect()
    )
}

