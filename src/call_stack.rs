
use crate::parser::{
    Expr,
};

use crate::env::{
    Env,
};

/// Holds stack information for the evaluation of an expression
///
/// - expr
///     - Stores the expression to be evaluate
/// - env
///     - Stores the enclosing environment for identifier lookup
/// - rib
///     - Stores intermediate evaluations of list elements in preparation for application and
///     evaluation of functions
#[derive(Debug, Clone)]
pub struct StackFrame {
    pub expr: Expr,
    pub env: Env,
    pub rib: Vec<Expr>,
}

impl StackFrame {
    pub fn new(expr: Expr, env: Env, rib: Vec<Expr>) -> StackFrame {
        StackFrame { expr, env, rib }
    }
}

/// Holds a stack data structure of StackFrames used to sequence evaluation of nested expressions
#[derive(Debug, Clone)]
pub struct CallStack {
    stack: Vec<StackFrame>
}

impl CallStack {
    pub fn new() -> CallStack {
        CallStack { stack: vec![] }
    }

    pub fn pop_frame(&mut self) -> Option<StackFrame> {
        self.stack.pop()
    }

    pub fn push_frame(&mut self, frame: StackFrame) {
        self.stack.push(frame)
    }

    pub fn append(&mut self, stack: CallStack) {
        for frame in stack.stack.into_iter().rev() {
            self.stack.push(frame);
        }
    }
}

