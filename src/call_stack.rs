
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
#[derive(Clone)]
pub struct StackFrame {
    pub expr: Expr,
    pub env: Env,
    pub rib: Vec<Expr>,
    pub is_prompt: bool,
    pub is_expanded: bool,
}

impl StackFrame {
    pub fn new(expr: Expr, env: Env, rib: Vec<Expr>) -> StackFrame {
        StackFrame { expr, env, rib, is_prompt: false, is_expanded: false }
    }

    pub fn new_prompt_frame(expr: Expr, env: Env, rib: Vec<Expr>) -> StackFrame {
        StackFrame { expr, env, rib, is_prompt: true, is_expanded: false }
    }
}

impl std::fmt::Debug for StackFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "Frame:\n\texpr:{}\n\trib:{:?}\n\tis_prompt:{}", self.expr, self.rib, self.is_prompt)
    }
}

/// Holds a stack data structure of StackFrames used to sequence evaluation of nested expressions
#[derive(Debug, Clone)]
pub struct CallStack {
    pub stack: Vec<StackFrame>
}

impl CallStack {
    pub fn new() -> CallStack {
        CallStack { stack: vec![] }
    }

    pub fn from(stack: Vec<StackFrame>) -> CallStack {
        CallStack { stack }
    }

    pub fn pop_frame(&mut self) -> Option<StackFrame> {
        self.stack.pop()
    }

    pub fn push_frame(&mut self, frame: StackFrame) {
        self.stack.push(frame)
    }

    pub fn append(&mut self, stack: CallStack) {
        for frame in stack.stack {
            self.stack.push(frame);
        }
    }
}

