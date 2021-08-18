
use crate::parser::{
    Expr,
};

use crate::env::{
    Env,
};

use std::vec::{
    IntoIter,
};

pub struct StackFrame {
    pub expr: Expr,
    pub env: Env,
    pub rib: Vec<Expr>,
}

impl StackFrame {
    pub fn new(expr: Expr,env: Env, rib: Vec<Expr>) -> StackFrame {
        StackFrame { env, rib, expr }
    }
}

pub struct CallStack {
    stack: Vec<StackFrame>
}

impl CallStack {
    pub fn new() -> CallStack {
        CallStack { stack: vec![] }
    }

    pub fn is_empty(&self) -> bool {
        return self.stack.len() == 0;
    }

    pub fn pop_frame(&mut self) -> Option<StackFrame> {
        self.stack.pop()
    }

    pub fn push_frame(&mut self, frame: StackFrame) {
        self.stack.push(frame)
    }
}

