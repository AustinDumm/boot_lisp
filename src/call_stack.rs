
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
    pub expr_queue: ExprQueue,
    pub env: Env,
    pub rib: Vec<Expr>,
}

impl StackFrame {
    pub fn new(expr_queue: ExprQueue,env: Env, rib: Vec<Expr>) -> StackFrame {
        StackFrame { env, rib, expr_queue }
    }
}

pub enum ExprQueue {
    Expr(Expr),
    Queue(IntoIter<Expr>),
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

