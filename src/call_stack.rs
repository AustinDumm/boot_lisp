
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

pub struct EvaluationEnvironment {
    pub accumulator: Option<Expr>,
    pub active_frame: Option<StackFrame>,
    pub stack: CallStack,
}

impl EvaluationEnvironment {
    pub fn new(expr: Expr, env: Env) -> EvaluationEnvironment {
        EvaluationEnvironment { 
            accumulator: None,
            active_frame: Some(StackFrame::new(ExprQueue::Expr(expr), env, vec![])),
            stack: CallStack::new() }
    }

    pub fn pop_frame(&mut self) {
        self.active_frame = self.stack.pop_frame()
    }
}

