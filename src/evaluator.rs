
use std::collections::HashMap;

use std::sync::{
    RwLock,
    Arc,
    RwLockReadGuard,
    RwLockWriteGuard,
};

use crate::parser::{
    Expr,
    ExprData,
};

#[derive(Debug)]
pub struct Env {
    dict: HashMap<String, Arc<Expr>>,
    next_env: Option<Arc<RwLock<Env>>>,
}

impl Env {
    pub fn new() -> Arc<RwLock<Env>> {
        Arc::new(RwLock::new(Env { dict: HashMap::new(), next_env: None }))
    }

    pub fn bind(self_guard: &mut RwLockWriteGuard<Self>, identifier_name: String, value: Expr) {
        self_guard.dict.insert(identifier_name, Arc::new(value));
    }

    pub fn get(self_guard: &RwLockReadGuard<Self>, identifier_name: String) -> Option<Arc<Expr>> {
        if self_guard.dict.contains_key(&identifier_name) {
            self_guard.dict.get(&identifier_name).cloned()
        } else if let Some(next_env) = &self_guard.next_env {
            Env::get(&next_env.read().unwrap(), identifier_name)
        } else {
            None
        }
    }

    pub fn set(self_guard: &mut RwLockWriteGuard<Self>, identifier_name: String, value: Expr) -> bool {
        if self_guard.dict.contains_key(&identifier_name) {
            self_guard.dict.insert(identifier_name, Arc::new(value));
            true
        } else if let Some(next_env) = self_guard.next_env.clone() {
            Env::set(&mut next_env.write().unwrap(), identifier_name, value)
        } else {
            false
        }
    }

    pub fn extend(self_lock: Arc<RwLock<Env>>) -> Arc<RwLock<Env>> {
        Arc::new(RwLock::new(Env { dict: HashMap::new(), next_env: Some(self_lock) }))
    }
}

pub struct EvalError {
    pub message: String,
}

impl EvalError {
    pub fn new(message: &str) -> EvalError {
        EvalError { message: String::from(message) }
    }
}

type EvalResult = Result<Expr, EvalError>;

pub fn eval(expr: Expr) -> EvalResult {
    Err(EvalError::new("Not yet implemented"))
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env() {
        
    }
}

