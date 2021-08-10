
use std::collections::HashMap;

use std::sync::{
    Mutex,
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
    dict: HashMap<String, Arc<RwLock<Expr>>>,
    next_env: Option<Arc<Env>>,
}

impl Env {
    pub fn new() -> Arc<Env> {
        Arc::new(Env { dict: HashMap::new(), next_env: None })
    }

    pub fn get<'a>(self_ptr: Arc<Self>, identifier_name: String) -> Option<Arc<RwLock<Expr>>> {
        if self_ptr.dict.contains_key(&identifier_name) {
            let expr = self_ptr.dict.get(&identifier_name).unwrap().clone();
            Some(expr)
        } else if let Some(next_env_arc) = &self_ptr.next_env {
            Env::get(next_env_arc.clone(), identifier_name)
        } else {
            None
        }
    }

    pub fn set(self_ptr: Arc<Self>, identifier_name: String, value: Expr) -> bool {
        if self_ptr.dict.contains_key(&identifier_name) {
            let dict_item = self_ptr.dict.get(&identifier_name).unwrap().clone();
            let mut expr_lock = dict_item.write().unwrap();
            *expr_lock = value;
            true
        } else if let Some(next_env) = self_ptr.next_env.clone() {
            Env::set(next_env, identifier_name, value)
        } else {
            false
        }
    }

    pub fn extend(self_ptr: Arc<Self>, binding_list: Expr, bindings: Expr) -> Option<Arc<Self>> {
        Some(Arc::new(Env { dict: HashMap::new(), next_env: Some(self_ptr) }))
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

