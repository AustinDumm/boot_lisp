
use crate::parser::{
    Expr,
    ExprData,
};

use std::collections::HashMap;

use std::sync::{
    RwLock,
    Arc,
};

#[derive(Debug, Clone)]
pub struct Env {
    env_data: Arc<EnvData>
}

impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.env_data, &other.env_data)
    }
}

#[derive(Debug)]
pub struct EnvData {
    dict: HashMap<String, Arc<RwLock<Expr>>>,
    next_env: Option<Env>,
}

impl Env {
    pub fn new() -> Env {
        Env { env_data: Arc::new(EnvData { dict: HashMap::new(), next_env: None }) }
    }

    pub fn containing(map: HashMap<String, Arc<RwLock<Expr>>>) -> Env {
        Env { env_data: Arc::new(EnvData { dict: map, next_env: None }) }
    }

    pub fn get<'a>(&self, identifier_name: &str) -> Option<Arc<RwLock<Expr>>> {
        let data_ptr = self.env_data.clone();
        if data_ptr.dict.contains_key(identifier_name) {
            let expr = data_ptr.dict.get(identifier_name).unwrap().clone();
            Some(expr)
        } else if let Some(next_env) = &data_ptr.next_env {
            next_env.get(identifier_name)
        } else {
            None
        }
    }

    pub fn set(&self, identifier_name: String, value: Expr) -> bool {
        let data_ptr = self.env_data.clone();
        if data_ptr.dict.contains_key(&identifier_name) {
            let dict_item = data_ptr.dict.get(&identifier_name).unwrap().clone();
            let mut expr_lock = dict_item.write().unwrap();
            *expr_lock = value;
            true
        } else if let Some(next_env) = data_ptr.next_env.clone() {
            Env::set(&next_env, identifier_name, value)
        } else {
            false
        }
    }

    pub fn extend(&self, binding_list: Expr, bindings: Expr) -> Env {
        fn collect_binding_pairs(mut binding_list: Expr, mut bindings: Expr) -> Vec<(String, Expr)> {
            if let ExprData::Identifier(name) = binding_list.expr_data {
                vec![(name, bindings)]
            } else {
                match (binding_list.expr_data, bindings.expr_data) {
                    (ExprData::List(raw_bindings), ExprData::List(raw_values)) => {
                        if raw_bindings.len() == raw_values.len() {
                            raw_bindings
                                .map(|expr| match &expr.expr_data {
                                                ExprData::Identifier(name) => name.clone(),
                                                _ => panic!("Attempt to bind value to non-identifier")
                                })
                                .zip(raw_values.into_iter()).collect()
                        } else {
                            panic!("Attempt to bind with unmatching list length");
                        }
                    },
                    (ExprData::DottedList(raw_bindings, final_binding), ExprData::List(raw_values)) => {
                        let mut pairs: Vec<(String, Expr)> = vec![];
                        let mut bindings_iter = raw_bindings.into_iter();
                        let mut values_iter = raw_values.into_iter();
                        while let Some(binding) = bindings_iter.next() {
                            let value = values_iter.next().expect("Too few arguments used to bind to environment");
                            if let ExprData::Identifier(name) = binding.expr_data {
                                pairs.push((name, value));
                            } else {
                                panic!("Invalid binding identifier")
                            }
                        }

                        if let ExprData::Identifier(name) = final_binding.expr_data {
                            pairs.push((name, Expr::form_list(values_iter.collect())))
                        } else {
                            panic!("Final expr in dotted binding list is not an identifier")
                        }

                        pairs
                    },
                    (_, _) => panic!("Invalid binding expression attempted to bind in environment")
                }
            }
        }

        let binding_pairs = collect_binding_pairs(binding_list, bindings)
            .into_iter()
            .map(|pair| (pair.0, Arc::new(RwLock::new(pair.1))));
        Env { env_data: Arc::new(EnvData { dict: binding_pairs.collect(), next_env: Some(Env { env_data: self.env_data.clone() })})}
    }
}

#[cfg(test)]
mod env_tests {
    use super::*;

    #[test]
    fn does_empty_env_get_none() {
        let env = Env::new();
        
        assert!(env.get("test").is_none());
    }

    #[test]
    fn does_set_on_empty_env_fail() {
        let env = Env::new();

        assert!(!env.set(String::from("test"), ExprData::Nil.to_expr()));
    }

    #[test]
    fn does_env_lookup() {
        let integer = Arc::new(RwLock::new(ExprData::Integer(16).to_expr()));
        let identifier = Arc::new(RwLock::new(ExprData::Identifier(String::from("ident")).to_expr()));
        let lambda_expr = Arc::new(RwLock::new(ExprData::Lambda(
                                                Box::new(ExprData::List(vec![ExprData::Identifier(String::from("arg")).to_expr()].into_iter()).to_expr()),
                                                vec![ExprData::Integer(5).to_expr()],
                                                Env::new()).to_expr()));
        let list = Arc::new(RwLock::new(ExprData::List(
                    vec![ExprData::Integer(5).to_expr(),
                         ExprData::Identifier(String::from("test")).to_expr()].into_iter()).to_expr()));
        let dotted = Arc::new(RwLock::new(ExprData::DottedList(
                    vec![ExprData::Integer(6).to_expr(),
                         ExprData::Identifier(String::from("rest")).to_expr()].into_iter(),
                    Box::new(ExprData::Integer(10).to_expr())).to_expr()));
        let nil = Arc::new(RwLock::new(ExprData::Nil.to_expr()));

        let env =
            Env::containing(
                vec![(String::from("integer"), integer.clone()),
                     (String::from("identifier"), identifier.clone()),
                     (String::from("lambda"), lambda_expr.clone()),
                     (String::from("list"), list.clone()),
                     (String::from("dotted"), dotted.clone()),
                     (String::from("nil"), nil.clone())]
                        .into_iter()
                        .collect());

        assert!(Arc::ptr_eq(&env.get("integer").unwrap(), &integer));
        assert!(Arc::ptr_eq(&env.get("identifier").unwrap(), &identifier));
        assert!(Arc::ptr_eq(&env.get("lambda").unwrap(), &lambda_expr));
        assert!(Arc::ptr_eq(&env.get("list").unwrap(), &list));
        assert!(Arc::ptr_eq(&env.get("dotted").unwrap(), &dotted));
        assert!(Arc::ptr_eq(&env.get("nil").unwrap(), &nil));
    }

    #[test]
    fn does_bind_single_expr() {
        let empty_env = Env::new();

        let env = empty_env.extend(
                              ExprData::Identifier(String::from("test")).to_expr(),
                              ExprData::List(vec![ExprData::Integer(5).to_expr()].into_iter()).to_expr());

        let env_value = env.get("test").unwrap();

        assert_eq!(env_value.read().unwrap().expr_data,
                   ExprData::List(vec![ExprData::Integer(5).to_expr()].into_iter()));
    }

    #[test]
    fn does_bind_from_list() {
        let empty_env = Env::new();
        let env = 
            empty_env.extend(
                Expr::form_list(vec![ExprData::Identifier(String::from("first")).to_expr(),
                                     ExprData::Identifier(String::from("sec")).to_expr()]),
                Expr::form_list(vec![ExprData::Integer(30).to_expr(),
                                     ExprData::Integer(40).to_expr()]));

        assert_eq!(env.get("first").unwrap().read().unwrap().expr_data,
                   ExprData::Integer(30));

        assert_eq!(env.get("sec").unwrap().read().unwrap().expr_data,
                   ExprData::Integer(40));
    }

    #[test]
    fn does_bind_from_dotted_list() {
        let empty_env = Env::new();
        let env =
            empty_env.extend(
                ExprData::DottedList(vec![ExprData::Identifier(String::from("first")).to_expr()].into_iter(),
                                     Box::new(ExprData::Identifier(String::from("rest")).to_expr())).to_expr(),
                Expr::form_list(vec![ExprData::Integer(100).to_expr(),
                                     ExprData::Integer(101).to_expr(),
                                     ExprData::Integer(102).to_expr()]));

        assert_eq!(env.get("first").unwrap().read().unwrap().expr_data,
                   ExprData::Integer(100));

        assert_eq!(env.get("rest").unwrap().read().unwrap().expr_data,
                   ExprData::List(vec![ExprData::Integer(101).to_expr(),
                                       ExprData::Integer(102).to_expr()].into_iter()));
    }

    #[test]
    fn does_set_update_env() {
        let env = Env::containing(vec![(String::from("key"), Arc::new(RwLock::new(ExprData::Integer(17).to_expr())))].into_iter().collect());

        assert_eq!(env.get("key").unwrap().read().unwrap().expr_data,
                   ExprData::Integer(17));

        env.set(String::from("key"), ExprData::Integer(33).to_expr());

        assert_eq!(env.get("key").unwrap().read().unwrap().expr_data,
                   ExprData::Integer(33));
    }

    #[test]
    fn does_set_update_refs() {
        let env = Env::containing(vec![(String::from("key"), Arc::new(RwLock::new(ExprData::Integer(69).to_expr())))].into_iter().collect());

        let val_ref = env.get("key").unwrap();

        assert_eq!(val_ref.read().unwrap().expr_data,
                   ExprData::Integer(69));

        env.set(String::from("key"), ExprData::Integer(420).to_expr());

        assert_eq!(val_ref.read().unwrap().expr_data,
                   ExprData::Integer(420));
    }
    
    #[test]
    fn does_fail_nonexistent_set() {
        let env = Env::containing(vec![(String::from("key"), Arc::new(RwLock::new(ExprData::Integer(2).to_expr())))].into_iter().collect());

        assert!(!env.set(String::from("nonexistent"), ExprData::Integer(5).to_expr()));
    }
}

