
use crate::parser::{
    Expr,
    ExprData,
};

use std::collections::HashMap;

use std::sync::{
    RwLock,
    Arc,
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

    // TODO: Setup binding_list and bindings to populate new hashmap
    pub fn extend(self_ptr: Arc<Self>, binding_list: Expr, bindings: Expr) -> Option<Arc<Self>> {
        fn collect_binding_pairs(mut binding_list: Expr, mut bindings: Expr) -> Vec<(String, Expr)> {
            if let ExprData::Identifier(name) = binding_list.expr_data {
                vec![(name, bindings)]
            } else {
                match (binding_list.expr_data, bindings.expr_data) {
                    (ExprData::List(raw_bindings), ExprData::List(raw_values)) => {
                        if raw_bindings.len() == raw_values.len() {
                            raw_bindings
                                .iter()
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
                        let bindings_iter = raw_bindings.into_iter();
                        let values_iter = raw_values.into_iter();
                        let mut zipped_iter = bindings_iter.zip(values_iter);
                        while let Some(pair) = zipped_iter.next() {
                            let (binding, value) = pair;
                            if let ExprData::Identifier(name) = binding.expr_data {
                                pairs.push((name, value));
                            } else {
                                panic!("Invalid binding identifier")
                            }
                        }

                        let (bindings_iter, values_iter): (Vec<_>, Vec<_>) = zipped_iter.unzip();

                        if bindings_iter.len() != 0 {
                            panic!("Unmatched binding arity. Too few values to bind");
                        }

                        if let ExprData::Identifier(name) = final_binding.expr_data {
                            pairs.push((name, Expr::form_list(values_iter)))
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
        Some(Arc::new(Env { dict: binding_pairs.collect(), next_env: Some(self_ptr) }))
    }
}
