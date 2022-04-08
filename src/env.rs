
use crate::parser::{
    Expr,
    ExprData,
};

use std::collections::HashMap;

use std::sync::{
    RwLock,
    Arc,
};

/// Type containing a reference counted pointer to an EnvData object which stores the information
/// for a particular Environment
#[derive(Debug, Clone)]
pub struct Env {
    env_data: Arc<EnvData>
}

impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.env_data, &other.env_data)
    }
}

/// Stores nested environment bindings from identifier Strings to Expressions. Contains
/// reference to next environment to handle nested bindings
#[derive(Debug)]
pub struct EnvData {
    pub dict: RwLock<HashMap<String, Expr>>,
    pub next_env: Option<Env>,
    pub sym_number: Option<RwLock<u64>>,
}

impl EnvData {
    fn new(sym_number: Option<RwLock<u64>>) -> EnvData {
        EnvData { dict: RwLock::new(HashMap::new()), next_env: None, sym_number }
    }

    fn containing(map: HashMap<String, Expr>, sym_number: Option<RwLock<u64>>) -> EnvData {
        EnvData { dict: RwLock::new(map), next_env: None, sym_number }
    }

    fn extending(map: HashMap<String, Expr>, env: Env) -> EnvData {
        EnvData { dict: RwLock::new(map), next_env: Some(env), sym_number: None }
    }
}

impl Env {
    /// Creates a new empty Environment with no bindings to any values and no parent environment
    pub fn new(sym_number: Option<RwLock<u64>>) -> Env {
        Env { env_data: Arc::new(EnvData::new(sym_number)) }
    }

    /// Creates a new Environment containing bindings from Strings to Expressions as contained in
    /// the passed-in hashmap
    ///
    /// ```
    /// let env = Env::containing(vec![("binding", ExprData::Integer(152).to_expr())].to_iter().collect());
    ///
    /// assert_eq!(env.get("binding").unwrap().read().unwrap(),
    ///            ExprData::Integer(152).to_expr());
    /// ```
    pub fn containing(map: HashMap<String, Expr>, sym_number: Option<RwLock<u64>>) -> Env {
        let locked_expr_map: HashMap<String, Expr> =
            map
                .into_iter()
                .map(|tuple| {
                    (tuple.0, tuple.1)
                }).collect();
        Env { env_data: Arc::new(EnvData::containing(locked_expr_map, sym_number)) }
    }

    /// Performs lookup of the given identifier name in this environment. If this environment does
    /// not contain a binding of the given identifier name and has a reference to a parent
    /// enclosing environment, will then perform lookup in the enclosing environment. If this
    /// environment does not contain a binding of the given identifier name and does not have a
    /// reference to a parent enclosing environment, will return None
    pub fn get<'a>(&self, identifier_name: &str) -> Option<Expr> {
        let data_ptr = self.env_data.clone();
        let dict = data_ptr.dict.read().unwrap();
        if dict.contains_key(identifier_name) {
            let expr = dict.get(identifier_name).unwrap().clone();
            Some(expr)
        } else if let Some(next_env) = &data_ptr.next_env {
            next_env.get(identifier_name)
        } else {
            None
        }
    }

    /// Updates value bound to given identifier_name to new value Expression and returns true on
    /// value update. If this environment does not contain a binding to the given identifier_name
    /// and has a reference to an enclosing environment, will attempt to set with the same
    /// identifier name and value on the enclosing environment. If this environment does not
    /// contain a binding to the given identifier_name and does not have a reference to an
    /// enclosing environment, will return false to indicate that no such set has been performed.
    pub fn set(&self, identifier_name: String, value: Expr) -> bool {
        let data_ptr = self.env_data.clone();
        let mut dict = data_ptr.dict.write().unwrap();
        if dict.contains_key(&identifier_name) {
            dict.insert(identifier_name, value);
            true
        } else if let Some(next_env) = data_ptr.next_env.clone() {
            Env::set(&next_env, identifier_name, value)
        } else {
            false
        }
    }

    /// Creates or updates a new binding in this environment from the identifier_name to expression
    /// and returns true if value was created or updated correctly.
    pub fn create(&self, identifier_name: String, value: Expr) -> bool {
        let data_ptr = self.env_data.clone();
        let mut dict = data_ptr.dict.write().unwrap();
        dict.insert(identifier_name, value);
        true
    }

    /// Creates a new Environment containing bindings from the identifier(s) in the binding_list to
    /// the corresponding values in the bindings Expression. This new environment is enclosed by
    /// the environment this function has been called on. Parameter "bindings" must be of
    /// Expression type "List".
    ///
    /// Three types of bindings between the binding_list and binding Expression are supported:
    /// - binding_list = Identifier
    ///     - Binds the entire list in "bindings" to the identifier stored in binding_list
    /// - binding_list = List
    ///     - Binds each identifier stored in the binding_list List to the matching value in the
    ///     bindings List
    ///     - List lengths for binding_list and bindings must be of matching arity for binding to
    ///     succeed
    /// - binding_list = DottedList
    ///     - Binds each identifier stored in the S-Expression piece of the binding_list DottedList
    ///     to the matching value in the bindings List. Binds the identifier in the final "dotted"
    ///     place of the binding_list DottedList to a List containing the remaining values in the
    ///     bindings List
    ///     - S-Expression piece of the binding_list must be of less than or equal arity to the
    ///     binding list length for the binding to succeed.
    pub fn extend(&self, binding_list: Expr, bindings: Expr) -> Env {
        fn collect_binding_pairs(binding_list: Expr, bindings: Expr) -> Vec<(String, Expr)> {
            if let ExprData::Identifier(name) = binding_list.expr_data {
                match &bindings.expr_data {
                    ExprData::List(_) =>
                        vec![(name, bindings)],
                    _ => panic!("Value to bind to must be of type List"),
                }
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
                            panic!("Attempt to bind with unmatching list length: {:?}", raw_bindings);
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
                            pairs.push((name, ExprData::List(values_iter).to_expr()))
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
            .map(|pair| (pair.0, pair.1));
        Env { env_data: Arc::new(EnvData::extending(binding_pairs.collect(), Env { env_data: self.env_data.clone() }))}
    }

    pub fn get_sym_number(&self) -> Option<u64> {
        let data_ptr = self.env_data.clone();
        if let Some(lock) = &data_ptr.sym_number {
            let mut number = lock.write().unwrap();
            let result = number.clone();
            *number += 1;

            Some(result)
        } else if let Some(parent) = data_ptr.next_env.clone() {
            parent.get_sym_number()
        } else {
            None
        }
    }
}

#[cfg(test)]
mod env_tests {
    use super::*;

    #[test]
    fn does_empty_env_get_none() {
        let env = Env::new(None);
        
        assert!(env.get("test").is_none());
    }

    #[test]
    fn does_set_on_empty_env_fail() {
        let env = Env::new(None);

        assert!(!env.set(String::from("test"), ExprData::nil().to_expr()));
    }

    #[test]
    fn does_env_lookup() {
        let integer = ExprData::Integer(16).to_expr();
        let identifier = ExprData::Identifier(String::from("ident")).to_expr();
        let lambda_expr = ExprData::Lambda(
                                        Box::new(ExprData::List(vec![ExprData::Identifier(String::from("arg")).to_expr()].into_iter()).to_expr()),
                                        Box::new(ExprData::Integer(5).to_expr()),
                                        Env::new(None)).to_expr();
        let list = ExprData::List(
                    vec![ExprData::Integer(5).to_expr(),
                         ExprData::Identifier(String::from("test")).to_expr()].into_iter()).to_expr();
        let dotted = ExprData::DottedList(
                    vec![ExprData::Integer(6).to_expr(),
                         ExprData::Identifier(String::from("rest")).to_expr()].into_iter(),
                    Box::new(ExprData::Integer(10).to_expr())).to_expr();
        let nil = ExprData::nil().to_expr();

        let env =
            Env::containing(
                vec![(String::from("integer"), integer.clone()),
                     (String::from("identifier"), identifier.clone()),
                     (String::from("lambda"), lambda_expr.clone()),
                     (String::from("list"), list.clone()),
                     (String::from("dotted"), dotted.clone()),
                     (String::from("nil"), nil.clone())]
                        .into_iter()
                        .collect(),
                        None);

        assert_eq!(env.get("integer").unwrap(), integer);
        assert_eq!(env.get("identifier").unwrap(), identifier);
        assert_eq!(env.get("lambda").unwrap(), lambda_expr);
        assert_eq!(env.get("list").unwrap(), list);
        assert_eq!(env.get("dotted").unwrap(), dotted);
        assert_eq!(env.get("nil").unwrap(), nil);
    }

    #[test]
    fn does_bind_single_expr() {
        let empty_env = Env::new(None);

        let env = empty_env.extend(
                              ExprData::Identifier(String::from("test")).to_expr(),
                              ExprData::List(vec![ExprData::Integer(5).to_expr()].into_iter()).to_expr());

        let env_value = env.get("test").unwrap();

        assert_eq!(env_value.expr_data,
                   ExprData::List(vec![ExprData::Integer(5).to_expr()].into_iter()));
    }

    #[test]
    fn does_bind_from_list() {
        let empty_env = Env::new(None);
        let env = 
            empty_env.extend(
                Expr::form_list(vec![ExprData::Identifier(String::from("first")),
                                     ExprData::Identifier(String::from("sec"))]),
                Expr::form_list(vec![ExprData::Integer(30),
                                     ExprData::Integer(40)]));

        assert_eq!(env.get("first").unwrap().expr_data,
                   ExprData::Integer(30));

        assert_eq!(env.get("sec").unwrap().expr_data,
                   ExprData::Integer(40));
    }

    #[test]
    fn does_bind_from_dotted_list() {
        let empty_env = Env::new(None);
        let env =
            empty_env.extend(
                ExprData::DottedList(vec![ExprData::Identifier(String::from("first")).to_expr()].into_iter(),
                                     Box::new(ExprData::Identifier(String::from("rest")).to_expr())).to_expr(),
                Expr::form_list(vec![ExprData::Integer(100),
                                     ExprData::Integer(101),
                                     ExprData::Integer(102)]));

        assert_eq!(env.get("first").unwrap().expr_data,
                   ExprData::Integer(100));

        assert_eq!(env.get("rest").unwrap().expr_data,
                   ExprData::List(vec![ExprData::Integer(101).to_expr(),
                                       ExprData::Integer(102).to_expr()].into_iter()));
    }

    #[test]
    fn does_set_update_env() {
        let env = Env::containing(vec![(String::from("key"), ExprData::Integer(17).to_expr())].into_iter().collect(), None);

        assert_eq!(env.get("key").unwrap().expr_data,
                   ExprData::Integer(17));

        env.set(String::from("key"), ExprData::Integer(33).to_expr());

        assert_eq!(env.get("key").unwrap().expr_data,
                   ExprData::Integer(33));
    }

    #[test]
    fn does_set_update_refs() {
        let env = Env::containing(vec![(String::from("key"), ExprData::Integer(69).to_expr())].into_iter().collect(), None);

        let val_ref = env.get("key").unwrap();

        assert_eq!(val_ref.expr_data,
                   ExprData::Integer(69));

        env.set(String::from("key"), ExprData::Integer(420).to_expr());

        let val_ref = env.get("key").unwrap();

        assert_eq!(val_ref.expr_data,
                   ExprData::Integer(420));
    }
    
    #[test]
    fn does_fail_nonexistent_set() {
        let env = Env::containing(vec![(String::from("key"), ExprData::Integer(2).to_expr())].into_iter().collect(), None);

        assert!(!env.set(String::from("nonexistent"), ExprData::Integer(5).to_expr()));
    }
}

