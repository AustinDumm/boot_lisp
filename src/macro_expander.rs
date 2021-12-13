
use crate::parser::{
    Expr,
    ExprData,
};

use crate::evaluator;

use crate::env::Env;

pub fn macro_expand(expr: &Expr, evaluation_env: Env, macro_env: &mut Env) -> Option<Expr> {
    let definition_found = collect_macro_definitions(&expr, evaluation_env.clone(), macro_env);

    if definition_found {
        Some(ExprData::Bool(false).to_expr())
    } else {
        expand_macro(expr, evaluation_env.clone(), macro_env)
    }
}

fn expand_macro(expr: &Expr, evaluation_env: Env, macro_env: &mut Env) -> Option<Expr> {
    match &expr.expr_data {
        ExprData::List(iter) => {
            let mut iter = iter.clone();
            match iter.next() {
                Some(Expr { expr_data: ExprData::Identifier(identifier) })
                    if macro_env.get(&identifier).is_some() => {
                        let macro_lambda = macro_env.get(&identifier).unwrap();
                        let mut macro_list = vec![macro_lambda];
                        macro_list.extend(iter.map(|expr| { expr.quoted() }));
                        let macro_application = ExprData::List(macro_list.into_iter()).to_expr();
                        let expanded_macro = evaluator::eval(macro_application, evaluation_env.clone(), macro_env).unwrap();
                        Some(expanded_macro)
                    },
                _ => {
                    None
                }
            }
        },
        _ => {
            None
        }
    }
}

fn collect_macro_definitions(expr: &Expr, evaluation_env: Env, macro_env: &mut Env) -> bool{
    match &expr.expr_data {
        ExprData::List(iter) => {
            let mut iter = iter.clone();
            match (iter.next(), iter.next()) {
                (Some(Expr { expr_data: ExprData::Identifier(macro_literal) }),
                 Some(Expr { expr_data: ExprData::Identifier(macro_name) }))
                    if macro_literal.as_str() == "define-macro" => {
                        let expr = evaluator::eval(iter.next().unwrap(), evaluation_env.clone(), macro_env).unwrap();
                        match &expr.expr_data {
                            ExprData::Lambda(_, _, _) => {
                                macro_env.create(macro_name, expr);
                                return true
                            },
                            other => panic!("define-macro must be given lambda as argument. Given: {}", other)
                        }
                    }
                _ => ()
            }
        }
        _ => ()
    }

    false
}

