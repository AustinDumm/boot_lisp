
use crate::parser::{
    Expr,
    ExprData,
};

use crate::evaluator;

use crate::env::Env;

pub fn macro_expand(exprs: Vec<Expr>, evaluation_env: Env, macro_env: &mut Env) -> Vec<Expr> {
    let exprs = collect_macro_definitions(exprs, evaluation_env.clone(), macro_env);
    let exprs = exprs
        .into_iter()
        .map(|expr| {
            expand_macro(expr, evaluation_env.clone(), macro_env)
        })
        .collect();

    exprs
}

fn expand_macro(expr: Expr, evaluation_env: Env, macro_env: &mut Env) -> Expr {
    match expr.expr_data {
        ExprData::List(mut iter) => {
            match iter.next() {
                Some(Expr { expr_data: ExprData::Identifier(identifier) })
                    if macro_env.get(&identifier).is_some() => {
                        let macro_lambda = macro_env.get(&identifier).unwrap();
                        let mut macro_list = vec![macro_lambda];
                        macro_list.extend(iter.map(|expr| { expr.quoted() }));
                        let macro_application = ExprData::List(macro_list.into_iter()).to_expr();
                        let expanded_macro = evaluator::eval(macro_application, evaluation_env.clone()).unwrap();
                        expand_macro(expanded_macro, evaluation_env, macro_env)
                    },
                Some(expr) => {
                    // No macro hit, recurse
                    let mut result_list = vec![expand_macro(expr, evaluation_env.clone(), macro_env)];
                    result_list.extend(iter.map(|expr| {
                        expand_macro(expr, evaluation_env.clone(), macro_env)
                    }));
                    ExprData::List(result_list.into_iter()).to_expr()
                },
                None => {
                    ExprData::List(vec![].into_iter()).to_expr()
                }
            }
        },
        _ => {
            expr
        }
    }
}

fn collect_macro_definitions(exprs: Vec<Expr>, evaluation_env: Env, macro_env: &mut Env) -> Vec<Expr> {
    let mut non_macro_exprs: Vec<Expr> = vec![];
    for expr in exprs {
        match expr.expr_data {
            ExprData::List(list_iter) => {
                let mut iter = list_iter.clone();
                match (iter.next(), iter.next()) {
                    (Some(Expr { expr_data: ExprData::Identifier(macro_literal) }),
                     Some(Expr { expr_data: ExprData::Identifier(macro_name) }))
                        if macro_literal.as_str() == "macro" => {
                            let expr = evaluator::eval(iter.next().unwrap(), evaluation_env.clone()).unwrap();
                            match &expr.expr_data {
                                ExprData::Lambda(_, _, _) => {
                                    macro_env.create(macro_name, expr);
                                },
                                _ => panic!("macro must be given lambda as argument")
                            }
                        },
                    _ => {
                        non_macro_exprs.push(ExprData::List(list_iter).to_expr());
                        continue
                    }
                }
            }
            _ => {
                non_macro_exprs.push(expr);
                continue 
            },
        }
    }

    non_macro_exprs
}

