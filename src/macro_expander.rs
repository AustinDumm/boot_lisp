
use crate::parser::{
    Expr,
    ExprData,
};

use crate::evaluator;

use crate::env::Env;

pub fn macro_expand(expr: Expr, evaluation_env: Env, macro_env: &mut Env) -> (bool, Expr) {
    let (searched_expr, definition_found) = collect_macro_definitions(expr, evaluation_env.clone(), macro_env);

    if definition_found {
        (false, ExprData::Void.to_expr())
    } else {
        expand_macro(searched_expr, evaluation_env.clone(), macro_env)
    }
}

fn expand_macro(expr: Expr, evaluation_env: Env, macro_env: &mut Env) -> (bool, Expr) {
    match expr.expr_data {
        ExprData::List(mut iter) => {
            match iter.next() {
                Some(Expr { expr_data: ExprData::Identifier(identifier) })
                    if macro_env.get(&identifier).is_some() => {
                        let macro_lambda = macro_env.get(&identifier).unwrap();
                        let mut macro_list = vec![macro_lambda];
                        macro_list.extend(iter.map(|expr| { expr.quoted() }));
                        let macro_application = ExprData::List(macro_list.into_iter()).to_expr();
                        let expanded_macro = evaluator::eval(macro_application, evaluation_env.clone(), macro_env).unwrap();
                        (true, expanded_macro)
                    },
                Some(expr) => {
                    (false, ExprData::List(
                        vec![expr]
                            .into_iter()
                            .chain(iter)
                            .collect::<Vec<Expr>>()
                            .into_iter()).to_expr())
                },
                None => {
                    (false, ExprData::List(vec![].into_iter()).to_expr())
                }
            }
        },
        expr => {
            (false, expr.to_expr())
        }
    }
}

fn collect_macro_definitions(expr: Expr, evaluation_env: Env, macro_env: &mut Env) -> (Expr, bool) {
    match expr.expr_data {
        ExprData::List(mut iter) => {
            match (iter.next(), iter.next()) {
                (Some(Expr { expr_data: ExprData::Identifier(macro_literal) }),
                 Some(Expr { expr_data: ExprData::Identifier(macro_name) }))
                    if macro_literal.as_str() == "define-macro" => {
                        let expr = evaluator::eval(iter.next().unwrap(), evaluation_env.clone(), macro_env).unwrap();
                        match &expr.expr_data {
                            ExprData::Lambda(_, _, _) => {
                                macro_env.create(macro_name, expr);
                                return (ExprData::Void.to_expr(), true)
                            },
                            other => panic!("define-macro must be given lambda as argument. Given: {}", other)
                        }
                    }
                (first, second) => (ExprData::List(vec![first, second]
                                                  .into_iter()
                                                  .filter(|x| x.is_some())
                                                  .map(|x| x.unwrap())
                                                  .chain(iter)
                                                  .collect::<Vec<Expr>>()
                                                  .into_iter()).to_expr(), false)
            }
        }
        _ => (expr, false)
    }
}
