
use crate::parser::{
    Expr,
    ExprData,
};

use crate::call_stack::{
    CallStack,
    StackFrame,
};


pub fn quote(accumulator: &mut Option<Expr>, frame: Option<StackFrame>, stack: &mut CallStack) -> Option<StackFrame> {
    if let Some(active_frame) = frame {
        if let Expr { expr_data: ExprData::List(list) } = &active_frame.expr {
            let mut list = list.clone();
            if let (Some(quoted_item), None) = (list.next(), list.next()) {
                *accumulator = Some(quoted_item);
                stack.pop_frame()
            } else {
                panic!("Invalid number of arguments provided to quote")
            }
        } else {
            panic!("No arguments provided to quote")
        }
    } else {
        panic!("No frame found for evaluation of quoted expression")
    }
}

pub fn quasiquote(_accumulator: &mut Option<Expr>, frame: Option<StackFrame>, _stack: &mut CallStack) -> Option<StackFrame> {
    fn apply_quasiquote(expr: Expr, depth: usize) -> Expr {
        match expr.expr_data {
            ExprData::List(mut iter) => {
                match iter.next() {
                    Some(Expr { expr_data: ExprData::Identifier(identifier) }) 
                        if identifier.as_str() == "quasiquote" => {
                            let mut expr_list = vec![ExprData::Identifier("cons".to_string()).to_expr()];
                            expr_list.push(ExprData::Identifier("quasiquote".to_string()).to_expr().quoted());
                            expr_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth + 1));
                            ExprData::List(expr_list.into_iter()).to_expr()
                    },
                    Some(Expr { expr_data: ExprData::Identifier(identifier) })
                        if identifier.as_str() == "unquote" || identifier.as_str() == "unquote-splicing" => {
                            if depth > 0 {
                                let mut expr_list = vec![ExprData::Identifier("cons".to_string()).to_expr()];
                                expr_list.push(ExprData::Identifier(identifier).to_expr().quoted());
                                expr_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth - 1));
                                ExprData::List(expr_list.into_iter()).to_expr()
                            } else if let (Some(next), None, true) = (iter.next(), iter.next(), identifier.as_str() == "unquote") {
                                next
                            } else {
                                panic!("Illegal use of {} in quasiquoted expression", identifier)
                            }
                    },
                    Some(expr) => {
                        let mut expr_list = vec![ExprData::Identifier("append".to_string()).to_expr()];
                        expr_list.push(apply_quasiquote_list(expr, depth));
                        if iter.clone().peekable().peek().is_some() {
                            expr_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth));
                        }
                        ExprData::List(expr_list.into_iter()).to_expr()
                    },
                    None => ExprData::List(vec![].into_iter()).to_expr().quoted(),
                }
            },
            expr => expr.to_expr().quoted()
        }
    }

    fn apply_quasiquote_list(expr: Expr, depth: usize) -> Expr {
        match expr.expr_data {
            ExprData::List(mut iter) => {
                match iter.next() {
                    Some(Expr { expr_data: ExprData::Identifier(identifier) })
                        if identifier.as_str() == "quasiquote" => {
                            let mut expr_list = vec![ExprData::Identifier("list".to_string()).to_expr()];
                            let mut inner_list = vec![ExprData::Identifier("cons".to_string()).to_expr(),
                                                      ExprData::Identifier("quasiquote".to_string()).to_expr().quoted()];
                            inner_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth + 1));
                            expr_list.push(ExprData::List(inner_list.into_iter()).to_expr());
                            ExprData::List(expr_list.into_iter()).to_expr()
                    },
                    Some(Expr { expr_data: ExprData::Identifier(identifier) })
                        if identifier.as_str() == "unquote" || identifier.as_str() == "unquote-splicing" => {
                            if depth > 0 {
                                let mut expr_list = vec![ExprData::Identifier("list".to_string()).to_expr()];
                                let mut inner_list = vec![ExprData::Identifier("cons".to_string()).to_expr(),
                                                          ExprData::Identifier(identifier).to_expr().quoted()];
                                inner_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth - 1));
                                expr_list.push(ExprData::List(inner_list.into_iter()).to_expr());
                                ExprData::List(expr_list.into_iter()).to_expr()
                            } else if identifier.as_str() == "unquote" {
                                let mut expr_list = vec![ExprData::Identifier("list".to_string()).to_expr()];
                                expr_list.extend(iter);
                                ExprData::List(expr_list.into_iter()).to_expr()
                            } else {
                                let mut expr_list = vec![ExprData::Identifier("append".to_string()).to_expr()];
                                expr_list.extend(iter);
                                ExprData::List(expr_list.into_iter()).to_expr()
                            }
                    },
                    Some(expr) => {
                        let mut expr_data = vec![ExprData::Identifier("list".to_string()).to_expr()];
                        let mut inner_list = vec![ExprData::Identifier("append".to_string()).to_expr()];
                        inner_list.push(apply_quasiquote_list(expr, depth));
                        inner_list.push(apply_quasiquote(ExprData::List(iter).to_expr(), depth));
                        expr_data.push(ExprData::List(inner_list.into_iter()).to_expr());
                        ExprData::List(expr_data.into_iter()).to_expr()
                    },
                    None => ExprData::List(vec![ExprData::List(vec![].into_iter()).to_expr()].into_iter()).to_expr().quoted(),
                }
            },
            expr => ExprData::List(vec![expr.to_expr()].into_iter()).to_expr().quoted(),
        }
    }

    match frame {
        Some(
            StackFrame { expr: Expr { expr_data: ExprData::List(mut iter) },
                         env,
                         rib: _,
                         is_prompt: _,
                         is_expanded: _, }) =>  {
            if let (Some(expr), None) = (iter.next(), iter.next()) {
                let quasiquoted_expr = apply_quasiquote(expr, 0);
                Some(StackFrame::new(quasiquoted_expr, env.clone(), vec![]))
            } else {
                panic!("Error: Invalid arity to quasiquote. Quasiquote takes only one argument")
            }
        },
        _ => panic!("Unexpected expr of non-list type for quasiquote")
    }
}

pub fn unquote(_accumulator: &mut Option<Expr>, _frame: Option<StackFrame>, _stack: &mut CallStack) -> Option<StackFrame> {
    panic!("Unexpected unquote found outside of quasiquoted expression")
}

pub fn unquote_splicing(_accumulator: &mut Option<Expr>, _frame: Option<StackFrame>, _stack: &mut CallStack) -> Option<StackFrame> {
    panic!("Unexpected unquote-splicing found outside of quasiquoted list expression")
}

