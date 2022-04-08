
use std::sync::RwLock;

use super::arithmetic_functions::*;
use super::bitwise_functions::*;
use super::logical_functions::*;
use super::comparison_functions::*;
use super::lambda_functions::*;
use super::conditional_functions::*;
use super::list_functions::*;
use super::quote_functions::*;
use super::type_functions::*;
use super::env_functions::*;
use super::eval_functions::*;
use super::continuation_functions::*;

use crate::parser::ExprData;

use crate::env::Env;


pub fn default_env(sym_number: RwLock<u64>) -> Env {
    Env::containing(
        vec![
            ("+".to_string(), ExprData::Function("+".to_string(), add).to_expr()),
            ("-".to_string(), ExprData::Function("-".to_string(), sub).to_expr()),
            ("*".to_string(), ExprData::Function("*".to_string(), mul).to_expr()),
            ("/".to_string(), ExprData::Function("/".to_string(), div).to_expr()),
            ("%".to_string(), ExprData::Function("%".to_string(), modulo).to_expr()),

            ("bit-and".to_string(), ExprData::Function("bit-and".to_string(), bit_and).to_expr()),
            ("bit-or".to_string(), ExprData::Function("bit-or".to_string(), bit_or).to_expr()),
            ("bit-xor".to_string(), ExprData::Function("bit-xor".to_string(), bit_xor).to_expr()),
            ("bit-not".to_string(), ExprData::Function("bit-not".to_string(), bit_not).to_expr()),

            ("and".to_string(), ExprData::Function("and".to_string(), and).to_expr()),
            ("or".to_string(), ExprData::Function("or".to_string(), or).to_expr()),
            ("xor".to_string(), ExprData::Function("xor".to_string(), xor).to_expr()),
            ("not".to_string(), ExprData::Function("not".to_string(), not).to_expr()),

            ("<".to_string(), ExprData::Function("<".to_string(), lt).to_expr()),
            (">".to_string(), ExprData::Function(">".to_string(), gt).to_expr()),
            ("<=".to_string(), ExprData::Function("<=".to_string(), leq).to_expr()),
            (">=".to_string(), ExprData::Function(">=".to_string(), geq).to_expr()),
            ("=".to_string(), ExprData::Function("=".to_string(), eq).to_expr()),

            ("eq?".to_string(), ExprData::Function("eq?".to_string(), equality).to_expr()),

            ("cons".to_string(), ExprData::Function("cons".to_string(), cons).to_expr()),
            ("first".to_string(), ExprData::Function("first".to_string(), first).to_expr()),
            ("rest".to_string(), ExprData::Function("rest".to_string(), rest).to_expr()),
            ("list".to_string(), ExprData::Function("list".to_string(), list_impl).to_expr()),
            ("append".to_string(), ExprData::Function("append".to_string(), append_impl).to_expr()),

            ("void?".to_string(), ExprData::Function("void?".to_string(), is_void).to_expr()),
            ("bool?".to_string(), ExprData::Function("bool?".to_string(), is_bool).to_expr()),
            ("integer?".to_string(), ExprData::Function("integer?".to_string(), is_integer).to_expr()),
            ("identifier?".to_string(), ExprData::Function("identifier?".to_string(), is_identifier).to_expr()),
            ("lambda?".to_string(), ExprData::Function("lambda?".to_string(), is_lambda).to_expr()),
            ("function?".to_string(), ExprData::Function("function?".to_string(), is_function).to_expr()),
            ("applicable?".to_string(), ExprData::Function("applicable?".to_string(), is_applicable).to_expr()),
            ("list?".to_string(), ExprData::Function("list?".to_string(), is_list).to_expr()),
            ("nil?".to_string(), ExprData::Function("nil?".to_string(), is_nil).to_expr()),
            ("empty?".to_string(), ExprData::Function("empty?".to_string(), is_nil).to_expr()),

            ("quote".to_string(), ExprData::Function("quote".to_string(), quote).to_expr()),
            ("quasiquote".to_string(), ExprData::Function("quasiquote".to_string(), quasiquote).to_expr()),
            ("unquote".to_string(), ExprData::Function("unquote".to_string(), unquote).to_expr()),

            ("set!".to_string(), ExprData::Function("set!".to_string(), set).to_expr()),
            ("create!".to_string(), ExprData::Function("create!".to_string(), create).to_expr()),

            ("begin".to_string(), ExprData::Function("begin".to_string(), begin).to_expr()),
            ("read".to_string(), ExprData::Function("read".to_string(), read).to_expr()),
            ("input".to_string(), ExprData::Function("input".to_string(), input).to_expr()),
            ("gensym".to_string(), ExprData::Function("gensym".to_string(), gensym).to_expr()),
            ("eval".to_string(), ExprData::Function("eval".to_string(), eval).to_expr()),
            ("apply".to_string(), ExprData::Function("apply".to_string(), apply).to_expr()),
            ("exit".to_string(), ExprData::Function("exit".to_string(), exit).to_expr()),

            ("lambda".to_string(), ExprData::Function("lambda".to_string(), build_lambda).to_expr()),
            ("void".to_string(), ExprData::Function("void".to_string(), build_void).to_expr()),
            ("if".to_string(), ExprData::Function("if".to_string(), if_impl).to_expr()),

            ("prompt".to_string(), ExprData::Function("prompt".to_string(), prompt).to_expr()),
            ("call-with-control".to_string(), ExprData::Function("call-with-control".to_string(), call_with_control).to_expr()),

            ("nil".to_string(), ExprData::nil().to_expr()),
        ].into_iter().collect(),
        Some(sym_number)
    )
}
