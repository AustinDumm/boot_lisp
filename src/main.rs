
#![warn(missing_docs)]

mod lexer;
mod parser;
mod evaluator;
mod env;
mod call_stack;
mod default_env;

fn main() {
    let result = lexer::lex(String::from("ident -501"));
    println!("--> {:?}", result);
}
