
mod lexer;
mod parser;
mod evaluator;
mod env;
mod call_stack;

fn main() {
    let result = lexer::lex(String::from("ident -501"));
    println!("--> {:?}", result);
}
