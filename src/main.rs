
mod lexer;
mod parser;
mod evaluator;
mod env;

fn main() {
    let result = lexer::lex(String::from("ident -501"));
    println!("--> {:?}", result);
}
