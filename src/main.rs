
mod lexer;
mod parser;
mod evaluator;

fn main() {
    let result = lexer::lex(String::from("ident -501"));
    println!("--> {:?}", result);
}
