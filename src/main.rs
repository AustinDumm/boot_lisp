
mod lexer;
mod parser;

fn main() {
    let result = lexer::lex(String::from("ident -501"));
    println!("--> {:?}", result);
}
