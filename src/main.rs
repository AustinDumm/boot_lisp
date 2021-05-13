
mod lexer;

fn main() {
    let result = lexer::lex(String::from("506"));
    println!("--> {:?}", result);
}
