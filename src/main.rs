
mod lexer;

fn main() {
    let result = lexer::lex(String::from("This is a test"));
    println!("--> {:?}", result);
}
