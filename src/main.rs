
#![warn(missing_docs)]

mod lexer;
mod parser;
mod evaluator;
mod env;
mod call_stack;
mod default_env;

use std::io::{
    self,
    Write,
};

fn main() {
    let result = lexer::lex(String::from("ident -501"));
    println!("--> {:?}", result);
    loop {
        print!(">>");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        io::stdin().read_line(&mut line).expect("Failure reading input");

        println!("{:?}", 
            evaluator::eval(
                parser::parse(
                    lexer::lex(line).unwrap()
                ).unwrap(),
                default_env::default_env()
            ).unwrap()
        );
    }
}
