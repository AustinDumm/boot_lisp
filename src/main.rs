
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
