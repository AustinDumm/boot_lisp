
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

use std::fs;

use clap::{
    App,
    Arg,
    ArgGroup,
    ArgMatches,
};

use crate::lexer::{
    BootLispError,
};

use crate::parser::{
    Expr,
};

fn clap_args() -> ArgMatches {
    App::new("boot_lisp")
        .version("1.0")
        .author("Austin Dumm <dummmagic@gmail.com>")
        .about("Interpreter for boot_lisp")
        .arg(Arg::new("file")
             .short('f')
             .long("file")
             .takes_value(true)).get_matches()
}

fn main() {
    let matches = clap_args();
    let eval_pipeline = 
        |input_string: String| -> Result<Expr, BootLispError> {
            evaluator::eval(parser::parse(lexer::lex(input_string)?)?,
                            default_env::default_env())
        };

    match matches.value_of("file") {
        None => {
            loop {
                print!(">>");
                io::stdout().flush().unwrap();
                let mut line = String::new();
                io::stdin().read_line(&mut line).expect("Failure reading input");

                println!("{:?}", 
                    eval_pipeline(line).unwrap()
                );
            }
        }
        Some(filename) => {
            let program: String = fs::read_to_string(filename)
                                        .expect("failed to open file")
                                        .parse()
                                        .expect("failed to parse file");
            println!("{:?}", eval_pipeline(program));
        }
    }
}
