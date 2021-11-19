
#![allow(dead_code)]

mod lexer;
mod parser;
mod evaluator;
mod env;
mod call_stack;
mod default_env;
mod macro_expander;

use std::io::{
    self,
    Write,
};

use std::fs;

use clap::{
    App,
    Arg,
    ArgMatches,
};

use crate::lexer::BootLispError;

use crate::parser::Expr;

use crate::env::Env;

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
    let env = default_env::default_env();
    let mut macro_env = Env::new();
    let mut eval_pipeline = 
        |input_string: String| -> Result<Vec<Expr>, BootLispError> {
            macro_expander::macro_expand(parser::parse(lexer::lex(input_string)?)?, env.clone(), &mut macro_env).into_iter().map(
                |expr| {
                    evaluator::eval(expr,
                                    env.clone())
                }).collect()
        };

    match matches.value_of("file") {
        None => {
            loop {
                print!(">>");
                io::stdout().flush().unwrap();
                let mut line = String::new();
                io::stdin().read_line(&mut line).expect("Failure reading input");

                for expr in eval_pipeline(line).unwrap() {
                    println!("{}", expr);
                }
            }
        }
        Some(filename) => {
            let program: String = fs::read_to_string(filename)
                                        .expect("failed to open file")
                                        .parse()
                                        .expect("failed to parse file");
            for expr in eval_pipeline(program).expect("Failure evaluating") {
                println!("{}", expr);
            }
        }
    }
}
