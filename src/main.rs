
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
             .takes_value(true))
        .arg(Arg::new("libraries")
             .short('l')
             .long("libraries")
             .takes_value(true)
             .multiple_values(true)
             .use_delimiter(true))
        .get_matches()
}

fn main() {
    let matches = clap_args();
    let env = default_env::default_env();
    let mut macro_env = Env::new();
    let mut eval_pipeline = 
        |input_string: String| -> Result<Vec<Option<Expr>>, BootLispError> {
            parser::parse(lexer::lex(input_string)?)?.into_iter().map(
                |expr| {
                    let expanded = macro_expander::macro_expand(vec![expr], env.clone(), &mut macro_env);

                    let mut last_expr: Option<Expr> = None;
                    for expr in expanded {
                        last_expr = Some(evaluator::eval(expr,
                                                    env.clone())?);
                    }

                    Ok(last_expr)
                }).collect()
        };

    match matches.values_of("libraries") {
        None => (),
        Some(library_files) => {
            for library_file in library_files {
                let library: String = fs::read_to_string(library_file)
                                            .expect("Failed to open library file")
                                            .parse()
                                            .expect("Failed to parse library file");
                eval_pipeline(library).expect("Failure loading library");
            }
        }
    }

    match matches.value_of("file") {
        None => {
            loop {
                print!(">>");
                io::stdout().flush().unwrap();
                let mut line = String::new();
                io::stdin().read_line(&mut line).expect("Failure reading input");

                for expr in eval_pipeline(line).unwrap() {
                    if let Some(expr) = expr {
                        println!("{}", expr);
                    }
                }
            }
        }
        Some(filename) => {
            let program: String = fs::read_to_string(filename)
                                        .expect("failed to open file")
                                        .parse()
                                        .expect("failed to parse file");
            for expr in eval_pipeline(program).expect("Failure evaluating") {
                if let Some(expr) = expr {
                    println!("{}", expr);
                }
            }
        }
    }
}
