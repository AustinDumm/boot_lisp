
#![allow(dead_code)]

mod lexer;
mod parser;
mod evaluator;
mod env;
mod call_stack;
mod default_env;
mod macro_expander;

use std::{
    io::{
        self,
        Write,
    },
    sync::RwLock,
};

use std::fs;

use clap::{
    App,
    Arg,
    ArgMatches,
};

use crate::lexer::BootLispError;

use crate::parser::{
    Expr,
    ExprData,
};

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
        .arg(Arg::new("standard_library")
             .long("standard-library")
             .takes_value(true)
             .multiple_values(false)
             .default_value("./library/blib.bl"))
        .get_matches()
}

fn main() {
    let matches = clap_args();
    let sym_number: RwLock<u64> = RwLock::new(0);
    let env = default_env::default_env(sym_number);
    let mut macro_env = Env::new(Some(RwLock::new(0)));
    let mut eval_pipeline = 
        |input_string: String| -> Result<Vec<Expr>, BootLispError> {
            parser::parse(lexer::lex(input_string)?)?.into_iter().map(
                |expr| {
                    evaluator::eval(expr, env.clone(), &mut macro_env)
                }).collect()
        };

    let standard_library = fs::read_to_string(matches.value_of("standard_library").unwrap()).expect("Failed to read standard library");
    eval_pipeline(standard_library).expect("Failure loading standard library");

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
                    match &expr.expr_data {
                        ExprData::Void => (),
                        _ => println!("{}", expr),
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
                match &expr.expr_data {
                    ExprData::Void => (),
                    _ => println!("{}", expr),
                }
            }
        }
    }
}
