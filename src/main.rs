mod tokenizer;
mod parser;
mod expr;
mod lox;
mod interpreter;

use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;
use crate::parser::Parser;
use crate::tokenizer::Tokenizer;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        writeln!(io::stderr(), "Usage: {} tokenize <filename>", args[0]).unwrap();
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    writeln!(io::stderr(), "Logs from your program will appear here!").unwrap();

    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        writeln!(io::stderr(), "Failed to read file {}", filename).unwrap();
        String::new()
    });
    
    match command.as_str() {
        "tokenize" => {
            let mut tokenizer = Tokenizer::new(file_contents);
            let tokens = tokenizer.tokenize().clone();
            
            for token in tokens {
                println!("{}", token);
            }

            if tokenizer.had_error {
                exit(65);
            }
        }

        "parse" => {
            let mut tokenizer = Tokenizer::new(file_contents);
            let tokens = tokenizer.tokenize().clone();

            if tokenizer.had_error {
                exit(65);
            }

            let expr = Parser::new(tokens.clone()).parse();
            
            match expr {
                Ok(expr) => {
                    for ex in expr {
                        println!("{}", ex);
                    }
                },
                Err(e) => exit(65)
            }
        },
        
        "evaluate" => {
            let mut tokenizer = Tokenizer::new(file_contents);
            let tokens = tokenizer.tokenize().clone();

            if tokenizer.had_error {
                exit(65);
            }

            let expr = Parser::new(tokens.clone()).parse();
            
            match expr {
                Ok(stmts) => {
                    for expr in stmts {
                        match interpreter::eval(&expr) { 
                            Ok(val) => println!("{}", val),
                            Err(err) => writeln!(io::stderr(), "{}", err).unwrap()
                        }
                    }
                }
                Err(err) => { 
                    eprintln!("{err}");
                    exit(70);
                }
            };
        },
        
        "run" => {
            let mut tokenizer = Tokenizer::new(file_contents);
            let tokens = tokenizer.tokenize().clone();

            if tokenizer.had_error {
                exit(65);
            }

            let expr = Parser::new(tokens.clone()).parse();

            match expr {
                Ok(expr) => {
                    if let Err(err) = interpreter::run(&expr) {
                        eprintln!("{err}");
                        exit(70)
                    }
                },

                Err(err) => {
                    eprintln!("{err}");
                    exit(65)
                }
            }  
        },
        
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
