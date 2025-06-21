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

    let mut tokenizer = Tokenizer::new(file_contents);
    
    let tokens = tokenizer.tokenize();
    let expr = Parser::new(tokens.clone()).parse();
    
    match command.as_str() {
        "tokenize" => {
            for token in tokens {
                println!("{}", token);
            }

            if tokenizer.had_error {
                exit(65);
            }
        }

        "parse" => {
            match expr {
                Some(expr) => println!("{}", expr),
                None => exit(65)
            }
        },
        
        "evaluate" => {
            match expr {
                Some(expr) => println!("{}", interpreter::eval(&expr)),
                None => exit(65)
            };
        }
        
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }
}
