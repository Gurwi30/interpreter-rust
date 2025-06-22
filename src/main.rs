mod tokenizer;
mod parser;
mod expr;
mod lox;
mod interpreter;
mod stmt;
mod environment;

use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::tokenizer::{Token, Tokenizer};
use std::env;
use std::fs;
use std::io::{self, Write};
use std::process::exit;

fn main() {
    // DEBUG START
    
    // let file_contents = fs::read_to_string("test.lox").unwrap_or_else(|_| {
    //     writeln!(io::stderr(), "Failed to read file {}", "text.lox").unwrap();
    //     String::new()
    // });
    // 
    // let mut tokenizer = Tokenizer::new(file_contents.clone());
    // let tokens = tokenizer.tokenize().clone();
    // 
    // if tokenizer.had_error {
    //     exit(65);
    // }
    // 
    // let expr = Parser::new(tokens.clone()).parse();
    // 
    // match expr {
    //     Ok(expr) => {
    //         if let Err(err) = interpreter::run(&expr) {
    //             eprintln!("{err}");
    //             exit(70)
    //         }
    //     },
    // 
    //     Err(err) => {
    //         eprintln!("{err}");
    //         exit(65)
    //     }
    // }

    // DEBUG END
    
    
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

        "parse" => parse(&file_contents),
        "evaluate" => eval(&file_contents),
        "run" => run(&file_contents),
        
        _ => {
            writeln!(io::stderr(), "Unknown command: {}", command).unwrap();
            return;
        }
    }

}

fn tokenize(file_contents: String) -> Vec<Token> {
    let mut tokenizer = Tokenizer::new(file_contents);
    let tokens = tokenizer.tokenize().clone();

    if tokenizer.had_error {
        exit(65);
    }
    
    tokens
}

fn parse(file_contents: &String) {
    let tokens = tokenize(file_contents.to_string());
    let parse_res = Parser::new(tokens).expression();

    match parse_res {
        Ok(expr) => println!("{}", expr),
        Err(err) => {
            eprintln!("{err}");
            exit(65)
        }
    }
}

fn eval(file_contents: &String) {
    let tokens = tokenize(file_contents.to_string());
    let parse_res = Parser::new(tokens).expression();

    match parse_res {
        Ok(expr) => {
            match Interpreter::new().eval(&expr) {
                Ok(val) => println!("{}", val),
                Err(err) => {
                    eprintln!("{err}");
                    exit(70);
                }
            }
        },

        Err(err) => {
            eprintln!("{err}");
            exit(65)
        }
    }
}

fn run(file_contents: &String) {
    let tokens = tokenize(file_contents.to_string());
    let parse_res = Parser::new(tokens).parse();

    match parse_res {
        Ok(statements) => {
            if let Err(err) = Interpreter::new().run(&statements) {
                eprintln!("{err}");
                exit(70);
            }
        },

        Err(err) => {
            eprintln!("{err}");
            exit(65)
        }
    }
}
