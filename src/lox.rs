use std::fmt::format;
use crate::tokenizer::{Token, TokenType};

pub fn report(line: usize, message: &str) {
    eprintln!("[line {line}] Error: {message}");
}

pub fn error(token: &Token, message: &str) {
    match token.token_type {
        TokenType::EOF => {
            report(token.line, &format!("at end {message}"));
        }
        _ => {
            report(token.line, &format!(" at '{}' {message}", token.lexeme));
        }
    }
}