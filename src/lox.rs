use crate::tokenizer::{Token, TokenType};

pub fn report(line: usize, message: &str) {
    eprintln!("[line {line}] Error: {message}");
}

pub fn error(token: &Token, message: &str) {
    match token.token_type {
        TokenType::EOF => {
            report(token.line, " at end", message);
        }
        _ => {
            report(token.line, &format!(" at '{}'", token.lexeme), message);
        }
    }
}