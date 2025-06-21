use crate::tokenizer::{Token, TokenType};

pub fn report(line: usize, message: String) {
    eprintln!("[line {line}] Error: {message}")
}

pub fn error(token: &Token, message: &str) {
    if token.token_type == TokenType::EOF {
        report(token.line, format!(" at end, {message}"));
        return;
    }

    let lexeme = &token.lexeme;
    report(token.line, format!("at '{lexeme}' {message}"));
}