use std::fmt::{Display, Formatter};
use std::str::FromStr;

pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Semicolon,
    Dot,
    Plus,
    Minus,
    Star,
    Slash,
    EOF
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LeftParen => write!(f, "LEFT_PAREN"),
            TokenType::RightParen => write!(f, "RIGHT_PAREN"),
            TokenType::LeftBrace => write!(f, "LEFT_BRACE"),
            TokenType::RightBrace => write!(f, "RIGHT_BRACE"),
            TokenType::Comma => write!(f, "COMMA"),
            TokenType::Semicolon => write!(f, "SEMICOLON"),
            TokenType::Dot => write!(f, "DOT"),
            TokenType::Plus => write!(f, "PLUS"),
            TokenType::Minus => write!(f, "MINUS"),
            TokenType::Star => write!(f, "STAR"),
            TokenType::Slash => write!(f, "SLASH"),
            TokenType::EOF => write!(f, "EOF"),
        }
    }
}

impl FromStr for TokenType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "(" => Ok(TokenType::LeftParen),
            ")" => Ok(TokenType::RightParen),
            "{" => Ok(TokenType::LeftBrace),
            "}" => Ok(TokenType::RightBrace),
            "," => Ok(TokenType::Comma),
            ";" => Ok(TokenType::Semicolon),
            "." => Ok(TokenType::Dot),
            "+" => Ok(TokenType::Plus),
            "-" => Ok(TokenType::Minus),
            "*" => Ok(TokenType::Star),
            "/" => Ok(TokenType::Slash),
            _ => Err(())
        }
    }
}

pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.token_type, self.lexeme, "null")
    }
}

pub struct Tokenizer {
    tokens: Vec<Token>,
    current_idx: usize,
    line: usize,
    source: String
}

impl Tokenizer {

    pub fn new(source: String) -> Tokenizer {
        Tokenizer {
            tokens: Vec::new(),
            current_idx: 0,
            line: 1,
            source
        }
    }

    pub fn tokenize(&mut self) -> &Vec<Token> {
        while self.current_idx < self.source.chars().count() {
            let start = self.current_idx;
            let next_val: String = self.get_next();

            if next_val == "\n" {
                self.line += 1;
                continue;
            }

            let lexeme = &self.source.to_string()[start..self.current_idx];

            match TokenType::from_str(next_val.as_str()) {
                Ok(token_type) => {
                    self.add_token(token_type, lexeme.to_string(), self.line);
                },
                
                Err(_) => {
                    eprintln!("[line {}] Error: Unexpected character: {}", self.line, lexeme);
                }
            }
        }

        self.add_token(TokenType::EOF, "".to_string(), self.line);
        &self.tokens
    }

    fn add_token(&mut self, token_type: TokenType, lexeme: String, line: usize) {
        self.tokens.push(Token { token_type, lexeme, line });
    }

    fn get_next(&mut self) -> String {
        self.current_idx += 1;

        self.source.chars()
            .nth(self.current_idx - 1)
            .unwrap()
            .to_string()
    }

}