use std::cmp::PartialEq;
use std::fmt::{Debug, Display, Formatter};
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
    Bang,
    Equal,
    EqualEqual,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
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
            TokenType::Equal => write!(f, "EQUAL"),
            TokenType::EqualEqual => write!(f, "EQUAL_EQUAL"),
            TokenType::Bang => write!(f, "BANG"),
            TokenType::BangEqual => write!(f, "BANG_EQUAL"),
            TokenType::Greater => write!(f, "GREATER"),
            TokenType::GreaterEqual => write!(f, "GREATER_EQUAL"),
            TokenType::Less => write!(f, "LESS"),
            TokenType::LessEqual => write!(f, "LESS_EQUAL"),
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
            "=" => Ok(TokenType::Equal),
            "==" => Ok(TokenType::EqualEqual),
            "!" => Ok(TokenType::Bang),
            "!=" => Ok(TokenType::BangEqual),
            ">" => Ok(TokenType::Greater),
            ">=" => Ok(TokenType::GreaterEqual),
            "<" => Ok(TokenType::Less),
            "<=" => Ok(TokenType::LessEqual),
            "EOF" => Ok(TokenType::EOF),
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
    pub invalid: bool,
    source_size: usize,
    source: String,
}

impl PartialEq for TokenType {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Tokenizer {

    pub fn new(source: String) -> Tokenizer {
        Tokenizer {
            tokens: Vec::new(),
            current_idx: 0,
            line: 1,
            invalid: false,
            source_size: source.chars().count(),
            source
        }
    }

    pub fn tokenize(&mut self) -> &Vec<Token> {
        while self.current_idx < self.source.chars().count() {
            let start = self.current_idx;
            let next_val: String = self.poll();

            match next_val.as_str() {
                " " | "\t" | "\r" => {},
                "\n" => self.line += 1,
                _ => {
                    match TokenType::from_str(next_val.as_str()) {
                        Ok(token_type) => {
                            let final_token = match token_type {
                                TokenType::Bang if self.match_next('=') => TokenType::BangEqual,
                                TokenType::Equal if self.match_next('=') => TokenType::EqualEqual,
                                TokenType::Greater if self.match_next('=') => TokenType::GreaterEqual,
                                TokenType::Less if self.match_next('=') => TokenType::LessEqual,
                                _ => token_type,
                            };

                            let lexeme = &self.source.to_string()[start..self.current_idx];

                            if final_token == TokenType::Slash {
                                if !self.match_next('/') {
                                    self.add_token(final_token, lexeme.to_string(), self.line);
                                    continue;
                                }

                                while self.peek() != '\n' && !self.is_at_end() {
                                    self.poll();
                                }
                            }
                        },

                        Err(_) => {
                            self.invalid = true;
                            eprintln!("[line {}] Error: Unexpected character: {}", self.line, self.source.chars().nth(start).unwrap());
                        }
                    }
                }
            }
        }

        self.add_token(TokenType::EOF, "".to_string(), self.line);
        &self.tokens
    }

    fn add_token(&mut self, token_type: TokenType, lexeme: String, line: usize) {
        self.tokens.push(Token { token_type, lexeme, line });
    }

    fn poll(&mut self) -> String {
        self.current_idx += 1;

        self.source.chars()
            .nth(self.current_idx - 1)
            .unwrap()
            .to_string()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }

        return self.source.chars().nth(self.current_idx).unwrap();
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        if self.source.chars().nth(self.current_idx) != Option::from(expected) {
            return false;
        }

        self.current_idx += 1;
        true
    }

    fn is_at_end(&self) -> bool {
        self.current_idx >= self.source_size
    }
}