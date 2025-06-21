use std::cmp::PartialEq;
use std::collections::HashMap;
use std::fmt::{Display, Formatter };
use std::str::FromStr;
use lazy_static::lazy_static;
use crate::lox;

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, TokenType> = HashMap::from([
        ("and", TokenType::And),
        ("class", TokenType::Class),
        ("else", TokenType::Else),
        ("false", TokenType::False),
        ("for", TokenType::For),
        ("fun", TokenType::Fun),
        ("if", TokenType::If),
        ("nil", TokenType::Nil),
        ("or", TokenType::Or),
        ("print", TokenType::Print),
        ("return", TokenType::Return),
        ("super", TokenType::Super),
        ("this", TokenType::This),
        ("true", TokenType::True),
        ("var", TokenType::Var),
        ("while", TokenType::While),
    ]);
}

#[derive(Clone, Eq, PartialEq)]
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
    String,
    Number,
    Identifier,
    EOF,

    // KEYWORDS

    If,
    Else,
    For,
    While,

    True,
    False,
    Nil,

    And,
    Or,

    Fun,
    Return,

    Class,
    Super,
    This,

    Var,

    Print
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
            TokenType::String => write!(f, "STRING"),
            TokenType::Number => write!(f, "NUMBER"),
            TokenType::Identifier => write!(f, "IDENTIFIER"),
            TokenType::EOF => write!(f, "EOF"),

            TokenType::If => write!(f, "IF"),
            TokenType::Else => write!(f, "ELSE"),
            TokenType::For => write!(f, "FOR"),
            TokenType::While => write!(f, "WHILE"),
            TokenType::True => write!(f, "TRUE"),
            TokenType::False => write!(f, "FALSE"),
            TokenType::Nil => write!(f, "NIL"),
            TokenType::And => write!(f, "AND"),
            TokenType::Or => write!(f, "OR"),
            TokenType::Fun => write!(f, "FUN"),
            TokenType::Return => write!(f, "RETURN"),
            TokenType::Class => write!(f, "CLASS"),
            TokenType::Super => write!(f, "SUPER"),
            TokenType::This => write!(f, "THIS"),
            TokenType::Var => write!(f, "VAR"),
            TokenType::Print => write!(f, "PRINT"),
        }
    }
}

impl FromStr for TokenType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if is_alpha(s.chars().nth(0).unwrap_or('\0')) {
            return Ok(TokenType::Identifier);
        }

        if s.trim().parse::<f64>().is_ok() {
            return Ok(TokenType::Number);
        }

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
            "\"" => Ok(TokenType::String),
            "EOF" => Ok(TokenType::EOF),
            _ => Err(())
        }
    }
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub literal: Option<Literal>,
    pub line: usize
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let literal = self.literal.as_ref()
            .map(|l| l.to_string())
            .unwrap_or("null".to_string());
        
        write!(f, "{} {} {}", self.token_type, self.lexeme, literal)
    }
}

#[derive(Clone, PartialEq)]
pub enum Literal {
    String(String),
    Integer(isize),
    Float(f64),
    Boolean(bool),
    Nil
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::String(s) => write!(f, "{s}"),
            Literal::Integer(n) => write!(f, "{n}"),
            Literal::Float(n) => write!(f, "{n:?}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

pub struct Tokenizer {
    tokens: Vec<Token>,
    current_idx: usize,
    line: usize,
    pub had_error: bool,
    source_size: usize,
    source: String,
}

impl Tokenizer {

    pub fn new(source: String) -> Tokenizer {
        Tokenizer {
            tokens: Vec::new(),
            current_idx: 0,
            line: 1,
            had_error: false,
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

                            match final_token {
                                TokenType::Slash => {
                                    if !self.match_next('/') {
                                        self.add_token(final_token, lexeme.to_string(), None, self.line);
                                        continue;
                                    }

                                    while self.peek() != '\n' && !self.is_at_end() {
                                        self.poll();
                                    }
                                },

                                TokenType::String => self.string(),
                                TokenType::Number => self.number(),
                                TokenType::Identifier => self.identifier(),

                                _ => self.add_token(final_token, lexeme.to_string(), None, self.line)
                            }
                        },

                        Err(_) => {
                            self.had_error = true;
                            lox::report(self.line, format!("Unexpected character: {}", self.source.chars().nth(start).unwrap()));
                        }
                    }
                }
            }
        }

        self.add_token(TokenType::EOF, "".to_string(), None, self.line);
        &self.tokens
    }

    fn string(&mut self) {
        let start = self.current_idx - 1;

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.poll();
        }

        if self.is_at_end() {
            self.had_error = true;
            lox::report(self.line, "Unterminated string.".to_string());

            return;
        }

        self.poll();

        let lexeme = self.source.as_str()[start..self.current_idx].to_string();
        let value = self.source.as_str()[start + 1..self.current_idx - 1].to_string();

        self.add_token(TokenType::String, lexeme, Some(Literal::String(value)), self.line);
    }

    fn number(&mut self) {
        let start = self.current_idx - 1;
        //let mut is_float = false;

        while is_digit(self.peek()) {
            self.poll();
        }

        if self.peek() == '.' && is_digit(self.peek_next()) {
            //is_float = true;
            self.poll();

            while is_digit(self.peek()) {
                self.poll();
            }
        }

        let value = self.source.as_str()[start..self.current_idx].to_string();

        // if !is_float {
        //     self.add_token(TokenType::Number, value.clone(), Some(Literal::Integer(value.parse::<isize>().unwrap())), self.line);
        //     return;
        // }

        self.add_token(TokenType::Number, value.clone(), Some(Literal::Float(value.parse::<f64>().unwrap())), self.line);
    }

    fn identifier(&mut self) {
        let start = self.current_idx - 1;

        while is_alphanumeric(self.peek()) {
            self.poll();
        }

        let lexeme = self.source.as_str()[start..self.current_idx].to_string();
        
        match KEYWORDS.get(lexeme.as_str()) { 
            Some(keyword) => self.add_token(keyword.clone(), lexeme, None, self.line),
            None => self.add_token(TokenType::Identifier, lexeme, None, self.line)
        }
    }

    fn add_token(&mut self, token_type: TokenType, lexeme: String, literal: Option<Literal>, line: usize) {
        self.tokens.push(Token { token_type, lexeme, literal, line });
    }

    fn poll(&mut self) -> String {
        self.current_idx += 1;

        self.source.chars()
            .nth(self.current_idx - 1)
            .unwrap()
            .to_string()
    }

    fn peek(&self) -> char {
        self.source.chars().nth(self.current_idx).unwrap_or('\n')
    }

    fn peek_next(&self) -> char {
        self.source.chars().nth(self.current_idx + 1).unwrap_or('\0')
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

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_alpha(c) || is_digit(c)
}