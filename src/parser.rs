use crate::expr::Expr;
use crate::tokenizer::{Literal, Token, TokenType};
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::Formatter;
use crate::lox;

#[derive(Debug)]
pub struct ParseError {
    pub line: usize,
    pub message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.message)
    }
}

impl std::error::Error for ParseError { }

pub struct Parser {
    tokens: Vec<Token>,
    cur_idx: usize,
}

impl PartialEq<&TokenType> for &Token {
    fn eq(&self, other: &&TokenType) -> bool {
        &self.token_type == *other
    }
}

impl Parser {
    
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            cur_idx: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut statements: Vec<Expr> = Vec::new();

        while !self.is_at_end() {
            statements.push(self.statement()?)
        }

        Ok(statements)
        //self.expression().ok()
    }

    fn statement(&mut self) -> Result<Expr, ParseError> {
        if self.match_types(&[TokenType::Print]) {
            return self.print_statement();
        }

        self.expression_statement()
    }

    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.comparison()?;

        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.comparison()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.term()?;

        while self.match_types(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.term()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.factor()?;

        while self.match_types(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.unary()?;

        while self.match_types(&[TokenType::Slash, TokenType::Star]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.unary()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator: Token = self.previous().clone();
            let right: Expr = self.unary()?;

            return Ok(Expr::unary(operator, right));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_types(&[TokenType::False]) {
            return Ok(Expr::literal(Literal::Boolean(false)));
        }

        if self.match_types(&[TokenType::True]) {
            return Ok(Expr::literal(Literal::Boolean(true)));
        }

        if self.match_types(&[TokenType::Nil]) {
            return Ok(Expr::literal(Literal::Nil));
        }

        if self.match_types(&[TokenType::Number, TokenType::String]) {
            return Ok(Expr::literal(self.previous().clone().literal.unwrap()))
        }

        if self.match_types(&[TokenType::LeftParen]) {
            let expr: Expr = self.expression()?;
            let _ = self.consume(TokenType::RightParen, "Expect ')' after expression.");

            return Ok(Expr::grouping(expr));
        }
        
        Err(error(self.peek(), "Expect expression."))
    }

    fn expression_statement(&mut self) -> Result<Expr, ParseError> {
        let expr = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        expr
    }

    fn print_statement(&mut self) -> Result<Expr, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Expr::print(expr))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class | TokenType::Fun | TokenType::Var | TokenType::For | TokenType::While | TokenType::If | TokenType::Print | TokenType::Return => {
                    return;
                },

                _ => self.advance()
            };
        }
    }

    fn match_types(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if !self.check(&token_type) {
                continue;
            }

            self.advance();
            return true;
        }

        false
    }

    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek() == token_type
    }

    fn advance(&mut self) -> &Token {
        self.cur_idx += 1;

        self.tokens.get(self.cur_idx - 1)
            .unwrap_or(self.tokens.last().unwrap())
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, ParseError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }

        Err(error(self.peek(), message))
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.cur_idx).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.cur_idx - 1).unwrap()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::EOF
    }
    
}

fn error(token: &Token, message: &str) -> ParseError {
    lox::error(token, message);
    ParseError { line: token.line, message: message.to_string() }
}