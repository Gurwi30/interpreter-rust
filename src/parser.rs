use crate::expr::Expr;
use crate::tokenizer::{Literal, Token, TokenType};
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::Formatter;
use crate::lox;
use crate::stmt::Statement;

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

    pub fn parse(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.is_at_end() {
            statements.push(self.declaration()?)
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> Result<Statement, ParseError> {
        let result = (|| {
            if self.match_types(&[TokenType::Fun]) {
                return self.function("function");
            }

            if self.match_types(&[TokenType::Var]) {
                return self.var_declaration();
            }

            self.statement()
        })();

        match result {
            Ok(stmt) => Ok(stmt),
            Err(e) => {
                self.synchronize();
                Err(e)
            }
        }
    }

    fn var_declaration(&mut self) -> Result<Statement, ParseError> {
        let name: Token = self.consume(TokenType::Identifier, "Expect variable name.")?.clone();
        let mut initializer: Expr = Expr::literal(Literal::Nil);

        if self.match_types(&[TokenType::Equal]) {
            initializer = self.expression()?;
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.")?;
        Ok(Statement::variable(name, initializer))
    }

    fn function(&mut self, kind: &str) -> Result<Statement, ParseError> {
        let name_token = self.consume(TokenType::Identifier, format!("Expect {} name.", kind).as_str())?;
        let name = name_token.clone();

        self.consume(TokenType::LeftParen, format!("Expect '(' after {} name.", kind).as_str())?;

        let mut params: Vec<Token> = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    if let Some(token) = self.peek() {
                        error(token, "Can't have more than 255 parameters.");
                    }
                }

                let param_token = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                params.push(param_token.clone());

                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        self.consume(TokenType::LeftBrace, format!("Expect '{{' before {} body.", kind).as_str())?;

        let body = self.block()?;

        Ok(Statement::function(name, params, body))
    }

    fn statement(&mut self) -> Result<Statement, ParseError> {
        if self.match_types(&[TokenType::For]) {
            return self.for_statement();
        }

        if self.match_types(&[TokenType::If]) {
            return self.if_statement();
        }

        if self.match_types(&[TokenType::LeftBrace]) {
            return Ok(Statement::block(self.block()?))
        }

        if self.match_types(&[TokenType::While]) {
            return self.while_statement();
        }

        if self.match_types(&[TokenType::Return]) {
            return self.return_statement();
        }

        if self.match_types(&[TokenType::Print]) {
            return self.print_statement();
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;

        Ok(Statement::print(expr))
    }

    fn return_statement(&mut self) -> Result<Statement, ParseError> {
        let keyword = self.previous().unwrap().clone();
        let mut value: Expr = Expr::literal(Literal::Nil);

        if !self.check(&TokenType::Semicolon) {
            value = self.expression()?;
        }

        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(Statement::r#return(keyword, value))
    }

    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let then_branch = self.statement()?;
        let mut else_branch: Option<Statement> = None;

        if self.match_types(&[TokenType::Else]) {
            else_branch = Some(self.statement()?);
        }

        Ok(Statement::r#if(condition, then_branch, else_branch))
    }

    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let body = self.statement()?;
        Ok(Statement::r#while(condition, body))
    }

    fn for_statement(&mut self) -> Result<Statement, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_types(&[TokenType::Semicolon]) {
            None
        } else if self.match_types(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(&TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let increment = if !self.check(&TokenType::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;

        if let Some(inc) = increment {
            body = Statement::block(vec![
                body,
                Statement::expression(inc),
            ]);
        }

        let condition = condition.unwrap_or_else(|| Expr::literal(Literal::Boolean(true)));

        body = Statement::r#while(condition, body);

        if let Some(init) = initializer {
            body = Statement::block(vec![init, body]);
        }

        Ok(body)
    }

    fn block(&mut self) -> Result<Vec<Statement>, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        Ok(Statement::expression(expr))
    }

    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.or()?;

        if self.match_types(&[TokenType::Equal]) {
            let equals = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing '=' token.")
            })?.clone();

            let value = self.assignment()?;

            if let Expr::Variable { name } = expr {
                return Ok(Expr::assign(name, value))
            }

            return Err(error(&equals, "Invalid assignment target."))
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;

        while self.match_types(&[TokenType::Or]) {
            let operator = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing 'or' operator.")
            })?.clone();

            let right = self.and()?;

            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.match_types(&[TokenType::And]) {
            let operator = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing 'and' operator.")
            })?.clone();

            let right = self.equality()?;

            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.comparison()?;

        while self.match_types(&[TokenType::BangEqual, TokenType::EqualEqual]) {
            let operator: Token = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing equality operator.")
            })?.clone();

            let right: Expr = self.comparison()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.term()?;

        while self.match_types(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator: Token = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing comparison operator.")
            })?.clone();

            let right: Expr = self.term()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.factor()?;

        while self.match_types(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing term operator.")
            })?.clone();

            let right = self.factor()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr: Expr = self.unary()?;

        while self.match_types(&[TokenType::Slash, TokenType::Star]) {
            let operator: Token = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing factor operator.")
            })?.clone();

            let right: Expr = self.unary()?;

            expr = Expr::binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.match_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator: Token = self.previous().ok_or_else(|| {
                error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Missing unary operator.")
            })?.clone();

            let right: Expr = self.unary()?;

            return Ok(Expr::unary(operator, right));
        }

        self.call()
    }

    pub fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if !self.match_types(&[TokenType::LeftParen]) {
                break;
            }

            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut arguments = Vec::new();

        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    if let Some(token) = self.peek() {
                        error(token, "Can't have more than 255 arguments.");
                    }
                }

                arguments.push(self.expression()?);

                if !self.match_types(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
        Ok(Expr::call(callee, paren.clone(), arguments))
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
            let prev = self.previous().ok_or_else(|| error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Expect literal."))?;
            return Ok(Expr::literal(prev.literal.clone().unwrap()))
        }

        if self.match_types(&[TokenType::Identifier]) {
            let prev = self.previous().ok_or_else(|| error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Expect identifier."))?;
            return Ok(Expr::variable(prev.clone()))
        }

        if self.match_types(&[TokenType::LeftParen]) {
            let expr: Expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;

            return Ok(Expr::grouping(expr));
        }

        Err(error(self.peek().unwrap_or_else(|| &self.tokens[self.tokens.len()-1]), "Expect expression."))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if let Some(prev) = self.previous() {
                if prev.token_type == TokenType::Semicolon {
                    return;
                }
            }

            match self.peek().map(|t| &t.token_type) {
                Some(TokenType::Class) | Some(TokenType::Fun) | Some(TokenType::Var) | Some(TokenType::For) | Some(TokenType::While) | Some(TokenType::If) | Some(TokenType::Print) | Some(TokenType::Return) => {
                    return;
                },

                _ => { self.advance(); }
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

        self.peek().map(|tok| &tok.token_type == token_type).unwrap_or(false)
    }

    fn advance(&mut self) -> &Token {
        self.cur_idx += 1;

        self.tokens.get(self.cur_idx - 1)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, ParseError> {
        if self.check(&token_type) {
            return Ok(self.advance());
        }

        Err(error(self.peek().unwrap_or_else(|| self.tokens.last().unwrap()), message))
    }

    fn previous(&self) -> Option<&Token> {
        if self.cur_idx == 0 {
            None
        } else {
            self.tokens.get(self.cur_idx - 1)
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.cur_idx)
    }

    fn is_at_end(&self) -> bool {
        self.peek().map(|t| t.token_type == TokenType::EOF).unwrap_or(true)
    }

}

fn error(token: &Token, message: &str) -> ParseError {
    lox::error(token, message);
    ParseError { line: token.line, message: message.to_string() }
}
