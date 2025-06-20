use crate::tokenizer::{Literal, Token, TokenType};

pub struct Expr {
    
}

pub struct Parser {
    tokens: Vec<Token>,
    cur_idx: usize
}

impl Parser {
    
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            cur_idx: 0
        }
    }

    pub fn parse(&mut self) {
        while self.cur_idx < self.tokens.len() {
            let token = self.consume();
            
            match token.literal { 
                Some(Literal::Boolean(s)) => println!("{}", s),
                _ => { }
            }
        }
    }
    
    fn consume(&mut self) -> &Token {
        self.cur_idx += 1;
        
        self.tokens.get(self.cur_idx - 1)
            .unwrap_or(self.tokens.last().unwrap())
    }
    
}