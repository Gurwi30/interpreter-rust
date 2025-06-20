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
            
            match token.token_type { 
                TokenType::True => println!("true"),
                TokenType::False => println!("false"),
                TokenType::Nil => println!("nil"),
                TokenType::Number => {
                    match &token.literal {
                        Some(literal) => match literal {
                            Literal::Integer(integer) => println!("{}", integer),
                            Literal::Float(float) => println!("{}", float),
                            _ => {}
                        }

                        None => {}
                    }
                }
                _ => {}
            }
        }
    }
    
    fn consume(&mut self) -> &Token {
        self.cur_idx += 1;
        
        self.tokens.get(self.cur_idx - 1)
            .unwrap_or(self.tokens.last().unwrap())
    }
    
}