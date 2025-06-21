use crate::tokenizer::{Literal, Token};
use std::fmt::{Display, Formatter};

pub enum Expr {
    Literal {
        literal: Literal,
    },

    Unary {
        operator: Token,
        right: Box<Expr>
    },

    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>
    },

    Grouping {
        expr: Box<Expr>
    },

    Print {
        expr: Box<Expr>
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal { literal } => write!(f, "{}", literal),
            Expr::Unary { operator, right } => write!(f, "({} {})", operator.lexeme, right),
            Expr::Binary { left, operator, right } => write!(f, "({} {} {})", operator.lexeme, left, right),
            Expr::Grouping { expr } => write!(f, "(group {})", expr),
            Expr::Print { expr } => write!(f, "print {}", expr),
        }
    }
}

impl Expr {
    
    pub fn literal(literal: Literal) -> Expr {
        Expr::Literal { literal }
    }
    
    pub fn unary(operator: Token, right: Expr) -> Expr {
        Expr::Unary { operator, right: Box::new(right) }
    }
    
    pub fn binary(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Binary { left: Box::new(left), operator, right: Box::new(right) }
    }
    
    pub fn grouping(expr: Expr) -> Expr {
        Expr::Grouping { expr: Box::new(expr) }
    }

    pub fn print(expr: Expr) -> Expr {
        Expr::Print { expr: Box::new(expr) }
    }
    
}