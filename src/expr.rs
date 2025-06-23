use crate::tokenizer::{Literal, Token};
use std::fmt::{Display, Formatter};

#[derive(PartialEq, Eq, Clone, Hash)]
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

    Variable {
        name: Token
    },

    Assign {
        name: Token,
        value: Box<Expr>
    },

    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>
    },

    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>
    }

}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal { literal } => write!(f, "{}", literal),
            Expr::Unary { operator, right } => write!(f, "({} {})", operator.lexeme, right),
            Expr::Binary { left, operator, right } => write!(f, "({} {} {})", operator.lexeme, left, right),
            Expr::Grouping { expr } => write!(f, "(group {})", expr),
            Expr::Variable { name } => write!(f, "var {}", name),
            Expr::Assign { name, value } => write!(f, "{} = {}", name, value),
            Expr::Logical { left, operator, right } => write!(f, "({} {} {})", operator.lexeme, left, right),
            Expr::Call { callee, paren, arguments: _arguments } => write!(f, "{}({})", callee, paren),
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

    pub fn variable(name: Token) -> Expr {
        Expr::Variable { name }
    }

    pub fn assign(name: Token, value: Expr) -> Expr {
        Expr::Assign { name, value: Box::new(value) }
    }

    pub fn logical(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Logical { left: Box::new(left), operator, right: Box::new(right) }
    }
    
    pub fn call(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Expr {
        Expr::Call { callee: Box::new(callee), paren, arguments }
    }
    
}