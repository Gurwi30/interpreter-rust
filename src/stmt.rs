use crate::expr::Expr;
use std::fmt;
use std::fmt::Formatter;
use crate::tokenizer::Token;

pub enum Statement {
    Expression {
        expr: Expr,
    },

    Variable {
        name: Token,
        initializer: Expr
    },

    Print {
        expr: Expr,
    }
}

impl Statement {
    pub fn expression(expr: Expr) -> Statement {
        Statement::Expression { expr }
    }

    pub fn variable(name: Token, initializer: Expr) -> Statement {
        Statement::Variable { name, initializer }
    }
    
    pub fn print(expr: Expr) -> Statement {
        Statement::Print { expr }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expression { expr } => write!(f, "{}", expr),
            Statement::Variable { name, initializer } => write!(f, "var {name} {initializer}"),
            Statement::Print { expr } => write!(f, "{}", expr)
        }
    }
}