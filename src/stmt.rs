use crate::expr::Expr;
use std::fmt;
use std::fmt::Formatter;

pub enum Statement {
    Expression {
        expr: Expr,
    },

    Print {
        expr: Expr,
    }
}

impl Statement {
    pub fn expression(expr: Expr) -> Statement {
        Statement::Expression { expr }
    }

    pub fn print(expr: Expr) -> Statement {
        Statement::Print { expr }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expression { expr } => write!(f, "{}", expr),
            Statement::Print { expr } => write!(f, "{}", expr),
        }
    }
}