use crate::expr::Expr;
use std::fmt;
use std::fmt::Formatter;
use crate::tokenizer::Token;

#[derive(Debug, Clone)]
pub enum Statement {
    Expression {
        expr: Expr,
    },

    Variable {
        name: Token,
        initializer: Expr
    },

    Block {
        statements: Vec<Statement>
    },

    If {
        condition: Expr,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>
    },

    While {
        condition: Expr,
        body: Box<Statement>
    },

    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Statement>
    },

    Return {
        keyword: Token,
        value: Expr
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

    pub fn block(statements: Vec<Statement>) -> Statement {
        Statement::Block { statements }
    }

    pub fn r#if(condition: Expr, then_branch: Statement, else_branch: Option<Statement>) -> Statement {
        Statement::If { condition, then_branch: Box::new(then_branch), else_branch: else_branch.map(Box::new) }
    }

    pub fn r#while(condition: Expr, body: Statement) -> Statement {
        Statement::While { condition, body: Box::new(body) }
    }
    
    pub fn function(name: Token, params: Vec<Token>, body: Vec<Statement>) -> Statement {
        Statement::Function { name, params, body }
    }

    pub fn r#return(keyword: Token, value: Expr) -> Statement {
        Statement::Return { keyword, value }
    }
    
    pub fn print(expr: Expr) -> Statement {
        Statement::Print { expr }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Expression { expr } => write!(f, "{}", expr),
            Statement::Variable { name, initializer } => write!(f, "var {name} = {initializer}"),
            Statement::Print { expr } => write!(f, "{}", expr),
            Statement::While { condition, body } => write!(f, "while ({}) {}", condition, body),
            Statement::Function { name, params: _params, body: _body } => write!(f, "function {name}"),
            Statement::Return { keyword, value } => write!(f, "ret {} {}", keyword, value),

            Statement::Block { statements } => {
                write!(f, "{{ ")?;

                for stmt in statements {
                    write!(f, "{} ", stmt)?;
                }

                write!(f, "}}")
            },

            Statement::If { condition, then_branch, else_branch } => {
                write!(f, "if ({}) {}",
                       condition,
                       then_branch
                )?;

                if let Some(else_stmt) = else_branch {
                    write!(f, " else {}", else_stmt)?;
                }

                Ok(())
            },
        }
    }
}