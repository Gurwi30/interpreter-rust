use std::fmt;
use std::fmt::{Display, Formatter};
use crate::expr::Expr;
use crate::tokenizer::{TokenType, Literal, Token};

#[derive(Debug)]
pub struct RuntimeError<'a> {
    pub token: &'a Token,
    pub message: String,
}

impl Display for RuntimeError<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.token.line, self.message)
    }
}

impl std::error::Error for RuntimeError<'_> { }

impl RuntimeError<'_> {
    pub fn new(token: &Token, message: String) -> RuntimeError {
        RuntimeError { token, message }
    }
}

#[derive(Clone, PartialEq)]
pub enum Value {
    String(String),
    Integer(isize),
    Float(f64),
    Boolean(bool),
    Nil
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{s}"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl Value {

    fn from_literal(literal: &Literal) -> Value {
        match literal {
            Literal::Integer(i) => Value::Integer(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::String(s) => Value::String(s.to_string()),
            Literal::Nil => Value::Nil
        }
    }

}

pub fn run(statements: &Vec<Expr>) -> Result<(), RuntimeError> {
    for expr in statements {
        eval(&expr)?;
    }
    
    Ok(())
}

pub fn eval(expr: &Expr) -> Result<Value, RuntimeError> {

    match expr {
        Expr::Literal { literal } => {
            Ok(Value::from_literal(literal))
        },

        Expr::Unary { operator, right } => {
            let right = eval(right);
            
            match operator.token_type {
                TokenType::Minus => {
                    match right {
                        Ok(Value::Integer(i)) => Ok(Value::Integer(-i)),
                        Ok(Value::Float(f)) => Ok(Value::Float(-f)),
                        _ => Err(RuntimeError::new(operator, "Operand must be a number".to_string()))
                    }
                },

                TokenType::Bang => {
                    Ok(Value::Boolean(!is_truthy(right?)))
                }

                _ => Err(RuntimeError::new(operator, "Unary operator not supported".to_string()))
            }
        },

        Expr::Binary { left, operator, right } => {
            let left = eval(left)?;
            let right = eval(right)?;

            match (&left, &right, &operator.token_type) {
                (Value::Float(l), Value::Float(r), TokenType::Plus) => {
                    Ok(Value::Float(l + r))
                },

                (Value::Float(l), Value::Float(r), TokenType::Minus) => {
                    Ok(Value::Float(l - r))
                },

                (Value::Float(l), Value::Float(r), TokenType::Star) => {
                    Ok(Value::Float(l * r))
                },

                (Value::Float(l), Value::Float(r), TokenType::Slash) => {
                    Ok(Value::Float(l / r))
                },

                (Value::String(l), Value::String(r), TokenType::Plus) => {
                    Ok(Value::String(format!("{}{}", l, r)))
                },

                (Value::Float(l), Value::Float(r), TokenType::Greater) => {
                    Ok(Value::Boolean(l > r))
                },

                (Value::Float(l), Value::Float(r), TokenType::GreaterEqual) => {
                    Ok(Value::Boolean(l >= r))
                },

                (Value::Float(l), Value::Float(r), TokenType::Less) => {
                    Ok(Value::Boolean(l < r))
                },

                (Value::Float(l), Value::Float(r), TokenType::LessEqual) => {
                    Ok(Value::Boolean(l <= r))
                },

                (_, _, TokenType::EqualEqual) => {
                    Ok(Value::Boolean(is_equal(&left, &right)))
                },
                
                (_, _, TokenType::BangEqual) => {
                    Ok(Value::Boolean(!is_equal(&left, &right)))
                }

                _ => Err(RuntimeError::new(operator, "Binary operator not supported".to_string())),
            }
        },

        Expr::Grouping { expr } => {
            eval(expr)
        },

        Expr::Print { expr } => {
            let value = eval(expr)?;
            println!("{}", value);
            Ok(Value::Nil)
        }
    }

}

fn is_truthy(value: Value) -> bool {
    match value {
        Value::Boolean(b) => b,
        Value::Nil => false,
        _ => true,
    }
}

fn is_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Nil, Value::Nil) => true,
        (Value::Nil, _) | (_, Value::Nil) => false,
        _ => a == b,
    }
}

