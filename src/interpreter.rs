use std::fmt::{Display, Formatter};
use crate::expr::Expr;
use crate::tokenizer::{TokenType, Literal};

#[derive(Clone, PartialEq)]
pub enum Value {
    String(String),
    Integer(isize),
    Float(f64),
    Boolean(bool),
    Nil
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

pub fn eval(expr: &Expr) -> Value {

    match expr {
        Expr::Literal { literal } => {
            Value::from_literal(literal)
        },

        Expr::Unary { operator, right } => {
            let right = eval(right);
            
            match operator.token_type {
                TokenType::Minus => {
                    match right {
                        Value::Integer(i) => Value::Integer(-i),
                        Value::Float(f) => Value::Float(-f),
                        _ => panic!("Invalid operation")
                    }
                },

                TokenType::Bang => {
                    Value::Boolean(!is_truthy(right))
                }

                _ => panic!("unary operator not supported")
            }
        },

        Expr::Binary { left, operator, right } => {
            let left = eval(left);
            let right = eval(right);

            match (&left, &right, &operator.token_type) {
                (Value::Float(l), Value::Float(r), TokenType::Plus) => {
                    Value::Float(l + r)
                },

                (Value::Float(l), Value::Float(r), TokenType::Minus) => {
                    Value::Float(l - r)
                },

                (Value::Float(l), Value::Float(r), TokenType::Star) => {
                    Value::Float(l * r)
                },

                (Value::Float(l), Value::Float(r), TokenType::Slash) => {
                    Value::Float(l / r)
                },

                (Value::String(l), Value::String(r), TokenType::Plus) => {
                    Value::String(format!("{}{}", l, r))
                },

                (Value::Float(l), Value::Float(r), TokenType::Greater) => {
                    Value::Boolean(l > r)
                },

                (Value::Float(l), Value::Float(r), TokenType::GreaterEqual) => {
                    Value::Boolean(l >= r)
                },

                (Value::Float(l), Value::Float(r), TokenType::Less) => {
                    Value::Boolean(l < r)
                },

                (Value::Float(l), Value::Float(r), TokenType::LessEqual) => {
                    Value::Boolean(l <= r)
                },

                (_, _, TokenType::EqualEqual) => {
                    Value::Boolean(is_equal(&left, &right))
                },
                
                (_, _, TokenType::BangEqual) => {
                    Value::Boolean(!is_equal(&left, &right))
                }

                _ => panic!("binary operator not supported for the given operand types"),
            }
        },

        Expr::Grouping { expr } => {
            eval(expr)
        },
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