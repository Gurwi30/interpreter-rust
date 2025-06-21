use crate::expr::Expr;
use crate::tokenizer::{Literal, TokenType};

pub fn eval(expr: &Expr) -> Literal {

    match expr {
        Expr::Literal { literal } => {
            literal.clone()
        },

        Expr::Unary { operator, right } => {
            let right = eval(right);
            
            match operator.token_type {
                TokenType::Minus => {
                    match right {
                        Literal::Integer(i) => Literal::Integer(-i),
                        Literal::Float(f) => Literal::Float(-f),
                        _ => panic!("Invalid operation")
                    }
                },

                TokenType::Bang => {
                    Literal::Boolean(!is_truthy(right))
                }

                _ => panic!("unary operator not supported")
            }
        },

        Expr::Binary { left, operator, right } => {
            let left = eval(left);
            let right = eval(right);

            match (&left, &right, &operator.token_type) {
                (Literal::Float(l), Literal::Float(r), TokenType::Plus) => {
                    Literal::Float(l + r)
                },

                (Literal::Float(l), Literal::Float(r), TokenType::Minus) => {
                    Literal::Float(l - r)
                },

                (Literal::Float(l), Literal::Float(r), TokenType::Star) => {
                    Literal::Float(l * r)
                },

                (Literal::Float(l), Literal::Float(r), TokenType::Slash) => {
                    Literal::Float(l / r)
                },

                (Literal::String(l), Literal::String(r), TokenType::Plus) => {
                    Literal::String(l + &r)
                },

                (Literal::Float(l), Literal::Float(r), TokenType::Greater) => {
                    Literal::Boolean(l > r)
                },

                (Literal::Float(l), Literal::Float(r), TokenType::GreaterEqual) => {
                    Literal::Boolean(l >= r)
                },

                (Literal::Float(l), Literal::Float(r), TokenType::Less) => {
                    Literal::Boolean(l < r)
                },

                (Literal::Float(l), Literal::Float(r), TokenType::LessEqual) => {
                    Literal::Boolean(l <= r)
                },

                (_, _, TokenType::EqualEqual) => {
                    Literal::Boolean(is_equal(&left, &right))
                }
                (_, _, TokenType::BangEqual) => {
                    Literal::Boolean(!is_equal(&left, &right))
                }

                _ => panic!("binary operator not supported for the given operand types"),
            }
        },

        Expr::Grouping { expr } => {
            eval(expr)
        },
    }

}

fn is_truthy(value: Literal) -> bool {
    match value {
        Literal::Boolean(b) => b,
        Literal::Nil => false,
        _ => true,
    }
}

fn is_equal(a: &Literal, b: &Literal) -> bool {
    match (a, b) {
        (Literal::Nil, Literal::Nil) => true,
        (Literal::Nil, _) | (_, Literal::Nil) => false,
        _ => a == b,
    }
}