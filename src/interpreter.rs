use crate::environment::Environment;
use crate::expr::Expr;
use crate::stmt::Statement;
use crate::tokenizer::{Literal, Token, TokenType};
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Debug)]
#[derive(Clone)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.token.line, self.message)
    }
}

impl std::error::Error for RuntimeError { }

impl RuntimeError {
    pub fn new(token: Token, message: String) -> RuntimeError {
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

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new())),
        }
    }

    pub fn run(&mut self, statements: &Vec<Statement>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.run_single(statement)?;
        }

        Ok(())
    }

    fn run_single(&mut self, statement: &Statement) -> Result<(), RuntimeError> {
        match statement {
            Statement::Expression { expr } => {
                self.eval(expr)?;
            },

            Statement::Variable { name, initializer } => {
                let value = self.eval(initializer)?;
                self.environment.borrow_mut().define(name.lexeme.clone(), value);
            },

            Statement::Block { statements } => {
                let previous = self.environment.clone();
                self.environment = Rc::new(RefCell::new(Environment::with_parent(previous.clone())));

                let result = self.run(statements);

                self.environment = previous;

                result?;
            },

            Statement::If { condition, then_branch, else_branch } => {
                if is_truthy(self.eval(&condition)?) {
                    self.run_single(&then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.run_single(&else_branch)?;
                }
            }

            Statement::Print { expr } => {
                let val = self.eval(expr)?;
                println!("{val}");
            }
        };

        Ok(())
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {

        match expr {
            Expr::Literal { literal } => {
                Ok(Value::from_literal(literal))
            },

            Expr::Unary { operator, right } => {
                let right = self.eval(right);

                match operator.token_type {
                    TokenType::Minus => {
                        match right {
                            Ok(Value::Integer(i)) => Ok(Value::Integer(-i)),
                            Ok(Value::Float(f)) => Ok(Value::Float(-f)),
                            _ => Err(RuntimeError::new(operator.clone(), "Operand must be a number".to_string()))
                        }
                    },

                    TokenType::Bang => {
                        Ok(Value::Boolean(!is_truthy(right?)))
                    }

                    _ => Err(RuntimeError::new(operator.clone(), "Unary operator not supported".to_string()))
                }
            },

            Expr::Binary { left, operator, right } => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;

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

                    _ => Err(RuntimeError::new(operator.clone(), "Binary operator not supported".to_string())),
                }
            },

            Expr::Grouping { expr } => {
                self.eval(expr)
            },

            Expr::Variable { name } => {
                let env = &self.environment;

                env.borrow().get(name)
                    .map(|val| val.clone())
            },

            Expr::Assign { name, value } => {
                let value = self.eval(value)?;
                self.environment.borrow_mut().assign(name.clone(), value.clone())?;
                Ok(value)
            },

            Expr::Logical { left, operator , right } => {
                let left = self.eval(left)?;

                if !is_truthy(left.clone()) {
                    return Ok(left);
                }

                self.eval(right)
            }

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

