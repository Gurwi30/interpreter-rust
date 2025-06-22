use crate::environment::Environment;
use crate::expr::Expr;
use crate::stmt::Statement;
use crate::tokenizer::{Literal, Token, TokenType};
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use crate::function;
use crate::function::{LoxCallable, LoxFunction};

#[derive(Debug)]
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

#[derive(Clone)]
pub enum Value {
    String(String),
    Integer(isize),
    Float(f64),
    Boolean(bool),
    Callable(Rc<dyn LoxCallable>),
    Nil
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Integer(a), Value::Integer(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{s}"),
            Value::Integer(i) => write!(f, "{i}"),
            Value::Float(fl) => write!(f, "{fl}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Callable(c) => write!(f, "{c}"),
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

#[derive(Clone, PartialEq)]
pub struct Interpreter {
    pub environment: Rc<RefCell<Environment>>,
    pub globals: Environment
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut globals = Environment::new();

        globals.define(
            "clock".to_string(),
            Value::Callable(function::create_builtin("clock", 0, |_| {
                let start = SystemTime::now();
                let since_epoch = start.duration_since(UNIX_EPOCH)
                    .expect("Time went backwards");

                Ok(Value::Float(since_epoch.as_secs() as f64))
            }))
        );

        Interpreter {
            environment: Rc::new(RefCell::new(Environment::new())),
            globals
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
                //
                // let result = self.run(statements);
                //
                // self.environment = previous;
                //
                // result?;
                self.execute_block(statements, self.environment.clone())?;
            },

            Statement::If { condition, then_branch, else_branch } => {
                if is_truthy(&self.eval(&condition)?) {
                    self.run_single(&then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.run_single(&else_branch)?;
                }
            },

            Statement::While { condition, body } => {
                while is_truthy(&self.eval(&condition)?) {
                    self.run_single(&body)?;
                }
            },

            Statement::Print { expr } => {
                let val = self.eval(expr)?;
                println!("{val}");
            },

            Statement::Function { name, params, body } => {
                let func = LoxFunction::new(Statement::function(
                    name.clone(),
                    params.clone(),
                    body.clone()
                ));
                
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), Value::Callable(Rc::new(func)));
            }
        };

        Ok(())
    }

    pub fn execute_block(
        &mut self,
        statements: &Vec<Statement>,
        new_env: Rc<RefCell<Environment>>,
    ) -> Result<(), RuntimeError> {
        let previous = Rc::clone(&self.environment);

        self.environment = Rc::clone(&new_env);

        let result = (|| {
            for statement in statements {
                self.run_single(&statement)?;
            }

            Ok(())
        })();

        self.environment = previous;

        result
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
                        Ok(Value::Boolean(!is_truthy(&right?)))
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

                if operator.token_type == TokenType::Or {
                    if is_truthy(&left) {
                        return Ok(left);
                    }
                } else {
                    if !is_truthy(&left) {
                        return Ok(left);
                    }
                }

                self.eval(right)
            },

            Expr::Call { callee, paren, arguments } => {
                let value = self.eval(callee)?;
                let mut args: Vec<Value> = Vec::new();

                for arg in arguments {
                    args.push(self.eval(arg)?);
                }

                if let Value::Callable(callable) = value {
                    if args.len() != callable.arity() {
                        return Err(RuntimeError::new(paren.clone(), format!("Expected {} arguments but got {}.", callable.arity(), args.len())));
                    }

                    callable.call(self, args)
                } else {
                    Err(RuntimeError::new(paren.clone(), "Can only call functions and classes.".to_string()))
                }

            }

        }

    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Boolean(b) => b.clone(),
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

