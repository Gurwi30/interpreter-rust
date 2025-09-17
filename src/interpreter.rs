use crate::environment::Environment;
use crate::expr::Expr;
use crate::function::{self, LoxCallable, LoxFunction};
use crate::stmt::Statement;
use crate::tokenizer::{Literal, Token, TokenType};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

type ExecResult = Result<(), ExecError>;

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

impl std::error::Error for RuntimeError {}

impl RuntimeError {
    pub fn new(token: Token, message: String) -> RuntimeError {
        RuntimeError { token, message }
    }
}

pub struct Return {
    value: Value
}

impl Debug for Return {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "return {}", self.value)
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "return {}", self.value)
    }
}

impl std::error::Error for Return {}

#[derive(Debug)]
pub enum ExecError {
    Return(Value),
    Runtime(RuntimeError),
}

impl Display for ExecError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            ExecError::Return(v) => write!(f, "return {}", v),
            ExecError::Runtime(e) => write!(f, "{e}")
        }
    }
}

#[derive(Clone)]
pub enum Value {
    String(String),
    Integer(isize),
    Float(f64),
    Boolean(bool),
    Callable(Rc<dyn LoxCallable>),
    Nil,
}


impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "String({:?})", s),
            Value::Integer(i) => write!(f, "Integer({})", i),
            Value::Float(fl) => write!(f, "Float({})", fl),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Callable(_) => write!(f, "Callable(<fn>)"),
            Value::Nil => write!(f, "Nil"),
        }
    }
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
            Value::Callable(c) => write!(f, "<fn {c}>"),
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
            Literal::Nil => Value::Nil,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
    pub environment: Rc<RefCell<Environment>>,
    pub locals: HashMap<Expr, usize>
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

        globals.define(
            "print".to_string(),
            Value::Callable(function::create_builtin("print", 1, | args | {
                let value = args.get(0).unwrap();
                println!("{}", value);

                Ok(Value::Nil)
            }))
        );

        let globals_rc = Rc::new(RefCell::new(globals));

        Interpreter {
            globals: Rc::clone(&globals_rc),
            environment: globals_rc,
            locals: HashMap::new()
        }
    }

    pub fn run(&mut self, statements: &[Statement]) -> ExecResult {
        for statement in statements {
            self.run_single(statement)?;
        }
        Ok(())
    }

    pub fn resolve(&mut self, expr: Expr, depth: usize) {
        self.locals.insert(expr, depth);
    }

    fn run_single(&mut self, statement: &Statement) -> ExecResult {
        match statement {
            Statement::Expression { expr } => {
                self.eval(expr)?;
                Ok(())
            },

            Statement::Variable { name, initializer } => {
                let value = self.eval(initializer)?;
                self.environment.borrow_mut().define(name.lexeme.clone(), value);
                Ok(())
            },

            Statement::Block { statements } => {
                let new_env = Rc::new(RefCell::new(Environment::with_parent(Rc::clone(&self.environment))));
                self.execute_block(statements, new_env)?;
                Ok(())
            },

            Statement::If { condition, then_branch, else_branch } => {
                if is_truthy(&self.eval(condition)?) {
                    self.run_single(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.run_single(else_branch)
                } else {
                    Ok(())
                }
            }

            Statement::While { condition, body } => {
                while is_truthy(&self.eval(condition)?) {
                    self.run_single(body)?;
                }

                Ok(())
            }

            Statement::Function { name, params, body } => {
                let func = LoxFunction::new(Statement::function(
                    name.clone(),
                    params.clone(),
                    body.clone(),
                ), self.environment.clone());
                
                self.environment
                    .borrow_mut()
                    .define(name.lexeme.clone(), Value::Callable(Rc::new(func)));

                Ok(())
            }

            Statement::Return { value, .. } => {
                let result = self.eval(value)?;
                Err(ExecError::Return(result))
            }

            Statement::Print { expr } => {
                let val = self.eval(expr)?;
                println!("{val}");
                Ok(())
            }
        }
    }

    pub fn execute_block(&mut self, statements: &[Statement], new_env: Rc<RefCell<Environment>>) -> ExecResult {
        let previous = Rc::clone(&self.environment);
    
        self.environment = Rc::clone(&new_env);
    
        let result = (|| {
            for statement in statements {
                self.run_single(statement)?;
            }
            Ok(())
        })();
    
        self.environment = previous;
    
        result
    }

    pub fn eval(&mut self, expr: &Expr) -> Result<Value, ExecError> {
        match expr {
            Expr::Literal { literal } => Ok(Value::from_literal(literal)),

            Expr::Unary { operator, right } => {
                let right = self.eval(right)?;

                match operator.token_type {
                    TokenType::Minus => match right {
                        Value::Integer(i) => Ok(Value::Integer(-i)),
                        Value::Float(f) => Ok(Value::Float(-f)),
                        _ => Err(ExecError::Runtime(RuntimeError::new(
                            operator.clone(),
                            "Operand must be a number".to_string(),
                        ))),
                    },

                    TokenType::Bang => Ok(Value::Boolean(!is_truthy(&right))),

                    _ => Err(ExecError::Runtime(RuntimeError::new(
                        operator.clone(),
                        "Unary operator not supported".to_string(),
                    ))),
                }
            }

            Expr::Binary { left, operator, right } => {
                let left = self.eval(left)?;
                let right = self.eval(right)?;
                
                match (&left, &right, &operator.token_type) {
                    (Value::Float(l), Value::Float(r), TokenType::Plus) => Ok(Value::Float(l + r)),
                    (Value::Float(l), Value::Float(r), TokenType::Minus) => Ok(Value::Float(l - r)),
                    (Value::Float(l), Value::Float(r), TokenType::Star) => Ok(Value::Float(l * r)),
                    (Value::Float(l), Value::Float(r), TokenType::Slash) => Ok(Value::Float(l / r)),

                    (Value::String(l), Value::String(r), TokenType::Plus) => Ok(Value::String(format!("{}{}", l, r))),

                    (Value::Float(l), Value::Float(r), TokenType::Greater) => Ok(Value::Boolean(l > r)),
                    (Value::Float(l), Value::Float(r), TokenType::GreaterEqual) => Ok(Value::Boolean(l >= r)),

                    (Value::Float(l), Value::Float(r), TokenType::Less) => Ok(Value::Boolean(l < r)),
                    (Value::Float(l), Value::Float(r), TokenType::LessEqual) => Ok(Value::Boolean(l <= r)),

                    (_, _, TokenType::EqualEqual) => Ok(Value::Boolean(is_equal(&left, &right))),
                    (_, _, TokenType::BangEqual) => Ok(Value::Boolean(!is_equal(&left, &right))),

                    _ => Err(ExecError::Runtime(
                        RuntimeError::new(
                            operator.clone(),
                            "Binary operator not supported".to_string(),
                        )
                    )),
                }
            }

            Expr::Grouping { expr } => self.eval(expr),

            Expr::Variable { name } => {
                self.look_up_var(name, expr)
            },

            Expr::Assign { name, value } => {
                let res = self.eval(value)?;

                match self.locals.get(expr) {
                    Some(distance) => {
                        Environment::assign_at(self.environment.clone(), *distance, name, &res);
                    }
                    None => self.globals.borrow_mut().assign(name.clone(), res.clone())?,
                }

                Ok(res)
             }

            Expr::Logical { left, operator, right } => {
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
            }

            Expr::Call { callee, paren, arguments, } => {
                let value = self.eval(callee)?;
                let mut args: Vec<Value> = Vec::new();

                for arg in arguments {
                    args.push(self.eval(arg)?);
                }

                if let Value::Callable(callable) = value {
                    if args.len() != callable.arity() {
                        return Err(ExecError::Runtime(
                            RuntimeError::new(
                                paren.clone(),
                                format!(
                                    "Expected {} arguments but got {}.",
                                    callable.arity(),
                                    args.len()
                                ),
                            )
                        ));
                    }

                    callable.call(self, args)
                } else {
                    Err(ExecError::Runtime(
                        RuntimeError::new(
                            paren.clone(),
                            "Can only call functions and classes.".to_string(),
                        )
                    ))
                }
            }
        }
    }

    fn look_up_var(&self, name: &Token, expr: &Expr) -> Result<Value, ExecError> {
        match self.locals.get(expr) {
            Some(distance) => match Environment::get_at(self.environment.clone(), *distance, name.lexeme.as_str()) {
                Some(val) => Ok(val),
                None => {
                    println!("Env");
                    println!("Current environment {:p}", Rc::as_ptr(&self.environment));
                    println!("Dept {}, Lexeme {}", distance, name.lexeme);
                    self.environment.borrow().debug_print(0);
                    
                    println!("Ancestor");
                    
                    Environment::ancestor(Rc::clone(&self.environment), *distance).borrow().debug_print(*distance);
                    
                    println!("Search {:p}", name.lexeme.as_str());
                    
                    if let Some(v) = Environment::get_at(self.environment.clone(), *distance, "hello") {
                        println!("Found {}", v);
                    } 
                    
                    Err(ExecError::Runtime(
                        RuntimeError::new(name.clone(), format!("Undefined variable '{}'", name.lexeme)), )
                    )
                },
            },
            None => self.globals.borrow().get(name)
        }
    }
    
    fn env_depth(env: Rc<RefCell<Environment>>) -> usize {
        let mut depth = 0;
        let mut current = Some(env);

        while let Some(env) = current {
            let env_ref = env.borrow();
            current = env_ref.enclosing.clone();
            depth += 1;
        }

        depth
    }
}

fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Boolean(b) => *b,
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
