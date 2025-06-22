use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::environment::Environment;
use crate::interpreter::{Interpreter, RuntimeError, Value};
use crate::stmt::Statement;

pub trait LoxCallable: Display {

    fn arity(&self) -> usize;

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RuntimeError>;

}

pub struct LoxFunction {
    declaration: Statement,
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.declaration { 
            Statement::Function { name, .. } => write!(f, "{}", name.lexeme),
            _ => write!(f, "<function>")
        }
    }
}

impl LoxCallable for LoxFunction {
    fn arity(&self) -> usize {
        if let Statement::Function { params, body, .. } = &self.declaration {
            return params.len()
        }
        
        0
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        let mut env = Environment::with_parent(Rc::new(RefCell::new(interpreter.globals.clone())));

        if let Statement::Function { params, body, .. } = &self.declaration {
            for i in 0..params.len() {
                env.define(
                    params.get(i).unwrap().lexeme.clone(),
                    arguments.get(i).unwrap().clone()
                );
            }

            interpreter.execute_block(body, Rc::new(RefCell::new(env)))?;
            return Ok(Value::Nil);
        }

        panic!("A LoxFunction declaration statement must be a Statement::Function");
    }
}

impl LoxFunction {
    pub fn new(declaration: Statement) -> LoxFunction {
        if let Statement::Function { .. } = &declaration {
            LoxFunction { declaration }
        } else {
            panic!("A LoxFunction declaration statement must be a Statement::Function");
        }
    }
}

pub struct BuiltinFunction<F>
where
    F: Fn(Vec<Value>) -> Result<Value, RuntimeError> + 'static,
{
    name: String,
    arity: usize,
    func: F,
}

impl<F> Display for BuiltinFunction<F>
where
    F: 'static + Fn(Vec<Value>) -> Result<Value, RuntimeError>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn builtin {}>", self.name)
    }
}

impl<F> LoxCallable for BuiltinFunction<F>
where
    F: Fn(Vec<Value>) -> Result<Value, RuntimeError> + 'static,
{
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, _interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value, RuntimeError> {
        (self.func)(arguments)
    }
}

pub fn create_builtin<F>(name: &str, arity: usize, func: F) -> Rc<dyn LoxCallable>
where
    F: Fn(Vec<Value>) -> Result<Value, RuntimeError> + 'static,
{
    Rc::new(BuiltinFunction {
        name: name.to_string(),
        arity,
        func,
    })
}