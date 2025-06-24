use crate::environment::Environment;
use crate::interpreter::{ExecError, Interpreter, Value};
use crate::stmt::Statement;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

type CallResult = Result<Value, ExecError>;

pub trait LoxCallable: Display {

    fn arity(&self) -> usize;

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> CallResult;

}

pub struct LoxFunction {
    declaration: Statement,
    pub(crate) closure: Rc<RefCell<Environment>>
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
        if let Statement::Function { params,  .. } = &self.declaration {
            return params.len()
        }
        
        0
    }

    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> CallResult {
        let mut env = Environment::with_parent(Rc::clone(&self.closure));

        //println!("Calling function with closure env keys: {:?}", self.closure.borrow().values.keys().collect::<Vec<_>>());

        if let Statement::Function { params, body, .. } = &self.declaration {
            for i in 0..params.len() {
                env.define(
                    params.get(i).unwrap().lexeme.clone(),
                    arguments.get(i).unwrap().clone()
                );
            }

            return  match interpreter.execute_block(body, Rc::new(RefCell::new(env))) {
                Err(ExecError::Runtime(e)) => Err(ExecError::Runtime(e)),
                Err(ExecError::Return(e)) => Ok(e),
                _ => Ok(Value::Nil)
            }
            
            // match interpreter.execute_block(body, Rc::new(RefCell::new(env))? { 
            //     ExecError::Runtime(e) => 
            //     _ => {}
            // }
            // 
            // return Ok(interpreter.execute_block(body, Rc::new(RefCell::new(env)))?.unwrap_or(Value::Nil));
            
            // return Ok(Value::Nil)
        }

        panic!("A LoxFunction declaration statement must be a Statement::Function");
    }
}

impl LoxFunction {
    pub fn new(declaration: Statement, closure: Rc<RefCell<Environment>>) -> LoxFunction {
        if let Statement::Function { .. } = &declaration {
            LoxFunction { declaration, closure }
        } else {
            panic!("A LoxFunction declaration statement must be a Statement::Function");
        }
    }
}

pub struct BuiltinFunction<F>
where
    F: Fn(Vec<Value>) -> CallResult + 'static,
{
    name: String,
    arity: usize,
    func: F,
}

impl<F> Display for BuiltinFunction<F>
where
    F: 'static + Fn(Vec<Value>) -> CallResult,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn builtin {}>", self.name)
    }
}

impl<F> LoxCallable for BuiltinFunction<F>
where
    F: Fn(Vec<Value>) -> CallResult + 'static,
{
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(&self, _interpreter: &mut Interpreter, arguments: Vec<Value>) -> CallResult {
        (self.func)(arguments)
    }
}

pub fn create_builtin<F>(name: &str, arity: usize, func: F) -> Rc<dyn LoxCallable>
where
    F: Fn(Vec<Value>) -> CallResult + 'static,
{
    Rc::new(BuiltinFunction {
        name: name.to_string(),
        arity,
        func,
    })
}