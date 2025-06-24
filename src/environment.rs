use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::interpreter::{ExecError, RuntimeError, Value};
use crate::tokenizer::Token;

#[derive(Clone)]
#[derive(PartialEq)]
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: None,
        }
    }

    pub fn with_parent(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            values: HashMap::new(),
            enclosing: Some(enclosing),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get_at(env: Rc<RefCell<Environment>>, distance: usize, name: &str) -> Option<Value> {
        Environment::ancestor(env, distance)
            .borrow()
            .values
            .get(name)
            .cloned()
    }

    pub fn assign_at(env: Rc<RefCell<Environment>>, distance: usize, name: &Token, value: &Value) {
        Environment::ancestor(env, distance)
            .borrow_mut()
            .values
            .insert(name.lexeme.clone(), value.clone());
    }

    pub fn ancestor(env: Rc<RefCell<Environment>>, distance: usize) -> Rc<RefCell<Environment>> {
        let mut current_env = env;
        for _ in 0..distance {
            let next = {
                let borrow = current_env.borrow();
                borrow.enclosing.clone().expect("No enclosing environment")
            };
            current_env = next;
        }
        
        current_env
    }

    pub fn assign(&mut self, name: Token, value: Value) -> Result<(), ExecError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value);
            return Ok(());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(name, value);
        }

        Err(ExecError::Runtime(RuntimeError::new(
            name.clone(),
            format!("Undefined variable '{}'.", name.lexeme),
        )))
    }

    pub fn get(&self, name: &Token) -> Result<Value, ExecError> {
        if let Some(val) = self.values.get(&name.lexeme) {
            return Ok(val.clone());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(ExecError::Runtime(RuntimeError::new(
            name.clone(),
            format!("Undefined variable '{}'.", name.lexeme),
        )))
    }

    pub fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{}Environment at depth {}: {:?}", indent, depth, self.values);

        if let Some(parent) = &self.enclosing {
            parent.borrow().debug_print(depth + 1);
        }
    }
    
}
