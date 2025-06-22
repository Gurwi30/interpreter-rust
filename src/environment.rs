use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::interpreter::{RuntimeError, Value};
use crate::tokenizer::Token;

#[derive(Clone)]
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

    pub fn assign(&mut self, name: Token, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, value);
            return Ok(())
        }
        
        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(name, value)
        }
        
        Err(RuntimeError::new(name.clone(), format!("Undefined variable '{}'.", name.lexeme)))
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        if let Some(val) = self.values.get(&name.lexeme) {
            return Ok(val.clone());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(RuntimeError::new(name.clone(), format!("Undefined variable '{}'.", name.lexeme)))
    }
    
}