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

    pub fn assign(&mut self, name: Token, value: Value) -> Result<(), ExecError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme, value);
            return Ok(())
        }
        
        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(name, value)
        }
        
        Err(ExecError::Runtime(
            RuntimeError::new(name.clone(), format!("Undefined variable '{}'.", name.lexeme))
        ))
    }

    pub fn get(&self, name: &Token) -> Result<Value, ExecError> {
        if let Some(val) = self.values.get(&name.lexeme) {
            return Ok(val.clone());
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.borrow().get(name);
        }

        Err(ExecError::Runtime(
            RuntimeError::new(name.clone(), format!("Undefined variable '{}'.", name.lexeme))
        ))
    }

    pub fn debug_print(&self, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{}Environment at depth {}: {:?}", indent, depth, self.values.keys());

        if let Some(ref parent) = self.enclosing {
            parent.borrow().debug_print(depth + 1);
        }
    }
    
}