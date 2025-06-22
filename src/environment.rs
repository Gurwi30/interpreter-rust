use std::collections::HashMap;
use crate::interpreter::{RuntimeError, Value};
use crate::tokenizer::Token;

pub struct Environment {
    values: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            values: HashMap::new()
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn get<'a>(&'a self, name: &'a Token) -> Result<&'a Value, RuntimeError> {
        match self.values.get(name.lexeme.as_str()) {
            Some(val) => Ok(val),
            None => Err(RuntimeError::new(name.clone(), format!("Undefined variable '{}'.", name.lexeme)))
        }
    }
    
    
}