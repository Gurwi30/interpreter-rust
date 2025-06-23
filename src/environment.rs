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

    pub fn get_at(&self, distance: usize, name: &String) -> Option<Value> {
        Some(self.ancestor(distance).borrow().values.get(name)?.clone())
    }

    pub fn ancestor(&self, distance: usize) -> Rc<RefCell<Environment>> {
        let mut env = Rc::new(RefCell::new(self.clone())); // If self is not Rc already, better approach below

        for _ in 0..distance {
            let enclosing_env = {
                let env_borrow = env.borrow();
                env_borrow.enclosing.as_ref().expect("No enclosing environment").clone()
            };

            env = enclosing_env;
        }
        env
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

    pub fn assign_at(&self, distance: usize, name: &Token, value: &Value) {
        self.ancestor(distance).borrow_mut().values.insert(name.lexeme.clone(), value.clone());
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