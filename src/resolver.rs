use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::expr::Expr;
use crate::interpreter::Interpreter;
use crate::lox;
use crate::stmt::Statement;
use crate::tokenizer::{Literal, Token};

type Scope = HashMap<String, bool>;
type ResolveResult = Result<(), ResolveError>;

pub enum ResolveError {
    VariableAlreadyDeclared(String),
    Other(String)
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            ResolveError::VariableAlreadyDeclared(name) => write!(f, "Variable already declared {}", name),
            ResolveError::Other(e) => write!(f, "Other error: {}", e),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum FunctionType {
    None,
    Function
}

pub struct Resolver {
    interpreter: Rc<RefCell<Interpreter>>,
    scopes: Vec<Scope>,
    current_function: FunctionType,
}

impl Resolver {
    pub fn new(interpreter: Rc<RefCell<Interpreter>>) -> Self {
        Resolver {
            interpreter,
            scopes: Vec::new(),
            current_function: FunctionType::None
        }
    }

    pub fn resolve(&mut self, statements: &Vec<Statement>) -> ResolveResult {
        for statement in statements {
            self.resolve_stmt(statement)?;
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) -> ResolveResult {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let scope: &mut Scope = self.scopes.last_mut().unwrap();

        if scope.contains_key(name.lexeme.as_str()) {
            lox::error(&name, "Already a variable with this name in this scope.");
            return Err(ResolveError::VariableAlreadyDeclared(name.lexeme.clone()))
        }

        scope.insert(name.lexeme.clone(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        let scope: &mut Scope = self.scopes.last_mut().unwrap();
        scope.insert(name.lexeme.clone(), true);
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        for i in (0..self.scopes.len()).rev() {
            if !self.scopes.get(i).unwrap().contains_key(name.lexeme.as_str()) {
                continue;
            }

            // Use borrow_mut() to access the interpreter
            self.interpreter.borrow_mut().resolve(expr, self.scopes.len() - 1 - i);
            return;
        }
    }

    fn resolve_stmt(&mut self, statement: &Statement) -> ResolveResult {
        match statement {
            Statement::Block { statements } => {
                self.begin_scope();
                self.resolve(statements)?;
                self.end_scope();

                Ok(())
            },

            Statement::Variable { name, initializer } => {
                self.declare(name)?;

                if let Expr::Literal { literal } = initializer {
                    if let Literal::Nil = literal { } else {
                        self.resolve_expr(initializer)?;
                    }
                }

                self.define(name);

                Ok(())
            },

            Statement::Function { name, .. } => {
                self.declare(name)?;
                self.define(name);

                self.resolve_func(statement, FunctionType::Function)?;

                Ok(())
            },

            Statement::Expression { expr } => {
                self.resolve_expr(expr)?;

                Ok(())
            },

            Statement::If { condition, then_branch, else_branch } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;

                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }

                Ok(())
            },

            Statement::Print { expr } => {
                self.resolve_expr(expr)?;
                Ok(())
            },

            Statement::Return { value, keyword } => {
                if self.current_function == FunctionType::None {
                    lox::error(keyword, "Can't return from top-level code.");
                    return Err(ResolveError::Other("Can't return from top-level code.".into()))
                }

                self.resolve_expr(value)?;

                Ok(())
            },

            Statement::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;

                Ok(())
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) -> ResolveResult {
        match expr {
            Expr::Variable { name } => {
                if !self.scopes.is_empty() {
                    if let Some(false) = self.scopes.last().unwrap().get(name.lexeme.as_str()) {
                        lox::error(name, "Can't read local variable in its own initializer.");
                        return Err(ResolveError::Other("Can't read local variable in its own initializer.".into()));
                    }
                }

                self.resolve_local(expr, name);

                Ok(())
            },

            Expr::Assign { name, value } => {
                self.resolve_expr(value)?;
                self.resolve_local(expr, name);

                Ok(())
            },

            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;

                Ok(())
            },

            Expr::Call { callee, arguments, .. } => {
                self.resolve_expr(callee)?;

                for argument in arguments {
                    self.resolve_expr(argument)?;
                }

                Ok(())
            },

            Expr::Grouping { expr } => {
                self.resolve_expr(expr)?;

                Ok(())
            },

            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;

                Ok(())
            },

            Expr::Unary { right, .. } => {
                self.resolve_expr(right)?;

                Ok(())
            },

            _ => Ok(())
        }
    }

    fn resolve_func(&mut self, statement: &Statement, function_type: FunctionType) -> ResolveResult {
        if let Statement::Function { params, body, .. } = statement {
            let enclosing_func = self.current_function.clone();
            self.current_function = function_type;

            self.begin_scope();

            for param in params {
                self.declare(&param)?;
                self.define(&param);
            }

            self.resolve(body)?;
            self.end_scope();

            self.current_function = enclosing_func;

            Ok(())
        } else {
            panic!("resolve_funct requires function statement.");
        }
    }
}