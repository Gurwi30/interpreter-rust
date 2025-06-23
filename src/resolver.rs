use std::collections::HashMap;
use crate::expr::Expr;
use crate::interpreter::Interpreter;
use crate::lox;
use crate::stmt::Statement;
use crate::tokenizer::{Literal, Token};

type Scope = HashMap<String, bool>;

#[derive(Clone, PartialEq)]
pub enum FunctionType {
    None,
    Function
}

pub struct  Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<Scope>,
    current_function: FunctionType,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Resolver { interpreter, scopes: Vec::new(), current_function: FunctionType::None }
    }

    pub fn resolve(&mut self, statements: &Vec<Statement>) {
         for statement in statements {
             self.resolve_stmt(statement);
         }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if self.scopes.is_empty() {
            return;
        }

        let scope: &mut Scope = self.scopes.last_mut().unwrap();

        if scope.contains_key(name.lexeme.as_str()) {
            lox::error(&name, "Already a variable with this name in this scope.")
        }

        scope.insert(name.lexeme.clone(), false);
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

            self.interpreter.resolve(expr, &self.scopes.len() - 1 - i);
            return;
        }
    }

    fn resolve_stmt(&mut self, statement: &Statement) {
        match statement {
            Statement::Block { statements } => {
                self.begin_scope();
                self.resolve(statements);
                self.end_scope();
            },

            Statement::Variable { name, initializer } => {
                if let Expr::Literal { literal } = initializer {
                    if let Literal::Nil = literal {

                    } else {
                        self.resolve_expr(initializer);
                    }

                    self.define(name);
                }
            },

            Statement::Function { name, .. } => {
                self.declare(name);
                self.define(name);

                self.resolve_func(statement, FunctionType::Function);
            },

            Statement::Expression { expr } => {
                self.resolve_expr(expr);
            },

            Statement::If { condition, then_branch, else_branch } => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_branch);

                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch);
                }
            },

            Statement::Print { expr } => {
                self.resolve_expr(expr);
            },

            Statement::Return { value, keyword } => {
                if self.current_function == FunctionType::None {
                    lox::error(keyword, "Can't return from top-level code.");
                }

                self.resolve_expr(value);
            },

            Statement::While { condition, body } => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }

            _ => {}
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Variable { name } => {
                if !self.scopes.is_empty() && self.scopes.last().unwrap().get(name.lexeme.as_str()).map_or(false, |v| *v) {
                    lox::error(name, "Can't read local variable in its own initializer.");
                }

                self.resolve_local(expr, name);
            },

            Expr::Assign { name, value } => {
                self.resolve_expr(value);
                self.resolve_local(expr, name);
            },

            Expr::Binary { left, right, operator } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            },

            Expr::Call { callee, arguments, .. } => {
                self.resolve_expr(callee);

                for argument in arguments {
                    self.resolve_expr(argument);
                }
            },

            Expr::Grouping { expr } => {
                self.resolve_expr(expr);
            },

            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            },

            Expr::Unary { right, .. } => {
                self.resolve_expr(right);
            },

            _ => {}
        }
    }

    fn resolve_func(&mut self, statement: &Statement, function_type: FunctionType) {
        if let Statement::Function { name, params, body } = statement {
            let enclosing_func = self.current_function.clone();
            self.current_function = function_type;

            self.begin_scope();

            for param in params {
                self.declare(&param);
                self.define(&param);
            }

            self.resolve(body);
            self.end_scope();

            self.current_function = enclosing_func;

        } else {
            panic!("resolve_funct requires function statement.");
        }
    }
}