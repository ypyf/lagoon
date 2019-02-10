use std::collections::BTreeMap;
use std::cell::RefCell;
use std::rc::Rc;

use scheme::value::Sexp;


type Env = BTreeMap<String, Rc<RefCell<Sexp>>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    env: Rc<RefCell<Env>>,
    up: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new(up: Option<Rc<Environment>>) -> Self {
        Environment {
            env: Rc::new(RefCell::new(Env::new())),
            up,
        }
    }

    pub fn is_global(&self) -> bool {
        self.up.is_none()
    }

    pub fn lookup(&self, name: &str) -> Option<Rc<RefCell<Sexp>>> {
        if let Some(val) = self.env.borrow().get(name) {
            return Some(val.clone());
        }

        let mut current = self.up.clone();
        while let Some(ctx) = current {
            if let Some(val) = ctx.env.borrow().get(name) {
                return Some(val.clone());
            }
            current = ctx.up.clone();
        }
        None
    }

    pub fn insert(&mut self, name: &str, expr: &Sexp) {
        let val = Rc::new(RefCell::new(expr.clone()));
        self.env.borrow_mut().insert(name.to_owned(), val);
    }

    pub fn insert_many(&mut self, keys: &[String], values: &[Sexp]) {
        for (k, v) in keys.iter().zip(values) {
            let val = Rc::new(RefCell::new(v.clone()));
            self.env.borrow_mut().insert(k.to_string(), val);
        }
    }

    pub fn assign(&mut self, name: &str, val: &Sexp) -> bool {
        if let Some(var) = self.lookup(name) {
            *var.borrow_mut() = val.clone();
            true
        } else {
            false
        }
    }
}
