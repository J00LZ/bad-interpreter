use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::value::Value;

#[derive(Debug, PartialEq)]
pub struct Inner {
    contents: HashMap<String, Value>,
    parent: Option<Env>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env(Rc<RefCell<Inner>>);

impl Env {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(Inner {
            contents: Default::default(),
            parent: None,
        })))
    }

    pub fn child(&self) -> Self {
        Self(Rc::new(RefCell::new(Inner {
            contents: Default::default(),
            parent: Some(self.clone()),
        })))
    }

    pub fn get_value(&self, key: String) -> Option<Value> {
        self.0.borrow().contents.get(&key).cloned()
    }

    pub fn get_value_rec(&self, key: String) -> Option<Value> {
        if let Some(x) = self.get_value(key.clone()) {
            Some(x)
        } else if let Some(e) = &self.0.borrow().parent {
            e.get_value_rec(key)
        } else {
            None
        }
    }

    pub fn set_value(&mut self, key: String, value: Value) {
        self.0.borrow_mut().contents.insert(key, value);
    }
}
