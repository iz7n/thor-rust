use std::collections::HashMap;

use crate::interpreter::Value;

pub struct Scope<'a> {
    name: String,
    pub values: HashMap<String, Value>,
    parent: Option<&'a Scope<'a>>,
}

impl<'a> Scope<'a> {
    pub fn new(name: String, parent: Option<&'a Scope<'a>>) -> Self {
        Self {
            name,
            values: HashMap::new(),
            parent,
        }
    }

    pub fn get(&self, name: &str) -> &Value {
        match self.values.get(name) {
            Some(value) => value,
            None => match self.parent {
                Some(parent) => parent.get(name),
                None => panic!("{} is not defined", name),
            },
        }
    }

    pub fn set(&mut self, name: String, value: Value) -> Value {
        self.values.insert(name, value.clone());
        value
    }
}
