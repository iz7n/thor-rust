use std::fmt;

use crate::Node;

#[derive(Clone)]
pub enum Value {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Array(Vec<Value>),
    Function(Vec<String>, Node),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            Int(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}", value),
            Bool(value) => write!(f, "{}", value),
            Str(value) => write!(f, "\"{}\"", value),
            Char(value) => write!(f, "'{}'", value),
            Array(value) => write!(
                f,
                "[{}]",
                value
                    .iter()
                    .map(|node| format!("{}", node))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Function(_, _) => write!(f, "<fn>"),
        }
    }
}
