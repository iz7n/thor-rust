use crate::{
    interpreter::{Scope, Value},
    BinaryOp, IdentifierOp, Node, UnaryOp,
};

pub struct Interpreter<'a> {
    scope: Scope<'a>,
    return_value: Option<Value>,
}

impl<'a> Interpreter<'a> {
    pub fn new(filename: String) -> Self {
        Self {
            scope: Scope::new(filename, None),
            return_value: None,
        }
    }

    fn create_child(&'a self, name: String) -> Self {
        Self {
            scope: Scope::new(name, Some(&self.scope)),
            return_value: None,
        }
    }

    pub fn interpret(&mut self, ast: Node) -> Value {
        self.visit(ast)
    }

    fn visit(&mut self, node: Node) -> Value {
        match node {
            Node::Int(value) => Value::Int(value as i32),
            Node::Float(value) => Value::Float(value),
            Node::Bool(value) => Value::Bool(value),
            Node::Str(value) => Value::Str(value),
            Node::Char(value) => Value::Char(value),
            Node::Array(nodes) => {
                Value::Array(nodes.iter().map(|node| self.visit(node.clone())).collect())
            }
            Node::Fn(name, arg_names, body) => {
                let value = Value::Function(arg_names, *body);
                match name {
                    Some(name) => {
                        self.scope.set(name, value.clone());
                    }
                    None => {}
                };
                value
            }
            Node::Identifier(name) => self.scope.get(&name).clone(),
            Node::Unary(op, node) => {
                let value = self.visit(*node);

                use UnaryOp::*;
                match op {
                    Pos => value,
                    Neg => match value {
                        Value::Int(value) => Value::Int(-value),
                        Value::Float(value) => Value::Float(-value),
                        _ => unimplemented!(),
                    },
                    Not => match value {
                        Value::Int(value) => Value::Int(!value),
                        Value::Float(value) => Value::Bool(value == 0.0),
                        Value::Bool(value) => Value::Bool(!value),
                        _ => unimplemented!(),
                    },
                }
            }
            Node::Binary(left, op, right) => {
                let l_value = self.visit(*left);
                let r_value = self.visit(*right);
                use BinaryOp::*;
                match op {
                    Add => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(l + r),
                            Value::Float(r) => Value::Float(l as f64 + r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(l + r as f64),
                            Value::Float(r) => Value::Float(l + r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Sub => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(l - r),
                            Value::Float(r) => Value::Float(l as f64 - r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(l - r as f64),
                            Value::Float(r) => Value::Float(l - r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Mul => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(l * r),
                            Value::Float(r) => Value::Float(l as f64 * r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(l * (r as f64)),
                            Value::Float(r) => Value::Float(l * r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Div => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(l / r),
                            Value::Float(r) => Value::Float(l as f64 / r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(l / r as f64),
                            Value::Float(r) => Value::Float(l / r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Rem => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(l % r),
                            Value::Float(r) => Value::Float(l as f64 % r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(l % r as f64),
                            Value::Float(r) => Value::Float(l % r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    And => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l != 0 && r != 0),
                            Value::Float(r) => Value::Bool(l != 0 && r != 0.0),
                            Value::Bool(r) => Value::Bool(l != 0 && r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l != 0.0 && r != 0),
                            Value::Float(r) => Value::Bool(l != 0.0 && r != 0.0),
                            Value::Bool(r) => Value::Bool(l != 0.0 && r),
                            _ => unimplemented!(),
                        },
                        Value::Bool(l) => match r_value {
                            Value::Int(r) => Value::Bool(l && r != 0),
                            Value::Float(r) => Value::Bool(l && r != 0.0),
                            Value::Bool(r) => Value::Bool(l && r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Or => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l != 0 || r != 0),
                            Value::Float(r) => Value::Bool(l != 0 || r != 0.0),
                            Value::Bool(r) => Value::Bool(l != 0 || r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l != 0.0 || r != 0),
                            Value::Float(r) => Value::Bool(l != 0.0 || r != 0.0),
                            Value::Bool(r) => Value::Bool(l != 0.0 || r),
                            _ => unimplemented!(),
                        },
                        Value::Bool(l) => match r_value {
                            Value::Int(r) => Value::Bool(l || r != 0),
                            Value::Float(r) => Value::Bool(l || r != 0.0),
                            Value::Bool(r) => Value::Bool(l || r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    EqEq => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l == r),
                            Value::Float(r) => Value::Bool(l as f64 == r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l == r as f64),
                            Value::Float(r) => Value::Bool(l == r),
                            _ => unimplemented!(),
                        },
                        Value::Bool(l) => match r_value {
                            Value::Bool(r) => Value::Bool(l == r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Neq => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l != r),
                            Value::Float(r) => Value::Bool(l as f64 != r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l != r as f64),
                            Value::Float(r) => Value::Bool(l != r),
                            _ => unimplemented!(),
                        },
                        Value::Bool(l) => match r_value {
                            Value::Bool(r) => Value::Bool(l != r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Lt => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l < r),
                            Value::Float(r) => Value::Bool((l as f64) < r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l < r as f64),
                            Value::Float(r) => Value::Bool(l < r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Lte => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l <= r),
                            Value::Float(r) => Value::Bool((l as f64) <= r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l <= r as f64),
                            Value::Float(r) => Value::Bool(l <= r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Gt => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l > r),
                            Value::Float(r) => Value::Bool(l as f64 > r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l > r as f64),
                            Value::Float(r) => Value::Bool(l > r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Gte => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(l >= r),
                            Value::Float(r) => Value::Bool(l as f64 >= r),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(l >= r as f64),
                            Value::Float(r) => Value::Bool(l >= r),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                }
            }
            Node::IdentifierOp(name, op, node) => {
                let value = self.visit(*node.clone());

                use IdentifierOp::*;
                match op {
                    Eq => self.scope.set(name, value),
                    Add => self.visit(Node::IdentifierOp(
                        name.clone(),
                        Eq,
                        Box::new(Node::Binary(
                            Box::new(Node::Identifier(name)),
                            BinaryOp::Add,
                            node,
                        )),
                    )),
                    Sub => self.visit(Node::IdentifierOp(
                        name.clone(),
                        Eq,
                        Box::new(Node::Binary(
                            Box::new(Node::Identifier(name)),
                            BinaryOp::Sub,
                            node,
                        )),
                    )),
                    Mul => self.visit(Node::IdentifierOp(
                        name.clone(),
                        Eq,
                        Box::new(Node::Binary(
                            Box::new(Node::Identifier(name)),
                            BinaryOp::Mul,
                            node,
                        )),
                    )),
                    Div => self.visit(Node::IdentifierOp(
                        name.clone(),
                        Eq,
                        Box::new(Node::Binary(
                            Box::new(Node::Identifier(name)),
                            BinaryOp::Div,
                            node,
                        )),
                    )),
                    Rem => self.visit(Node::IdentifierOp(
                        name.clone(),
                        Eq,
                        Box::new(Node::Binary(
                            Box::new(Node::Identifier(name)),
                            BinaryOp::Rem,
                            node,
                        )),
                    )),
                }
            }
            Node::Index(node, index) => {
                let value = self.visit(*node);
                match value {
                    Value::Str(value) => Value::Char(value.chars().nth(index as usize).unwrap()),
                    Value::Array(value) => value[index as usize].clone(),
                    _ => panic!("index {} is out of bounds", index),
                }
            }
            Node::While(condition, body) => {
                while match self.visit(*condition.clone()) {
                    Value::Bool(value) => value,
                    _ => unimplemented!(),
                } {
                    self.visit(*body.clone());
                    match self.return_value.clone() {
                        Some(value) => return value,
                        None => {}
                    };
                }
                Value::Int(0)
            }
            Node::If(condition, body, else_case) => {
                let condition_value = match self.visit(*condition) {
                    Value::Bool(value) => value,
                    _ => panic!("if statements can only have a bool as their condition"),
                };

                if condition_value {
                    self.visit(*body)
                } else {
                    match else_case {
                        Some(else_case) => self.visit(*else_case),
                        None => Value::Int(0),
                    }
                }
            }
            Node::Return(node) => {
                let value = self.visit(*node);
                self.return_value = Some(value.clone());
                value
            }
            Node::Call(name, args) => {
                let arg_values: Vec<Value> =
                    args.iter().map(|arg| self.visit(arg.clone())).collect();

                match name.as_str() {
                    "print" => {
                        println!(
                            "{}",
                            arg_values
                                .iter()
                                .map(|value| format!("{}", value))
                                .collect::<Vec<String>>()
                                .join(" ")
                        );
                        Value::Int(0)
                    }
                    _ => {
                        let fn_value = self.scope.get(&name).clone();
                        match fn_value {
                            Value::Function(arg_names, body) => {
                                let mut interpreter = self.create_child(name);
                                for (value, name) in arg_values.iter().zip(arg_names) {
                                    interpreter.scope.set(name, value.clone());
                                }
                                interpreter.visit(body)
                            }
                            _ => unimplemented!(),
                        }
                    }
                }
            }
            Node::Statements(nodes) => {
                let mut rtn_value = Value::Int(0);
                for node in nodes {
                    rtn_value = self.visit(node);
                    match self.return_value.clone() {
                        Some(value) => return value,
                        None => {}
                    };
                }
                rtn_value
            }
            _ => unimplemented!(),
        }
    }
}
