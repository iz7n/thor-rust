use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue},
    FloatPredicate, IntPredicate,
};

use crate::{BinaryOp, IdentifierOp, Node, TypeLiteral, UnaryOp};

pub enum Value<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
    Bool(IntValue<'ctx>),
}

pub struct Codegen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub functions: HashMap<String, (FunctionValue<'ctx>, TypeLiteral)>,
    pub variables: HashMap<String, (PointerValue<'ctx>, TypeLiteral)>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    pub fn init(&mut self, filename: &str) {
        self.module.set_source_file_name(filename);
        self.generate_main_fn();
        self.add_printf();
    }

    pub fn generate_llvm_ir(&mut self, ast: Node) {
        self.visit(ast);
        self.builder
            .build_return(Some(&self.context.i32_type().const_zero()));
    }

    fn visit(&mut self, node: Node) -> Value<'ctx> {
        match node {
            Node::Int(value) => Value::Int(self.context.i32_type().const_int(value as u64, true)),
            Node::Float(value) => Value::Float(self.context.f64_type().const_float(value)),
            Node::Bool(value) => Value::Bool(
                self.context
                    .bool_type()
                    .const_int(if value { 1 } else { 0 }, false),
            ),
            Node::Cast(literal, node) => {
                let value = self.visit(*node);

                let i32_type = self.context.i32_type();
                let f64_type = self.context.f64_type();
                let bool_type = self.context.bool_type();

                match literal {
                    TypeLiteral::Int => Value::Int(match value {
                        Value::Int(value) | Value::Bool(value) => value,
                        Value::Float(value) => self
                            .builder
                            .build_float_to_signed_int(value, i32_type, "int"),
                    }),
                    TypeLiteral::Float => Value::Float(match value {
                        Value::Int(value) => self
                            .builder
                            .build_signed_int_to_float(value, f64_type, "float"),
                        Value::Float(value) => value,
                        Value::Bool(value) => self
                            .builder
                            .build_unsigned_int_to_float(value, f64_type, "float"),
                    }),
                    TypeLiteral::Bool => Value::Bool(match value {
                        Value::Int(value) | Value::Bool(value) => value,
                        Value::Float(value) => self
                            .builder
                            .build_float_to_unsigned_int(value, bool_type, "bool"),
                    }),
                }
            }
            Node::Identifier(name) => match self.variables.get(name.as_str()) {
                Some((ptr, literal)) => {
                    let value = self.builder.build_load(*ptr, &name);
                    match literal {
                        TypeLiteral::Int => Value::Int(value.into_int_value()),
                        TypeLiteral::Float => Value::Float(value.into_float_value()),
                        TypeLiteral::Bool => Value::Bool(value.into_int_value()),
                    }
                }
                None => panic!("{} is not defined", name),
            },
            Node::Unary(op, node) => {
                let value = self.visit(*node);

                use UnaryOp::*;
                match op {
                    Pos => value,
                    Neg => match value {
                        Value::Int(value) => Value::Int(value.const_neg()),
                        Value::Float(value) => Value::Float(value.const_neg()),
                        Value::Bool(_) => unimplemented!(),
                    },
                    Not => match value {
                        Value::Int(value) => Value::Bool(self.builder.build_int_compare(
                            IntPredicate::EQ,
                            value,
                            self.context.i32_type().const_zero(),
                            "not",
                        )),
                        Value::Float(value) => Value::Bool(self.builder.build_float_compare(
                            FloatPredicate::OEQ,
                            value,
                            self.context.f64_type().const_zero(),
                            "not",
                        )),
                        Value::Bool(value) => Value::Bool(self.builder.build_int_compare(
                            IntPredicate::EQ,
                            value,
                            self.context.bool_type().const_zero(),
                            "not",
                        )),
                    },
                }
            }
            Node::Binary(left, op, right) => {
                let l_value = self.visit(*left);
                let r_value = self.visit(*right);

                let f64_type = self.context.f64_type();

                use BinaryOp::*;
                match op {
                    Add => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(self.builder.build_int_add(l, r, "add")),
                            Value::Float(r) => Value::Float(self.builder.build_float_add(
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "add",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(self.builder.build_float_add(
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "add",
                            )),
                            Value::Float(r) => {
                                Value::Float(self.builder.build_float_add(l, r, "add"))
                            }
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Sub => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(self.builder.build_int_sub(l, r, "sub")),
                            Value::Float(r) => Value::Float(self.builder.build_float_sub(
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "sub",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(self.builder.build_float_sub(
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "sub",
                            )),
                            Value::Float(r) => {
                                Value::Float(self.builder.build_float_sub(l, r, "sub"))
                            }
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Mul => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Int(self.builder.build_int_mul(l, r, "mul")),
                            Value::Float(r) => Value::Float(self.builder.build_float_mul(
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "mul",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(self.builder.build_float_mul(
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "mul",
                            )),
                            Value::Float(r) => {
                                Value::Float(self.builder.build_float_mul(l, r, "mul"))
                            }
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Div => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => {
                                Value::Int(self.builder.build_int_unsigned_div(l, r, "div"))
                            }
                            Value::Float(r) => Value::Float(self.builder.build_float_div(
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "div",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Float(self.builder.build_float_div(
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "div",
                            )),
                            Value::Float(r) => {
                                Value::Float(self.builder.build_float_div(l, r, "div"))
                            }
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    And => unimplemented!(),
                    Or => unimplemented!(),
                    EqEq => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_int_compare(
                                IntPredicate::EQ,
                                l,
                                r,
                                "eqeq",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OEQ,
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "eqeq",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OEQ,
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "eqeq",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OEQ,
                                l,
                                r,
                                "eqeq",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Bool(l) => match r_value {
                            Value::Bool(r) => Value::Bool(
                                self.builder
                                    .build_not(self.builder.build_xor(l, r, "xor"), "not"),
                            ),
                            _ => unimplemented!(),
                        },
                    },
                    Neq => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_int_compare(
                                IntPredicate::NE,
                                l,
                                r,
                                "neq",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::ONE,
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "neq",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::ONE,
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "neq",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::ONE,
                                l,
                                r,
                                "neq",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Bool(l) => match r_value {
                            Value::Bool(r) => Value::Bool(self.builder.build_xor(l, r, "xor")),
                            _ => unimplemented!(),
                        },
                    },
                    Lt => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_int_compare(
                                IntPredicate::SLT,
                                l,
                                r,
                                "lt",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OLT,
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "lt",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OLT,
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "lt",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OLT,
                                l,
                                r,
                                "lt",
                            )),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Lte => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_int_compare(
                                IntPredicate::SLE,
                                l,
                                r,
                                "lte",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OLE,
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "lte",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OLE,
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "lte",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OLE,
                                l,
                                r,
                                "lte",
                            )),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Gt => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_int_compare(
                                IntPredicate::SGT,
                                l,
                                r,
                                "gt",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OGT,
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "gt",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OGT,
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "gt",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OGT,
                                l,
                                r,
                                "gt",
                            )),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Gte => match l_value {
                        Value::Int(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_int_compare(
                                IntPredicate::SGE,
                                l,
                                r,
                                "gte",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OGE,
                                self.builder.build_signed_int_to_float(l, f64_type, "left"),
                                r,
                                "gte",
                            )),
                            _ => unimplemented!(),
                        },
                        Value::Float(l) => match r_value {
                            Value::Int(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OGE,
                                l,
                                self.builder.build_signed_int_to_float(r, f64_type, "right"),
                                "gte",
                            )),
                            Value::Float(r) => Value::Bool(self.builder.build_float_compare(
                                FloatPredicate::OGE,
                                l,
                                r,
                                "gte",
                            )),
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
                    Eq => {
                        let val_ptr_result = self.variables.get(name.as_str());
                        match value {
                            Value::Int(value) => {
                                let val_ptr = match val_ptr_result {
                                    None => {
                                        self.builder.build_alloca(self.context.i32_type(), &name)
                                    }
                                    Some((ptr, _)) => *ptr,
                                };
                                self.variables.insert(name, (val_ptr, TypeLiteral::Int));
                                self.builder.build_store(val_ptr, value);
                            }
                            Value::Float(value) => {
                                let val_ptr = match val_ptr_result {
                                    None => {
                                        self.builder.build_alloca(self.context.f64_type(), &name)
                                    }
                                    Some((ptr, _)) => *ptr,
                                };
                                self.variables.insert(name, (val_ptr, TypeLiteral::Float));
                                self.builder.build_store(val_ptr, value);
                            }
                            Value::Bool(value) => {
                                let val_ptr = match val_ptr_result {
                                    None => {
                                        self.builder.build_alloca(self.context.bool_type(), &name)
                                    }
                                    Some((ptr, _)) => *ptr,
                                };
                                self.variables.insert(name, (val_ptr, TypeLiteral::Bool));
                                self.builder.build_store(val_ptr, value);
                            }
                        };

                        value
                    }
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
                }
            }
            Node::Fn(name, args, return_type, body) => {
                let i32_type = self.context.i32_type();
                let f64_type = self.context.f64_type();
                let bool_type = self.context.bool_type();

                let fn_type = match return_type {
                    TypeLiteral::Int => i32_type.fn_type(
                        args.iter()
                            .map(|(_, literal)| match literal {
                                TypeLiteral::Int => BasicTypeEnum::IntType(i32_type),
                                TypeLiteral::Float => BasicTypeEnum::FloatType(f64_type),
                                TypeLiteral::Bool => BasicTypeEnum::IntType(bool_type),
                            })
                            .collect::<Vec<BasicTypeEnum>>()
                            .as_slice(),
                        false,
                    ),
                    TypeLiteral::Float => f64_type.fn_type(
                        args.iter()
                            .map(|(_, literal)| match literal {
                                TypeLiteral::Int => BasicTypeEnum::IntType(i32_type),
                                TypeLiteral::Float => BasicTypeEnum::FloatType(f64_type),
                                TypeLiteral::Bool => BasicTypeEnum::IntType(bool_type),
                            })
                            .collect::<Vec<BasicTypeEnum>>()
                            .as_slice(),
                        false,
                    ),
                    TypeLiteral::Bool => bool_type.fn_type(
                        args.iter()
                            .map(|(_, literal)| match literal {
                                TypeLiteral::Int => BasicTypeEnum::IntType(i32_type),
                                TypeLiteral::Float => BasicTypeEnum::FloatType(f64_type),
                                TypeLiteral::Bool => BasicTypeEnum::IntType(bool_type),
                            })
                            .collect::<Vec<BasicTypeEnum>>()
                            .as_slice(),
                        false,
                    ),
                };
                let function = self.module.add_function(&name, fn_type, None);
                let block = self.context.append_basic_block(function, "body");
                let builder = self.context.create_builder();
                builder.position_at_end(block);

                let mut variables: HashMap<String, (PointerValue<'ctx>, TypeLiteral)> =
                    HashMap::new();
                args.iter()
                    .enumerate()
                    .for_each(|(i, (arg_name, literal))| {
                        let value = function.get_nth_param(i as u32).unwrap();
                        match literal {
                            TypeLiteral::Int => {
                                let val_ptr = builder.build_alloca(i32_type, &arg_name);
                                variables.insert(arg_name.clone(), (val_ptr, TypeLiteral::Int));
                                builder.build_store(val_ptr, value.into_int_value());
                            }
                            TypeLiteral::Float => {
                                let val_ptr = builder.build_alloca(f64_type, &arg_name);
                                variables.insert(arg_name.clone(), (val_ptr, TypeLiteral::Float));
                                builder.build_store(val_ptr, value.into_float_value());
                            }
                            TypeLiteral::Bool => {
                                let val_ptr = builder.build_alloca(bool_type, &arg_name);
                                variables.insert(arg_name.clone(), (val_ptr, TypeLiteral::Bool));
                                builder.build_store(val_ptr, value.into_int_value());
                            }
                        };
                    });

                let mut codegen = Self {
                    context: self.context,
                    module: self.module,
                    builder,
                    functions: HashMap::new(),
                    variables,
                };
                codegen.add_printf();
                codegen.visit(*body);

                self.functions.insert(name, (function, return_type));

                Value::Int(i32_type.const_zero())
            }
            Node::Return(node) => {
                let value = self.visit(*node);
                self.builder.build_return(Some(match &value {
                    Value::Int(value) => value,
                    Value::Float(value) => value,
                    Value::Bool(value) => value,
                }));
                Value::Int(self.context.i32_type().const_zero())
            }
            Node::Call(name, args) => {
                let mut compiled_args: Vec<Value<'ctx>> = vec![];
                for arg in args {
                    compiled_args.push(self.visit(arg));
                }

                let fn_value = self.functions.get(&name).clone();
                match fn_value {
                    Some((function, return_type)) => {
                        let mut argsv: Vec<BasicValueEnum<'ctx>> = vec![];
                        for val in compiled_args {
                            argsv.push(match val {
                                Value::Int(value) => value.into(),
                                Value::Float(value) => value.into(),
                                Value::Bool(value) => value.into(),
                            });
                        }

                        if name == "print" {
                            let format_string = self.generate_printf_format_string(argsv.clone());
                            argsv.insert(0, format_string);
                        }

                        match self
                            .builder
                            .build_call(
                                *function,
                                argsv.as_slice(),
                                match name.as_str() {
                                    "print" => "printf",
                                    _ => &name,
                                },
                            )
                            .try_as_basic_value()
                            .left()
                        {
                            Some(value) => match return_type {
                                TypeLiteral::Int => Value::Int(value.into_int_value()),
                                TypeLiteral::Float => Value::Float(value.into_float_value()),
                                TypeLiteral::Bool => Value::Bool(value.into_int_value()),
                            },
                            None => panic!("Invalid call to {}", name),
                        }
                    }
                    None => panic!("function {} is not defined", name),
                }
            }
            Node::Statements(nodes) => {
                for node in nodes {
                    self.visit(node);
                }
                Value::Int(self.context.i32_type().const_zero())
            }
            _ => panic!("Unknown node: {:?}", node),
        }
    }
}
// weekendvibes
