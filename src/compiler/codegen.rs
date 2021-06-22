use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FloatValue, FunctionValue, IntValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
    compiler::{Scope, Value},
    BinaryOp, IdentifierOp, Node, Type, TypeLiteral, UnaryOp,
};

pub struct Codegen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub function: FunctionValue<'ctx>,
    pub scope: Scope<'a, 'ctx>,
}

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    pub fn new(
        filename: &str,
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: Builder<'ctx>,
    ) -> Self {
        module.set_source_file_name(filename);

        let i32_type = context.i32_type();
        let str_type = context.i8_type().ptr_type(AddressSpace::Generic);

        let fn_type = i32_type.fn_type(
            &[
                BasicTypeEnum::IntType(i32_type),
                BasicTypeEnum::PointerType(str_type),
            ],
            false,
        );
        let function = module.add_function("main", fn_type, None);
        let block = context.append_basic_block(function, "body");
        builder.position_at_end(block);

        let mut codegen = Self {
            context: &context,
            module: &module,
            builder,
            function,
            scope: Scope::new(None),
        };
        codegen.add_printf();
        codegen
    }

    pub fn create_child(&'a self, function: FunctionValue<'ctx>) -> Self {
        Self {
            context: self.context,
            module: self.module,
            builder: self.context.create_builder(),
            function,
            scope: Scope::new(Some(&self.scope)),
        }
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
            Node::Str(value) => Value::Str({
                let string = self.context.const_string(value.as_bytes(), true);
                let ptr = self.builder.build_alloca(string.get_type(), "str");
                self.builder.build_store(ptr, string);
                ptr
            }),
            Node::Char(value) => Value::Char(self.context.i8_type().const_int(value as u64, false)),
            Node::Array(nodes) => {
                let size = nodes.len() as u32;
                let mut ty = TypeLiteral::Int;

                let mut values: Vec<BasicValueEnum<'ctx>> = vec![];
                for node in nodes {
                    let value = self.visit(node);
                    ty = match value {
                        Value::Int(_) => TypeLiteral::Int,
                        Value::Float(_) => TypeLiteral::Float,
                        Value::Bool(_) => TypeLiteral::Bool,
                        Value::Str(_) => TypeLiteral::Str,
                        Value::Char(_) => TypeLiteral::Char,
                        _ => panic!("invalid array type"),
                    };
                    values.push(value.get_value());
                }

                let int_type = self.context.i32_type();
                let float_type = self.context.f64_type();
                let bool_type = self.context.bool_type();
                let str_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
                let char_type = self.context.i8_type();

                let array = match ty {
                    TypeLiteral::Int => int_type.const_array(
                        values
                            .iter()
                            .map(|value| value.into_int_value())
                            .collect::<Vec<IntValue<'ctx>>>()
                            .as_slice(),
                    ),
                    TypeLiteral::Float => float_type.const_array(
                        values
                            .iter()
                            .map(|value| value.into_float_value())
                            .collect::<Vec<FloatValue<'ctx>>>()
                            .as_slice(),
                    ),
                    TypeLiteral::Bool => bool_type.const_array(
                        values
                            .iter()
                            .map(|value| value.into_int_value())
                            .collect::<Vec<IntValue<'ctx>>>()
                            .as_slice(),
                    ),
                    TypeLiteral::Str => str_type.const_array(
                        values
                            .iter()
                            .map(|value| value.into_pointer_value())
                            .collect::<Vec<PointerValue<'ctx>>>()
                            .as_slice(),
                    ),
                    TypeLiteral::Char => char_type.const_array(
                        values
                            .iter()
                            .map(|value| value.into_int_value())
                            .collect::<Vec<IntValue<'ctx>>>()
                            .as_slice(),
                    ),
                    TypeLiteral::Void => unreachable!(),
                };

                let size_value = int_type.const_int(size as u64, false);
                let ptr = match ty {
                    TypeLiteral::Int => self
                        .builder
                        .build_array_alloca(int_type, size_value, "array"),
                    TypeLiteral::Float => self
                        .builder
                        .build_array_alloca(float_type, size_value, "array"),
                    TypeLiteral::Bool => self
                        .builder
                        .build_array_alloca(bool_type, size_value, "array"),
                    TypeLiteral::Str => self
                        .builder
                        .build_array_alloca(str_type, size_value, "array"),
                    TypeLiteral::Char => self
                        .builder
                        .build_array_alloca(char_type, size_value, "array"),
                    TypeLiteral::Void => unreachable!(),
                };
                self.builder.build_store(ptr, array);

                Value::Array(ptr, ty, size)
            }
            Node::Cast(ty, node) => {
                let value = self.visit(*node);

                let int_type = self.context.i32_type();
                let float_type = self.context.f64_type();
                let bool_type = self.context.bool_type();

                match ty {
                    Type::Int => Value::Int(match value {
                        Value::Int(value) | Value::Bool(value) => value,
                        Value::Float(value) => self
                            .builder
                            .build_float_to_signed_int(value, int_type, "int"),
                        _ => unimplemented!(),
                    }),
                    Type::Float => Value::Float(match value {
                        Value::Int(value) => self
                            .builder
                            .build_signed_int_to_float(value, float_type, "float"),
                        Value::Float(value) => value,
                        Value::Bool(value) => self
                            .builder
                            .build_unsigned_int_to_float(value, float_type, "float"),
                        _ => unimplemented!(),
                    }),
                    Type::Bool => Value::Bool(match value {
                        Value::Int(value) | Value::Bool(value) => value,
                        Value::Float(value) => self
                            .builder
                            .build_float_to_unsigned_int(value, bool_type, "bool"),
                        _ => unimplemented!(),
                    }),
                    Type::Str => Value::Str(match value {
                        Value::Str(value) => value,
                        _ => unimplemented!(),
                    }),
                    Type::Char => Value::Char(match value {
                        Value::Char(value) => value,
                        _ => unimplemented!(),
                    }),
                    Type::Array(_, _) => panic!("can't cast to an array"),
                    Type::Void => panic!("can't cast to a void type"),
                }
            }
            Node::Identifier(name) => self.scope.get(&name, &self.builder),
            Node::Unary(op, node) => {
                let value = self.visit(*node);

                use UnaryOp::*;
                match op {
                    Pos => value,
                    Neg => match value {
                        Value::Int(value) => Value::Int(value.const_neg()),
                        Value::Float(value) => Value::Float(value.const_neg()),
                        _ => unimplemented!(),
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
                        _ => unimplemented!(),
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
                    And => match l_value {
                        Value::Bool(l) => match r_value {
                            Value::Bool(r) => Value::Bool(self.builder.build_and(l, r, "and")),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    Or => match l_value {
                        Value::Bool(l) => match r_value {
                            Value::Bool(r) => Value::Bool(self.builder.build_or(l, r, "or")),
                            _ => unimplemented!(),
                        },
                        _ => unimplemented!(),
                    },
                    EqEq => match l_value {
                        Value::Int(l) | Value::Char(l) => match r_value {
                            Value::Int(r) | Value::Char(r) => Value::Bool(
                                self.builder
                                    .build_int_compare(IntPredicate::EQ, l, r, "eqeq"),
                            ),
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
                        _ => unimplemented!(),
                    },
                    Neq => match l_value {
                        Value::Int(l) | Value::Char(l) => match r_value {
                            Value::Int(r) | Value::Char(r) => Value::Bool(
                                self.builder
                                    .build_int_compare(IntPredicate::NE, l, r, "neq"),
                            ),
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
                        _ => unimplemented!(),
                    },
                    Lt => match l_value {
                        Value::Int(l) | Value::Char(l) => match r_value {
                            Value::Int(r) | Value::Char(r) => Value::Bool(
                                self.builder
                                    .build_int_compare(IntPredicate::SLT, l, r, "lt"),
                            ),
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
                        Value::Int(l) | Value::Char(l) => match r_value {
                            Value::Int(r) | Value::Char(r) => Value::Bool(
                                self.builder
                                    .build_int_compare(IntPredicate::SLE, l, r, "lte"),
                            ),
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
                        Value::Int(l) | Value::Char(l) => match r_value {
                            Value::Int(r) | Value::Char(r) => Value::Bool(
                                self.builder
                                    .build_int_compare(IntPredicate::SGT, l, r, "gt"),
                            ),
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
                        Value::Int(l) | Value::Char(l) => match r_value {
                            Value::Int(r) | Value::Char(r) => Value::Bool(
                                self.builder
                                    .build_int_compare(IntPredicate::SGE, l, r, "gte"),
                            ),
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
                    Eq => self.scope.set(name, value, &self.context, &self.builder),
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
            Node::Index(node, index) => {
                let value = self.visit(*node);
                match value {
                    Value::Str(value) => Value::Char({
                        let chars = self.builder.build_load(value, "str").into_array_value();
                        match self.builder.build_extract_value(chars, index, "index") {
                            Some(value) => value.into_int_value(),
                            None => self.context.i8_type().const_zero(),
                        }
                    }),
                    Value::Array(value, ty, _) => {
                        let array = self.builder.build_load(value, "array").into_array_value();
                        let item = self.builder.build_extract_value(array, index, "index");
                        println!("ITEM: {:#?}", item);
                        match item {
                            Some(value) => match ty {
                                TypeLiteral::Int => Value::Int(value.into_int_value()),
                                TypeLiteral::Float => Value::Float(value.into_float_value()),
                                TypeLiteral::Bool => Value::Bool(value.into_int_value()),
                                TypeLiteral::Str => Value::Str(value.into_pointer_value()),
                                TypeLiteral::Char => Value::Char(value.into_int_value()),
                                TypeLiteral::Void => panic!("can't have a void array"),
                            },
                            None => panic!("failed to index array"),
                        }
                    }
                    _ => panic!("index {} is out of bounds", index),
                }
            }
            Node::While(condition, body) => {
                let condition_block = self.context.append_basic_block(self.function, "while_cond");
                self.builder.build_unconditional_branch(condition_block);
                self.builder.position_at_end(condition_block);
                let condition_value = match self.visit(*condition) {
                    Value::Bool(value) => value,
                    _ => panic!("while loops can only have a bool as their condition"),
                };

                let loop_block = self.context.append_basic_block(self.function, "while_loop");
                self.builder.position_at_end(loop_block);
                self.visit(*body);

                let end_block = self.context.append_basic_block(self.function, "while_end");
                self.builder.build_unconditional_branch(condition_block);

                self.builder.position_at_end(condition_block);
                self.builder
                    .build_conditional_branch(condition_value, loop_block, end_block);

                self.builder.position_at_end(end_block);

                Value::Int(self.context.i32_type().const_zero())
            }
            Node::If(condition, body, else_case) => {
                let condition_value = match self.visit(*condition) {
                    Value::Bool(value) => value,
                    _ => panic!("if statements can only have a bool as their condition"),
                };

                let then_block = self.context.append_basic_block(self.function, "then");
                match else_case {
                    Some(else_case) => {
                        let else_block = self.context.append_basic_block(self.function, "else");
                        let end_block = self.context.append_basic_block(self.function, "if_end");

                        self.builder.build_conditional_branch(
                            condition_value,
                            then_block,
                            else_block,
                        );

                        // Then
                        self.builder.position_at_end(then_block);
                        let then_value = self.visit(*body);
                        self.builder.build_unconditional_branch(end_block);

                        let then_block = self.builder.get_insert_block().unwrap();

                        // Else
                        self.builder.position_at_end(else_block);
                        let else_value = self.visit(*else_case);
                        self.builder.build_unconditional_branch(end_block);

                        let else_block = self.builder.get_insert_block().unwrap();

                        self.builder.position_at_end(end_block);

                        let phi = self
                            .builder
                            .build_phi(then_value.get_type(self.context), "phi");
                        phi.add_incoming(&[
                            (&then_value.get_value(), then_block),
                            (&else_value.get_value(), else_block),
                        ]);

                        let phi_value = phi.as_basic_value();
                        match then_value {
                            Value::Int(_) => Value::Int(phi_value.into_int_value()),
                            Value::Float(_) => Value::Float(phi_value.into_float_value()),
                            Value::Bool(_) => Value::Bool(phi_value.into_int_value()),
                            Value::Str(_) => Value::Str(phi_value.into_pointer_value()),
                            Value::Char(_) => Value::Char(phi_value.into_int_value()),
                            Value::Array(_, ty, size) => {
                                Value::Array(phi_value.into_pointer_value(), ty, size)
                            }
                            Value::Void => panic!("void isn't a valid type"),
                        }
                    }
                    None => {
                        let end_block = self.context.append_basic_block(self.function, "end");

                        self.builder.build_conditional_branch(
                            condition_value,
                            then_block,
                            end_block,
                        );

                        // Then
                        self.builder.position_at_end(then_block);
                        self.visit(*body);
                        self.builder.build_unconditional_branch(end_block);

                        self.builder.position_at_end(end_block);

                        Value::Int(self.context.i32_type().const_zero())
                    }
                }
            }
            Node::Fn(name, args, return_type, body) => {
                let int_type = self.context.i32_type();
                let float_type = self.context.f64_type();
                let bool_type = self.context.bool_type();
                let str_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
                let char_type = self.context.i8_type();
                let void_type = self.context.void_type();

                macro_rules! make_args {
                    () => {
                        args.iter()
                            .map(|(_, ty)| ty.get_type(self.context))
                            .collect::<Vec<BasicTypeEnum>>()
                            .as_slice()
                    };
                }

                let fn_type = match return_type {
                    Type::Int => int_type.fn_type(make_args!(), false),
                    Type::Float => float_type.fn_type(make_args!(), false),
                    Type::Bool => bool_type.fn_type(make_args!(), false),
                    Type::Str => str_type.fn_type(make_args!(), false),
                    Type::Char => char_type.fn_type(make_args!(), false),
                    Type::Array(ty, size) => match ty {
                        TypeLiteral::Int => int_type.array_type(size).fn_type(make_args!(), false),
                        TypeLiteral::Float => {
                            float_type.array_type(size).fn_type(make_args!(), false)
                        }
                        TypeLiteral::Bool => {
                            bool_type.array_type(size).fn_type(make_args!(), false)
                        }
                        TypeLiteral::Str => str_type.array_type(size).fn_type(make_args!(), false),
                        TypeLiteral::Char => {
                            char_type.array_type(size).fn_type(make_args!(), false)
                        }
                        TypeLiteral::Void => panic!("can't have a void array"),
                    },
                    Type::Void => void_type.fn_type(make_args!(), false),
                };
                let function = self.module.add_function(&name, fn_type, None);
                let block = self.context.append_basic_block(function, "body");

                let mut codegen = self.create_child(self.function);
                codegen.builder.position_at_end(block);
                args.iter().enumerate().for_each(|(i, (arg_name, ty))| {
                    let arg_name = arg_name.clone();
                    let value = function.get_nth_param(i as u32).unwrap();
                    match ty {
                        Type::Int => {
                            let val_ptr = codegen.builder.build_alloca(int_type, &arg_name);
                            codegen
                                .scope
                                .variables
                                .insert(arg_name, (val_ptr, Type::Int));
                            codegen.builder.build_store(val_ptr, value.into_int_value());
                        }
                        Type::Float => {
                            let val_ptr = codegen.builder.build_alloca(float_type, &arg_name);
                            codegen
                                .scope
                                .variables
                                .insert(arg_name, (val_ptr, Type::Float));
                            codegen
                                .builder
                                .build_store(val_ptr, value.into_float_value());
                        }
                        Type::Bool => {
                            let val_ptr = codegen.builder.build_alloca(bool_type, &arg_name);
                            codegen
                                .scope
                                .variables
                                .insert(arg_name, (val_ptr, Type::Bool));
                            codegen.builder.build_store(val_ptr, value.into_int_value());
                        }
                        Type::Str => {
                            codegen
                                .scope
                                .variables
                                .insert(arg_name, (value.into_pointer_value(), Type::Str));
                        }
                        Type::Char => {
                            let val_ptr = codegen.builder.build_alloca(bool_type, &arg_name);
                            codegen
                                .scope
                                .variables
                                .insert(arg_name, (val_ptr, Type::Char));
                            codegen.builder.build_store(val_ptr, value.into_int_value());
                        }
                        Type::Array(arr_ty, size) => {
                            codegen.scope.variables.insert(
                                arg_name,
                                (value.into_pointer_value(), Type::Array(*arr_ty, *size)),
                            );
                        }
                        Type::Void => panic!("void isn't a valid argument type"),
                    };
                });

                codegen.visit(*body);
                if return_type == Type::Void {
                    codegen.builder.build_return(None);
                }

                self.scope.add_function(name, function, return_type);

                Value::Int(int_type.const_zero())
            }
            Node::Return(node) => {
                let value = self.visit(*node);
                self.builder.build_return(Some(match &value {
                    Value::Int(value) => value,
                    Value::Float(value) => value,
                    Value::Bool(value) => value,
                    Value::Str(value) => value,
                    Value::Char(value) => value,
                    Value::Array(value, _, _) => value,
                    Value::Void => panic!("void isn't a valid type"),
                }));
                Value::Int(self.context.i32_type().const_zero())
            }
            Node::Call(name, args) => {
                let mut compiled_args: Vec<Value<'ctx>> = vec![];
                for arg in args {
                    compiled_args.push(self.visit(arg));
                }

                let fn_value = self.scope.get_function(&name);
                match fn_value {
                    (function, return_type) => {
                        let format_string = self.generate_printf_format_string(&compiled_args);

                        let mut argsv: Vec<BasicValueEnum<'ctx>> = vec![];
                        for val in compiled_args {
                            argsv.push(match val {
                                Value::Int(value) => value.into(),
                                Value::Float(value) => value.into(),
                                Value::Bool(value) => value.into(),
                                Value::Str(value) => value.into(),
                                Value::Char(value) => value.into(),
                                Value::Array(value, _, _) => value.into(),
                                Value::Void => panic!("void isn't a valid type"),
                            });
                        }

                        if name == "print" {
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
                                Type::Int => Value::Int(value.into_int_value()),
                                Type::Float => Value::Float(value.into_float_value()),
                                Type::Bool => Value::Bool(value.into_int_value()),
                                Type::Str => Value::Str(value.into_pointer_value()),
                                Type::Char => Value::Char(value.into_int_value()),
                                Type::Array(ty, size) => {
                                    Value::Array(value.into_pointer_value(), *ty, *size)
                                }
                                Type::Void => unreachable!(),
                            },
                            None => match return_type {
                                Type::Void => Value::Void,
                                _ => panic!("Invalid call to {}", name),
                            },
                        }
                    }
                }
            }
            Node::Statements(nodes) => {
                let mut rtn_value = Value::Int(self.context.i32_type().const_zero());
                for node in nodes {
                    rtn_value = self.visit(node);
                }
                rtn_value
            }
            _ => unimplemented!(),
        }
    }
}
// weekendvibes
