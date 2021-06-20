use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue},
    AddressSpace, FloatPredicate, IntPredicate,
};

use crate::{
    compiler::{Scope, Value},
    BinaryOp, IdentifierOp, Node, TypeLiteral, UnaryOp,
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
        let builder = self.context.create_builder();
        Self {
            context: self.context,
            module: self.module,
            builder,
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
            Node::Str(value) => Value::Str(
                self.builder
                    .build_global_string_ptr(&value, "str")
                    .as_pointer_value(),
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
                        _ => unimplemented!(),
                    }),
                    TypeLiteral::Float => Value::Float(match value {
                        Value::Int(value) => self
                            .builder
                            .build_signed_int_to_float(value, f64_type, "float"),
                        Value::Float(value) => value,
                        Value::Bool(value) => self
                            .builder
                            .build_unsigned_int_to_float(value, f64_type, "float"),
                        _ => unimplemented!(),
                    }),
                    TypeLiteral::Bool => Value::Bool(match value {
                        Value::Int(value) | Value::Bool(value) => value,
                        Value::Float(value) => self
                            .builder
                            .build_float_to_unsigned_int(value, bool_type, "bool"),
                        _ => unimplemented!(),
                    }),
                    TypeLiteral::Str => Value::Str(match value {
                        Value::Str(value) => value,
                        _ => unimplemented!(),
                    }),
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
                        _ => unimplemented!(),
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
                        _ => unimplemented!(),
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
            Node::If(condition, body, else_case) => {
                let condition_value = match self.visit(*condition) {
                    Value::Bool(value) => value,
                    _ => panic!("if statements can only have a bool as their condition"),
                };

                let then_block = self.context.append_basic_block(self.function, "then");
                match else_case {
                    Some(else_case) => {
                        let else_block = self.context.append_basic_block(self.function, "else");
                        let end_block = self.context.append_basic_block(self.function, "end");

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

                        let phi = self.builder.build_phi(
                            match then_value {
                                Value::Int(_) => BasicTypeEnum::IntType(self.context.i32_type()),
                                Value::Float(_) => {
                                    BasicTypeEnum::FloatType(self.context.f64_type())
                                }
                                Value::Bool(_) => BasicTypeEnum::IntType(self.context.bool_type()),
                                Value::Str(_) => BasicTypeEnum::VectorType(
                                    self.context.const_string(&[], false).get_type(),
                                ),
                            },
                            "phi",
                        );
                        phi.add_incoming(&[
                            (
                                &match then_value {
                                    Value::Int(value) => BasicValueEnum::IntValue(value),
                                    Value::Float(value) => BasicValueEnum::FloatValue(value),
                                    Value::Bool(value) => BasicValueEnum::IntValue(value),
                                    Value::Str(value) => BasicValueEnum::PointerValue(value),
                                },
                                then_block,
                            ),
                            (
                                &match else_value {
                                    Value::Int(value) => BasicValueEnum::IntValue(value),
                                    Value::Float(value) => BasicValueEnum::FloatValue(value),
                                    Value::Bool(value) => BasicValueEnum::IntValue(value),
                                    Value::Str(value) => BasicValueEnum::PointerValue(value),
                                },
                                else_block,
                            ),
                        ]);

                        let phi_value = phi.as_basic_value();
                        match then_value {
                            Value::Int(_) => Value::Int(phi_value.into_int_value()),
                            Value::Float(_) => Value::Float(phi_value.into_float_value()),
                            Value::Bool(_) => Value::Bool(phi_value.into_int_value()),
                            Value::Str(_) => Value::Str(phi_value.into_pointer_value()),
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
                let i32_type = self.context.i32_type();
                let f64_type = self.context.f64_type();
                let bool_type = self.context.bool_type();
                let str_type = self.context.i8_type().ptr_type(AddressSpace::Generic);

                let fn_type = match return_type {
                    TypeLiteral::Int => i32_type.fn_type(
                        args.iter()
                            .map(|(_, literal)| match literal {
                                TypeLiteral::Int => BasicTypeEnum::IntType(i32_type),
                                TypeLiteral::Float => BasicTypeEnum::FloatType(f64_type),
                                TypeLiteral::Bool => BasicTypeEnum::IntType(bool_type),
                                TypeLiteral::Str => BasicTypeEnum::PointerType(str_type),
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
                                TypeLiteral::Str => BasicTypeEnum::PointerType(str_type),
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
                                TypeLiteral::Str => BasicTypeEnum::PointerType(str_type),
                            })
                            .collect::<Vec<BasicTypeEnum>>()
                            .as_slice(),
                        false,
                    ),
                    TypeLiteral::Str => str_type.fn_type(
                        args.iter()
                            .map(|(_, literal)| match literal {
                                TypeLiteral::Int => BasicTypeEnum::IntType(i32_type),
                                TypeLiteral::Float => BasicTypeEnum::FloatType(f64_type),
                                TypeLiteral::Bool => BasicTypeEnum::IntType(bool_type),
                                TypeLiteral::Str => BasicTypeEnum::PointerType(str_type),
                            })
                            .collect::<Vec<BasicTypeEnum>>()
                            .as_slice(),
                        false,
                    ),
                };
                let function = self.module.add_function(&name, fn_type, None);
                let block = self.context.append_basic_block(function, "body");

                let mut codegen = self.create_child(self.function);
                codegen.builder.position_at_end(block);
                args.iter()
                    .enumerate()
                    .for_each(|(i, (arg_name, literal))| {
                        let arg_name = arg_name.clone();
                        let value = function.get_nth_param(i as u32).unwrap();
                        match literal {
                            TypeLiteral::Int => codegen.scope.set(
                                arg_name,
                                Value::Int(value.into_int_value()),
                                &self.context,
                                &self.builder,
                            ),
                            TypeLiteral::Float => codegen.scope.set(
                                arg_name,
                                Value::Float(value.into_float_value()),
                                &self.context,
                                &self.builder,
                            ),
                            TypeLiteral::Bool => codegen.scope.set(
                                arg_name,
                                Value::Bool(value.into_int_value()),
                                &self.context,
                                &self.builder,
                            ),
                            TypeLiteral::Str => codegen.scope.set(
                                arg_name,
                                Value::Str(value.into_pointer_value()),
                                &self.context,
                                &self.builder,
                            ),
                        };
                    });

                codegen.visit(*body);

                self.scope.add_function(name, function, return_type);

                Value::Int(i32_type.const_zero())
            }
            Node::Return(node) => {
                let value = self.visit(*node);
                self.builder.build_return(Some(match &value {
                    Value::Int(value) => value,
                    Value::Float(value) => value,
                    Value::Bool(value) => value,
                    Value::Str(value) => value,
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
                                TypeLiteral::Int => Value::Int(value.into_int_value()),
                                TypeLiteral::Float => Value::Float(value.into_float_value()),
                                TypeLiteral::Bool => Value::Bool(value.into_int_value()),
                                TypeLiteral::Str => Value::Str(value.into_pointer_value()),
                            },
                            None => panic!("Invalid call to {}", name),
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
            _ => panic!("Unknown node: {:?}", node),
        }
    }
}
// weekendvibes
