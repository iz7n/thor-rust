use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
    IntPredicate,
};

use crate::{BinaryOp, IdentifierOp, Node, UnaryOp};

pub struct Codegen<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub functions: HashMap<String, FunctionValue<'ctx>>,
    pub variables: HashMap<String, PointerValue<'ctx>>,
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
            .build_return(Some(&self.context.i32_type().const_int(0, false)));
    }

    fn visit(&mut self, node: Node) -> IntValue<'ctx> {
        match node {
            Node::Int(value) => self.context.i32_type().const_int(value as u64, true),
            Node::Identifier(name) => match self.variables.get(name.as_str()) {
                Some(var) => self.builder.build_load(*var, &name).into_int_value(),
                None => panic!("{} is not defined", name),
            },
            Node::Unary(op, node) => {
                let value = self.visit(*node);

                use UnaryOp::*;
                match op {
                    Pos => value,
                    Neg => value.const_neg(),
                    Not => value,
                }
            }
            Node::Binary(left, op, right) => {
                let l_value = self.visit(*left);
                let r_value = self.visit(*right);

                use BinaryOp::*;
                match op {
                    Add => self.builder.build_int_add(l_value, r_value, "add"),
                    Sub => self.builder.build_int_sub(l_value, r_value, "sub"),
                    Mul => self.builder.build_int_mul(l_value, r_value, "mul"),
                    Div => self.builder.build_int_signed_div(l_value, r_value, "div"),
                    And => self.builder.build_and(l_value, r_value, "and"),
                    Or => self.builder.build_or(l_value, r_value, "or"),
                    EqEq => {
                        self.builder
                            .build_int_compare(IntPredicate::EQ, l_value, r_value, "eqeq")
                    }
                    Neq => {
                        self.builder
                            .build_int_compare(IntPredicate::NE, l_value, r_value, "neq")
                    }
                    Lt => self
                        .builder
                        .build_int_compare(IntPredicate::SLT, l_value, r_value, "lt"),
                    Lte => {
                        self.builder
                            .build_int_compare(IntPredicate::SLE, l_value, r_value, "lte")
                    }
                    Gt => self
                        .builder
                        .build_int_compare(IntPredicate::SGT, l_value, r_value, "gt"),
                    Gte => {
                        self.builder
                            .build_int_compare(IntPredicate::SGE, l_value, r_value, "gte")
                    }
                }
            }
            Node::IdentifierOp(name, op, node) => {
                let value = self.visit(*node.clone());

                use IdentifierOp::*;
                match op {
                    Eq => {
                        let val_ptr_result = self.variables.get(name.as_str());
                        let val_ptr = match val_ptr_result {
                            None => self.builder.build_alloca(self.context.i32_type(), &name),
                            _ => *val_ptr_result.unwrap(),
                        };
                        self.variables.insert(name, val_ptr);
                        self.builder.build_store(val_ptr, value);

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
            Node::Fn(name, args, body) => {
                let i32_type = self.context.i32_type();
                let fn_type = i32_type.fn_type(
                    args.iter()
                        .map(|_| BasicTypeEnum::IntType(i32_type))
                        .collect::<Vec<BasicTypeEnum>>()
                        .as_slice(),
                    false,
                );
                let function = self.module.add_function(&name, fn_type, None);
                let block = self.context.append_basic_block(function, "body");
                let builder = self.context.create_builder();
                builder.position_at_end(block);

                let mut variables: HashMap<String, PointerValue<'ctx>> = HashMap::new();
                args.iter().enumerate().for_each(|(i, arg)| {
                    let value = function.get_nth_param(i as u32).unwrap().into_int_value();
                    let val_ptr = builder.build_alloca(self.context.i32_type(), &arg);
                    variables.insert(arg.clone(), val_ptr);
                    builder.build_store(val_ptr, value);
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

                self.functions.insert(name, function);

                i32_type.const_int(0, false)
            }
            Node::Return(node) => {
                let value = self.visit(*node);
                self.builder.build_return(Some(&value));
                self.context.i32_type().const_int(0, false)
            }
            Node::Call(name, args) => {
                let mut compiled_args: Vec<IntValue<'ctx>> = vec![];
                for arg in args {
                    compiled_args.push(self.visit(arg));
                }

                let fn_value = self.functions.get(&name).clone();
                match fn_value {
                    Some(function) => {
                        let mut argsv: Vec<BasicValueEnum<'ctx>> = compiled_args
                            .iter()
                            .by_ref()
                            .map(|&val| val.into())
                            .collect();

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
                            Some(value) => value.into_int_value(),
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
                self.context.i32_type().const_int(0, false)
            }
            _ => panic!("Unknown node: {:?}", node),
        }
    }
}
