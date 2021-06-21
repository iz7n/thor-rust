use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    values::{FunctionValue, PointerValue},
};

use crate::{compiler::Value, TypeLiteral};

pub struct Scope<'a, 'ctx> {
    pub variables: HashMap<String, (PointerValue<'ctx>, TypeLiteral)>,
    functions: HashMap<String, (FunctionValue<'ctx>, TypeLiteral)>,
    parent: Option<&'a Scope<'a, 'ctx>>,
}

impl<'a, 'ctx> Scope<'a, 'ctx> {
    pub fn new(parent: Option<&'a Scope<'a, 'ctx>>) -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
            parent,
        }
    }

    pub fn get(&self, name: &str, builder: &Builder<'ctx>) -> Value<'ctx> {
        match self.variables.get(name) {
            Some((ptr, literal)) => match literal {
                TypeLiteral::Int => {
                    let value = builder.build_load(*ptr, &name);
                    Value::Int(value.into_int_value())
                }
                TypeLiteral::Float => {
                    let value = builder.build_load(*ptr, &name);
                    Value::Float(value.into_float_value())
                }
                TypeLiteral::Bool => {
                    let value = builder.build_load(*ptr, &name);
                    Value::Bool(value.into_int_value())
                }
                TypeLiteral::Str => Value::Str(*ptr),
            },
            None => match self.parent {
                Some(parent) => parent.get(name, builder),
                None => panic!("{} is not defined", name),
            },
        }
    }

    pub fn set(
        &mut self,
        name: String,
        value: Value<'ctx>,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
    ) -> Value<'ctx> {
        let val_ptr_result = self.variables.get(&name);
        match value {
            Value::Int(value) => {
                let val_ptr = match val_ptr_result {
                    None => builder.build_alloca(context.i32_type(), &name),
                    Some((ptr, _)) => *ptr,
                };
                self.variables.insert(name, (val_ptr, TypeLiteral::Int));
                builder.build_store(val_ptr, value);
            }
            Value::Float(value) => {
                let val_ptr = match val_ptr_result {
                    None => builder.build_alloca(context.f64_type(), &name),
                    Some((ptr, _)) => *ptr,
                };
                self.variables.insert(name, (val_ptr, TypeLiteral::Float));
                builder.build_store(val_ptr, value);
            }
            Value::Bool(value) => {
                let val_ptr = match val_ptr_result {
                    None => builder.build_alloca(context.bool_type(), &name),
                    Some((ptr, _)) => *ptr,
                };
                self.variables.insert(name, (val_ptr, TypeLiteral::Bool));
                builder.build_store(val_ptr, value);
            }
            Value::Str(value) => {
                self.variables.insert(name, (value, TypeLiteral::Str));
            }
        };

        value
    }

    pub fn add_function(
        &mut self,
        name: String,
        function: FunctionValue<'ctx>,
        return_type: TypeLiteral,
    ) {
        self.functions.insert(name, (function, return_type));
    }

    pub fn get_function(&self, name: &str) -> &(FunctionValue<'ctx>, TypeLiteral) {
        match self.functions.get(name) {
            Some(value) => value,
            None => match self.parent {
                Some(parent) => parent.get_function(name),
                None => panic!("function {} is not defined", name),
            },
        }
    }
}
