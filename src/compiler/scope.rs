use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    values::{FunctionValue, PointerValue},
};

use crate::{compiler::Value, Type};

pub struct Scope<'a, 'ctx> {
    pub variables: HashMap<String, (PointerValue<'ctx>, Type)>,
    functions: HashMap<String, (FunctionValue<'ctx>, Type)>,
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

    pub fn get_ptr(&self, name: &str, builder: &Builder<'ctx>) -> PointerValue<'ctx> {
        match self.variables.get(name) {
            Some((ptr, _)) => *ptr,
            None => match self.parent {
                Some(parent) => parent.get_ptr(name, builder),
                None => panic!("{} is not defined", name),
            },
        }
    }

    pub fn get(&self, name: &str, builder: &Builder<'ctx>) -> Value<'ctx> {
        match self.variables.get(name) {
            Some((ptr, ty)) => match ty {
                Type::Int => {
                    let value = builder.build_load(*ptr, &name);
                    Value::Int(value.into_int_value())
                }
                Type::Float => {
                    let value = builder.build_load(*ptr, &name);
                    Value::Float(value.into_float_value())
                }
                Type::Bool => {
                    let value = builder.build_load(*ptr, &name);
                    Value::Bool(value.into_int_value())
                }
                Type::Str => Value::Str(*ptr),
                Type::Char => {
                    let value = builder.build_load(*ptr, &name);
                    Value::Char(value.into_int_value())
                }
                Type::Array(ty, size) => Value::Array(*ptr, *ty, *size),
                Type::Void => panic!("void isn't a valid variable type"),
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
        let int_type = context.i32_type();
        let float_type = context.f64_type();
        let bool_type = context.bool_type();
        let char_type = context.i8_type();

        let val_ptr_result = self.variables.get(&name);
        match value {
            Value::Int(value) => {
                let val_ptr = match val_ptr_result {
                    None => builder.build_alloca(int_type, &name),
                    Some((ptr, _)) => *ptr,
                };
                self.variables.insert(name, (val_ptr, Type::Int));
                builder.build_store(val_ptr, value);
            }
            Value::Float(value) => {
                let val_ptr = match val_ptr_result {
                    None => builder.build_alloca(float_type, &name),
                    Some((ptr, _)) => *ptr,
                };
                self.variables.insert(name, (val_ptr, Type::Float));
                builder.build_store(val_ptr, value);
            }
            Value::Bool(value) => {
                let val_ptr = match val_ptr_result {
                    None => builder.build_alloca(bool_type, &name),
                    Some((ptr, _)) => *ptr,
                };
                self.variables.insert(name, (val_ptr, Type::Bool));
                builder.build_store(val_ptr, value);
            }
            Value::Str(value) => {
                self.variables.insert(name, (value, Type::Str));
            }
            Value::Char(value) => {
                let val_ptr = match val_ptr_result {
                    None => builder.build_alloca(char_type, &name),
                    Some((ptr, _)) => *ptr,
                };
                self.variables.insert(name, (val_ptr, Type::Char));
                builder.build_store(val_ptr, value);
            }
            Value::Array(value, ty, size) => {
                self.variables.insert(name, (value, Type::Array(ty, size)));
            }
            Value::Void => panic!("void isn't a valid type"),
        };

        value
    }

    pub fn add_function(&mut self, name: String, function: FunctionValue<'ctx>, return_type: Type) {
        self.functions.insert(name, (function, return_type));
    }

    pub fn get_function(&self, name: &str) -> &(FunctionValue<'ctx>, Type) {
        match self.functions.get(name) {
            Some(value) => value,
            None => match self.parent {
                Some(parent) => parent.get_function(name),
                None => panic!("function {} is not defined", name),
            },
        }
    }
}
