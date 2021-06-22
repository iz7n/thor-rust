use inkwell::{
    context::Context,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FloatValue, IntValue, PointerValue},
    AddressSpace,
};

use crate::TypeLiteral;

pub enum Value<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
    Bool(IntValue<'ctx>),
    Str(PointerValue<'ctx>),
    Char(IntValue<'ctx>),
    Array(PointerValue<'ctx>, TypeLiteral, u32),
    Void,
}

impl<'ctx> Value<'ctx> {
    pub fn get_type(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        let int_type = context.i32_type();
        let float_type = context.f64_type();
        let bool_type = context.bool_type();
        let str_type = context.i8_type().ptr_type(AddressSpace::Generic);
        let char_type = context.i8_type();

        use Value::*;
        match self {
            Int(_) => BasicTypeEnum::IntType(int_type),
            Float(_) => BasicTypeEnum::FloatType(float_type),
            Bool(_) => BasicTypeEnum::IntType(bool_type),
            Str(_) => BasicTypeEnum::PointerType(str_type),
            Char(_) => BasicTypeEnum::IntType(char_type),
            Array(_, ty, size) => BasicTypeEnum::PointerType(
                match ty {
                    TypeLiteral::Int => int_type.array_type(*size),
                    TypeLiteral::Float => float_type.array_type(*size),
                    TypeLiteral::Bool => bool_type.array_type(*size),
                    TypeLiteral::Str => str_type.array_type(*size),
                    TypeLiteral::Char => char_type.array_type(*size),
                    TypeLiteral::Void => panic!("can't have a void array"),
                }
                .ptr_type(AddressSpace::Generic),
            ),
            Void => panic!("void isn't a valid argument type"),
        }
    }

    pub fn get_value(&self) -> BasicValueEnum<'ctx> {
        use Value::*;
        match self {
            Int(value) => BasicValueEnum::IntValue(*value),
            Float(value) => BasicValueEnum::FloatValue(*value),
            Bool(value) => BasicValueEnum::IntValue(*value),
            Str(value) => BasicValueEnum::PointerValue(*value),
            Char(value) => BasicValueEnum::IntValue(*value),
            Array(value, _, _) => BasicValueEnum::PointerValue(*value),
            Void => panic!("void isn't a valid value type"),
        }
    }
}
