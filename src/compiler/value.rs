use inkwell::values::{FloatValue, IntValue, PointerValue};

pub enum Value<'ctx> {
    Int(IntValue<'ctx>),
    Float(FloatValue<'ctx>),
    Bool(IntValue<'ctx>),
    Str(PointerValue<'ctx>),
    Char(IntValue<'ctx>),
    Void,
}
