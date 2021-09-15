use inkwell::values::BasicValueEnum;

use crate::{
    compiler::{Codegen, Function, Value},
    Type,
};

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    pub fn print(&mut self) {
        Function::new_llvm("print", "printf", &[Type::Str], true, Type::Int, self);
    }

    pub fn generate_printf_format_string(
        &self,
        compiled_args: &Vec<Value<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let mut format_string = String::from("");

        for arg in compiled_args {
            let format_arg = match arg {
                Value::Int(_) => "%i ",
                Value::Float(_) => "%f ",
                Value::Bool(_) => "%i ",
                Value::Str(_) => "%s ",
                Value::Char(_) => "%c ",
                Value::Array(_, _, _) => "%p ",
                Value::Void => "%p ",
            };

            format_string.push_str(format_arg);
        }
        format_string.push('\n');

        BasicValueEnum::PointerValue(
            self.builder
                .build_global_string_ptr(format_string.as_str(), "printf_format_string")
                .as_pointer_value(),
        )
    }
}
