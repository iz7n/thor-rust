use inkwell::{module::Linkage, types::BasicTypeEnum, values::BasicValueEnum};

use crate::{compiler::Codegen, TypeLiteral};

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    pub fn add_printf(&mut self) {
        let i32_type = self.context.i32_type();
        let str_type = self
            .context
            .i8_type()
            .ptr_type(inkwell::AddressSpace::Generic);
        let printf_args_type = vec![BasicTypeEnum::PointerType(str_type)];

        let printf_type = i32_type.fn_type(printf_args_type.as_slice(), true);

        let printf_fn = self
            .module
            .add_function("printf", printf_type, Some(Linkage::External));

        self.functions
            .insert("print".to_string(), (printf_fn, TypeLiteral::Int));
    }

    pub fn generate_printf_format_string(
        &self,
        compiled_args: Vec<BasicValueEnum<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let mut format_string = String::from("");

        for arg in compiled_args {
            let format_arg = match arg {
                BasicValueEnum::IntValue(_) => "%i ",
                BasicValueEnum::FloatValue(_) => "%f ",
                BasicValueEnum::PointerValue(_) => "%p ",
                _ => panic!("Invalid arg type for printf"),
            };

            format_string.push_str(format_arg);
        }
        format_string.push('\n');

        BasicValueEnum::PointerValue(
            self.builder
                .build_global_string_ptr(format_string.as_str(), "format_string")
                .as_pointer_value(),
        )
    }
}
