use inkwell::{
    builder::Builder,
    module::Linkage,
    types::BasicTypeEnum,
    values::{BasicValueEnum, FunctionValue},
    AddressSpace,
};

use crate::{
    compiler::{Codegen, Value},
    Type,
};

pub struct Function<'ctx> {
    pub value: FunctionValue<'ctx>,
    pub return_type: Type,
}

impl<'ctx> Function<'ctx> {
    pub fn new_user<'a>(
        name: &str,
        arg_types: Vec<Type>,
        return_type: Type,
        codegen: &mut Codegen<'a, 'ctx>,
    ) -> Self {
        Function::new(name, &arg_types, false, return_type, None, codegen)
    }

    pub fn new_llvm<'a>(
        name: &str,
        llvm_name: &str,
        arg_types: &[Type],
        var_args: bool,
        return_type: Type,
        codegen: &mut Codegen<'a, 'ctx>,
    ) {
        let function = Function::new(
            llvm_name,
            arg_types,
            var_args,
            return_type,
            Some(Linkage::External),
            codegen,
        );
        codegen.scope.add_function(name.to_string(), function);
    }

    pub fn new<'a>(
        name: &str,
        arg_types: &[Type],
        var_args: bool,
        return_type: Type,
        linkage: Option<Linkage>,
        codegen: &mut Codegen<'a, 'ctx>,
    ) -> Self {
        let context = codegen.context;
        let arg_types = arg_types
            .iter()
            .map(|ty| ty.get_type(context))
            .collect::<Vec<BasicTypeEnum<'ctx>>>();
        let fn_type = match return_type {
            Type::Int => context.i32_type().fn_type(&arg_types, var_args),
            Type::Float => context.f32_type().fn_type(&arg_types, var_args),
            Type::Char => context.i8_type().fn_type(&arg_types, var_args),
            Type::Str => context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .fn_type(&arg_types, var_args),
            Type::Void => context.void_type().fn_type(&arg_types, var_args),
            _ => unimplemented!(),
        };
        let fn_value = codegen.module.add_function(name, fn_type, linkage);
        Self {
            value: fn_value,
            return_type,
        }
    }

    pub fn call(&self, values: Vec<Value<'ctx>>, builder: &Builder<'ctx>) -> BasicValueEnum<'ctx> {
        builder
            .build_call(
                self.value,
                values
                    .iter()
                    .map(|value| value.get_value())
                    .collect::<Vec<BasicValueEnum>>()
                    .as_slice(),
                "call",
            )
            .try_as_basic_value()
            .left()
            .unwrap()
    }
}
