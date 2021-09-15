use crate::{
    compiler::{Codegen, Function, Value},
    Type,
};

impl<'a, 'ctx> Codegen<'a, 'ctx> {
    pub fn math(&mut self) {
        self.add_var(
            "PI",
            Value::Float(self.float_type.const_float(std::f64::consts::PI)),
        );

        self.abs();
        self.round();
        self.floor();
        self.ceil();
        self.sqrt();
        self.sin();
        self.cos();
        self.tan();
    }

    fn abs(&mut self) {
        Function::new_llvm("abs", "llvm.abs.i32", &[Type::Int], false, Type::Int, self);
    }

    fn round(&mut self) {
        Function::new_llvm(
            "round",
            "llvm.round.f64",
            &[Type::Float],
            false,
            Type::Float,
            self,
        );
    }

    fn floor(&mut self) {
        Function::new_llvm(
            "floor",
            "llvm.floor.f64",
            &[Type::Float],
            false,
            Type::Float,
            self,
        );
    }

    fn ceil(&mut self) {
        Function::new_llvm(
            "ceil",
            "llvm.ceil.f64",
            &[Type::Float],
            false,
            Type::Float,
            self,
        );
    }

    fn sqrt(&mut self) {
        Function::new_llvm(
            "sqrt",
            "llvm.sqrt.f64",
            &[Type::Float],
            false,
            Type::Float,
            self,
        );
    }

    fn sin(&mut self) {
        Function::new_llvm(
            "sin",
            "llvm.sin.f64",
            &[Type::Float],
            false,
            Type::Float,
            self,
        );
    }

    fn cos(&mut self) {
        Function::new_llvm(
            "cos",
            "llvm.cos.f64",
            &[Type::Float],
            false,
            Type::Float,
            self,
        );
    }

    fn tan(&mut self) {
        Function::new_llvm(
            "tan",
            "llvm.tan.f64",
            &[Type::Float],
            false,
            Type::Float,
            self,
        );
    }
}
