use std::fs;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use inkwell::{
    context::Context,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    OptimizationLevel,
};

mod compiler;
mod lexer;
mod node;
mod parser;
mod token;

use compiler::Codegen;
pub use lexer::Lexer;
pub use node::{BinaryOp, IdentifierOp, Node, UnaryOp};
pub use parser::Parser;
pub use token::{Token, TypeLiteral};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        1 => panic!("Too few many arguments passed"),
        2 => compile_file(&args[1], "output.o", false),
        3 => match args[2].as_str() {
            "--log" => compile_file(&args[1], "output.o", true),
            out_filename => compile_file(&args[1], out_filename, false),
        },
        4 => match args[3].as_str() {
            "--log" => compile_file(&args[1], &args[2], true),
            _ => panic!("Too many arguments passed"),
        },
        _ => panic!("Too many arguments passed"),
    };
}

fn compile_file(path: &str, out_filename: &str, log: bool) {
    let text = fs::read_to_string(path).unwrap();
    compile(text, path, out_filename, log);
}

fn compile(text: String, filename: &str, out_filename: &str, log: bool) {
    let begin = Instant::now();

    let mut lexer = Lexer::new(text);
    let tokens = lexer.lex();
    if log {
        println!("tokens:");
        for token in &tokens {
            print!(" {}", token);
        }
        print!("\n");
    }

    let mut parser = Parser::new(tokens);
    let ast = parser.parse();
    if log {
        println!("ast: {}", ast);
    }

    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();
    let mut codegen = Codegen::new(filename, &context, &module, builder);
    codegen.generate_llvm_ir(ast);
    match codegen.module.verify() {
        Ok(_) => {}
        Err(err) => eprintln!("{}", err),
    };
    codegen
        .module
        .print_to_file(format!("{}.ll", out_filename))
        .unwrap();

    Target::initialize_all(&InitializationConfig::default());
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).expect("couldn't create target from target triple");
    let target_machine = target
        .create_target_machine(
            &triple,
            "generic",
            "",
            OptimizationLevel::None,
            RelocMode::Default,
            CodeModel::Default,
        )
        .expect("couldn't create target machine");
    let object_filename = &format!("{}.o", out_filename);
    target_machine
        .write_to_file(
            &codegen.module,
            FileType::Object,
            Path::new(object_filename),
        )
        .expect("couldn't write module to file");

    let args = [object_filename, "-o", out_filename];
    Command::new("clang")
        .args(args)
        .output()
        .expect("failed to execute linker");

    let end = Instant::now();
    println!(
        "compiled in {:.3}s",
        end.duration_since(begin).as_secs_f64()
    );
}
