use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::time::Instant;

use inkwell::context::Context;

mod compiler;
mod lexer;
mod node;
mod parser;
mod token;

pub use compiler::Codegen;
pub use lexer::Lexer;
pub use node::{BinaryOp, IdentifierOp, Node, UnaryOp};
pub use parser::Parser;
pub use token::Token;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        1 => panic!("Too few many arguments passed"),
        2 => compile_file(&args[1], false),
        3 => match args[2].as_str() {
            "--log" => compile_file(&args[1], true),
            _ => panic!("Too many arguments passed"),
        },
        _ => panic!("Too many arguments passed"),
    };
}

fn compile_file(path: &str, log: bool) {
    let text = fs::read_to_string(path).unwrap();
    compile(text, path, false, log);
}

fn compile(text: String, filename: &str, repl: bool, log: bool) {
    let begin = Instant::now();

    let mut lexer = Lexer::new(text);
    let tokens = lexer.lex();
    if log {
        println!("tokens: {:?}", tokens);
    }

    let mut parser = Parser::new(tokens);
    let ast = if repl {
        parser.statement()
    } else {
        parser.parse()
    };
    if log {
        println!("ast: {:?}", ast);
    }

    let context = Context::create();
    let module = context.create_module("thor");
    let builder = context.create_builder();
    let mut codegen = Codegen {
        context: &context,
        module: &module,
        builder,
        functions: HashMap::new(),
        variables: HashMap::new(),
    };

    codegen.init(filename);
    codegen.generate_llvm_ir(ast);
    if log {
        println!(
            "LLVM IR: {}",
            codegen.module.print_to_string().to_str().unwrap()
        );
    }
    codegen.module.verify().expect("Errors were encountered");

    let parent_dir = Path::new(filename).parent().unwrap();
    let out_file = parent_dir.join("output.ll");
    codegen
        .module
        .print_to_file(out_file)
        .expect("Error writing compiled file");

    let end = Instant::now();
    println!(
        "compiled in {:.3}s",
        end.duration_since(begin).as_secs_f64()
    );
}
