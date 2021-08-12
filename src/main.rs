use std::fs;
use std::time::Instant;

mod interpreter;
mod lexer;
mod node;
mod parser;
mod token;

use interpreter::Interpreter;
pub use lexer::Lexer;
pub use node::{BinaryOp, IdentifierOp, Node, UnaryOp};
pub use parser::Parser;
pub use token::Token;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    match args.len() {
        1 => panic!("Too few many arguments passed"),
        2 => run_file(&args[1], false),
        3 => match args[2].as_str() {
            "--log" => run_file(&args[1], true),
            _ => panic!("Too many arguments passed"),
        },
        _ => panic!("Too many arguments passed"),
    };
}

fn run_file(path: &str, log: bool) {
    let text = fs::read_to_string(path).unwrap();
    compile(text, path, log);
}

fn compile(text: String, filename: &str, log: bool) {
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

    let mut interpreter = Interpreter::new(filename.to_string());
    interpreter.interpret(ast);

    let end = Instant::now();
    println!("ran in {:.3}s", end.duration_since(begin).as_secs_f64());
}
