use std::fmt;

use inkwell::{context::Context, types::BasicTypeEnum, AddressSpace};

use crate::TypeLiteral;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
    Char,
    Array(TypeLiteral, u32),
    Void,
}

impl Type {
    pub fn get_type<'ctx>(&self, context: &'ctx Context) -> BasicTypeEnum<'ctx> {
        let int_type = context.i32_type();
        let float_type = context.f64_type();
        let bool_type = context.bool_type();
        let str_type = context.i8_type().ptr_type(AddressSpace::Generic);
        let char_type = context.i8_type();

        match self {
            Type::Int => BasicTypeEnum::IntType(int_type),
            Type::Float => BasicTypeEnum::FloatType(float_type),
            Type::Bool => BasicTypeEnum::IntType(bool_type),
            Type::Str => BasicTypeEnum::PointerType(str_type),
            Type::Char => BasicTypeEnum::IntType(char_type),
            Type::Array(ty, size) => BasicTypeEnum::PointerType(
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
            Type::Void => panic!("void isn't a valid argument type"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Bool => write!(f, "bool"),
            Str => write!(f, "str"),
            Char => write!(f, "char"),
            Array(ty, size) => write!(f, "{}[{}]", ty, size),
            Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    EqEq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum IdentifierOp {
    Eq,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Int(u32),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Array(Vec<Node>),
    Type(Type),
    Cast(Type, Box<Node>),
    Identifier(String),
    Unary(UnaryOp, Box<Node>),
    Binary(Box<Node>, BinaryOp, Box<Node>),
    IdentifierOp(String, IdentifierOp, Box<Node>),
    Index(Box<Node>, u32),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    While(Box<Node>, Box<Node>),
    For(String, Box<Node>, Box<Node>),
    Fn(String, Vec<(String, Type)>, Type, Box<Node>),
    Return(Box<Node>),
    Call(String, Vec<Node>),
    Statements(Vec<Node>),
    EOF,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Int(value) => write!(f, "{}", value),
            Node::Float(value) => write!(f, "{}f", value),
            Node::Bool(value) => write!(f, "{}", value),
            Node::Str(value) => write!(f, "\"{}\"", value),
            Node::Char(value) => write!(f, "'{}'", value),
            Node::Array(nodes) => write!(
                f,
                "[{}]",
                nodes
                    .iter()
                    .map(|node| format!("{}", node))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Node::Type(ty) => write!(f, "{}", ty),
            Node::Cast(literal, node) => write!(f, "{}({})", literal, node),
            Node::Identifier(name) => write!(f, "{}", name),
            Node::Unary(op, node) => {
                use UnaryOp::*;
                match op {
                    Pos => write!(f, "(+{})", node),
                    Neg => write!(f, "(-{})", node),
                    Not => write!(f, "(not {})", node),
                }
            }
            Node::Binary(left, op, right) => {
                use BinaryOp::*;
                match op {
                    Add => write!(f, "({} + {})", left, right),
                    Sub => write!(f, "({} - {})", left, right),
                    Mul => write!(f, "({} * {})", left, right),
                    Div => write!(f, "({} / {})", left, right),
                    Rem => write!(f, "({} % {})", left, right),
                    And => write!(f, "({} and {})", left, right),
                    Or => write!(f, "({} or {})", left, right),
                    EqEq => write!(f, "({} == {})", left, right),
                    Neq => write!(f, "({} != {})", left, right),
                    Lt => write!(f, "({} < {})", left, right),
                    Lte => write!(f, "({} <= {})", left, right),
                    Gt => write!(f, "({} > {})", left, right),
                    Gte => write!(f, "({} >= {})", left, right),
                }
            }
            Node::IdentifierOp(name, op, node) => {
                use IdentifierOp::*;
                match op {
                    Eq => write!(f, "({} = {})", name, node),
                    Add => write!(f, "({} += {})", name, node),
                    Sub => write!(f, "({} -= {})", name, node),
                    Mul => write!(f, "({} *= {})", name, node),
                    Div => write!(f, "({} /= {})", name, node),
                    Rem => write!(f, "({} %= {})", name, node),
                }
            }
            Node::Index(node, index) => write!(f, "{}[{}]", node, index),
            Node::If(condition, body, else_case) => match else_case {
                Some(case) => write!(f, "if {}: {} else: {}", condition, body, case),
                _ => write!(f, "if {}: {}", condition, body),
            },
            Node::While(condition, body) => write!(f, "while {}: {}", condition, body),
            Node::For(identifier, iterable, body) => {
                write!(f, "for {} in {}: {}", identifier, iterable, body)
            }
            Node::Fn(name, args, return_type, body) => {
                write!(
                    f,
                    "fn {} ({}): {} -> {}",
                    name,
                    args.iter()
                        .map(|(name, ty)| format!("{}: {}", name, ty))
                        .collect::<Vec<String>>()
                        .join(", "),
                    return_type,
                    body
                )
            }
            Node::Return(node) => write!(f, "(return {})", node),
            Node::Call(name, args) => write!(
                f,
                "{}({})",
                name,
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Node::Statements(nodes) => write!(
                f,
                "[\n  {}\n]",
                nodes
                    .iter()
                    .map(|node| format!("{}", node))
                    .collect::<Vec<String>>()
                    .join("\n  ")
            ),
            Node::EOF => write!(f, "<eof>"),
        }
    }
}
