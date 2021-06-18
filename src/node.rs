use std::fmt;

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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    Int(i32),
    Identifier(String),
    Unary(UnaryOp, Box<Node>),
    Binary(Box<Node>, BinaryOp, Box<Node>),
    IdentifierOp(String, IdentifierOp, Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    For(String, Box<Node>, Box<Node>),
    Fn(String, Vec<String>, Box<Node>),
    Return(Box<Node>),
    Call(String, Vec<Node>),
    Statements(Vec<Node>),
    EOF,
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt(&self, f)
    }
}

fn fmt(node: &Node, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match node {
        Node::Int(value) => write!(f, "{}", value),
        Node::Identifier(name) => write!(f, "{}", name),
        Node::Unary(op, node) => {
            use UnaryOp::*;
            match op {
                Pos => write!(f, "(+{})", *node),
                Neg => write!(f, "(-{})", *node),
                Not => write!(f, "not {}", *node),
            }
        }
        Node::Binary(left, op, right) => {
            use BinaryOp::*;
            match op {
                Add => write!(f, "({} + {})", *left, *right),
                Sub => write!(f, "({} - {})", *left, *right),
                Mul => write!(f, "({} * {})", *left, *right),
                Div => write!(f, "({} / {})", *left, *right),
                And => write!(f, "({} and {})", *left, *right),
                Or => write!(f, "({} or {})", *left, *right),
                EqEq => write!(f, "({} == {})", *left, *right),
                Neq => write!(f, "({} != {})", *left, *right),
                Lt => write!(f, "({} < {})", *left, *right),
                Lte => write!(f, "({} <= {})", *left, *right),
                Gt => write!(f, "({} > {})", *left, *right),
                Gte => write!(f, "({} >= {})", *left, *right),
            }
        }
        Node::IdentifierOp(name, op, node) => {
            use IdentifierOp::*;
            match op {
                Eq => write!(f, "({} = {})", name, *node),
                Add => write!(f, "({} + {})", name, *node),
                Sub => write!(f, "({} - {})", name, *node),
                Mul => write!(f, "({} * {})", name, *node),
                Div => write!(f, "({} / {})", name, *node),
            }
        }
        Node::If(condition, body, else_case) => match else_case {
            Some(case) => write!(f, "if {} then {} else {}", *condition, *body, *case),
            _ => write!(f, "if {} then {}", *condition, body),
        },
        Node::For(identifier, iterable, body) => {
            write!(f, "for {} in {}: {}", identifier, *iterable, *body)
        }
        Node::Fn(name, args, body) => write!(f, "fn {} ({:?}) {}", name, args, *body),
        Node::Return(node) => write!(f, "(return {})", *node),
        Node::Call(name, args) => write!(f, "{}({:?})", name, args),
        Node::Statements(nodes) => write!(f, "{:?}", nodes),
        Node::EOF => write!(f, "<eof>"),
    }
}
