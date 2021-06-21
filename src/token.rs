use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeLiteral {
    Int,
    Float,
    Bool,
    Str,
    Char,
    Void,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Int(i32),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
    Type(TypeLiteral),
    Identifier(String),
    Eq,
    Add,
    AddEq,
    Sub,
    SubEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    In,
    Not,
    And,
    Or,
    EqEq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Comma,
    Colon,
    If,
    Else,
    While,
    For,
    Fn,
    Return,
    Newline,
    EOF,
}

impl fmt::Display for TypeLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeLiteral::*;
        match self {
            Int => write!(f, "int"),
            Float => write!(f, "float"),
            Bool => write!(f, "bool"),
            Str => write!(f, "str"),
            Char => write!(f, "char"),
            Void => write!(f, "void"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Int(value) => write!(f, "{}", value),
            Float(value) => write!(f, "{}f", value),
            Bool(value) => write!(f, "{}", value),
            Str(value) => write!(f, "\"{}\"", value),
            Char(value) => write!(f, "'{}'", value),
            Type(literal) => write!(f, "{}", literal),
            Identifier(name) => write!(f, "{}", name),
            Eq => write!(f, "'='"),
            Add => write!(f, "'+'"),
            AddEq => write!(f, "'+='"),
            Sub => write!(f, "'-'"),
            SubEq => write!(f, "'-='"),
            Mul => write!(f, "'*'"),
            MulEq => write!(f, "'*='"),
            Div => write!(f, "'/'"),
            DivEq => write!(f, "'/='"),
            LParen => write!(f, "'('"),
            RParen => write!(f, "')'"),
            LBrace => write!(f, "'{{'"),
            RBrace => write!(f, "'}}'"),
            LBracket => write!(f, "'['"),
            RBracket => write!(f, "']'"),
            In => write!(f, "'in'"),
            Not => write!(f, "'not'"),
            And => write!(f, "'and'"),
            Or => write!(f, "'or'"),
            EqEq => write!(f, "'=='"),
            Neq => write!(f, "'!='"),
            Lt => write!(f, "'<'"),
            Lte => write!(f, "'<='"),
            Gt => write!(f, "'>'"),
            Gte => write!(f, "'>='"),
            Comma => write!(f, "','"),
            Colon => write!(f, "':'"),
            If => write!(f, "'if'"),
            Else => write!(f, "'else'"),
            While => write!(f, "'while'"),
            For => write!(f, "'for'"),
            Fn => write!(f, "'fn'"),
            Return => write!(f, "'return'"),
            Newline => write!(f, "'\\n'"),
            EOF => write!(f, "<eof>"),
        }
    }
}
