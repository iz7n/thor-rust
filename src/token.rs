use std::fmt;

#[derive(Clone, PartialEq)]
pub enum Token {
    Int(i32),
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
    For,
    Fn,
    Return,
    Newline,
    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt(&self, f)
    }
}
impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt(&self, f)
    }
}

fn fmt(token: &Token, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    use Token::*;
    match token {
        Int(value) => write!(f, "'{}'", value),
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
        For => write!(f, "'for'"),
        Fn => write!(f, "'fn'"),
        Return => write!(f, "'return'"),
        Newline => write!(f, "'\\n'"),
        EOF => write!(f, "<eof>"),
    }
}
