use crate::{BinaryOp, IdentifierOp, Node, Token, TypeLiteral, UnaryOp};

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    token: Token,
}

use Token::*;

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            token: tokens[0].clone(),
            tokens,
            index: 0,
        }
    }

    fn advance(&mut self) {
        self.index += 1;
        let next = self.tokens.get(self.index);
        self.token = match next {
            Some(token) => token.clone(),
            _ => EOF,
        };
    }

    fn next_token(&mut self) -> Token {
        let next = self.tokens.get(self.index + 1);
        match next {
            Some(token) => token.clone(),
            _ => EOF,
        }
    }

    fn skip_newlines(&mut self) -> u32 {
        let mut newlines = 0u32;
        while self.token == Newline {
            self.advance();
            newlines += 1;
        }
        newlines
    }

    pub fn parse(&mut self) -> Node {
        self.statements()
    }

    fn statements(&mut self) -> Node {
        let mut statements: Vec<Node> = vec![];
        self.skip_newlines();

        statements.push(self.statement());

        let mut more_statements = true;

        loop {
            let newlines = self.skip_newlines();
            if newlines == 0 {
                more_statements = false;
            }

            if !more_statements || self.token == RBrace {
                break;
            }

            let statement = self.statement();
            if statement == Node::EOF {
                more_statements = false;
                continue;
            }
            statements.push(statement);
        }

        Node::Statements(statements)
    }

    pub fn statement(&mut self) -> Node {
        match self.token {
            Return => {
                self.advance();
                Node::Return(Box::new(self.expr()))
            }
            _ => self.expr(),
        }
    }

    fn expr(&mut self) -> Node {
        match self.token.clone() {
            Identifier(name) => match self.next_token() {
                Eq => {
                    self.advance();
                    self.advance();
                    Node::IdentifierOp(name, IdentifierOp::Eq, Box::new(self.or_expr()))
                }
                AddEq => {
                    self.advance();
                    self.advance();
                    Node::IdentifierOp(name, IdentifierOp::Add, Box::new(self.or_expr()))
                }
                SubEq => {
                    self.advance();
                    self.advance();
                    Node::IdentifierOp(name, IdentifierOp::Sub, Box::new(self.or_expr()))
                }
                MulEq => {
                    self.advance();
                    self.advance();
                    Node::IdentifierOp(name, IdentifierOp::Mul, Box::new(self.or_expr()))
                }
                DivEq => {
                    self.advance();
                    self.advance();
                    Node::IdentifierOp(name, IdentifierOp::Div, Box::new(self.or_expr()))
                }
                _ => self.or_expr(),
            },
            _ => self.or_expr(),
        }
    }

    fn or_expr(&mut self) -> Node {
        let result = self.and_expr();

        match self.token {
            Or => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Or, Box::new(self.or_expr()))
            }
            _ => result,
        }
    }

    fn and_expr(&mut self) -> Node {
        let result = self.not_expr();

        match self.token {
            And => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::And, Box::new(self.and_expr()))
            }
            _ => result,
        }
    }

    fn not_expr(&mut self) -> Node {
        match self.token {
            Not => {
                self.advance();
                Node::Unary(UnaryOp::Not, Box::new(self.not_expr()))
            }
            _ => self.comp_expr(),
        }
    }

    fn comp_expr(&mut self) -> Node {
        let result = self.arith_expr();

        match self.token {
            EqEq => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::EqEq, Box::new(self.comp_expr()))
            }
            Neq => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Neq, Box::new(self.comp_expr()))
            }
            Lt => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Lt, Box::new(self.comp_expr()))
            }
            Lte => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Lte, Box::new(self.comp_expr()))
            }
            Gt => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Gt, Box::new(self.comp_expr()))
            }
            Gte => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Gte, Box::new(self.comp_expr()))
            }

            _ => result,
        }
    }

    fn arith_expr(&mut self) -> Node {
        let result = self.term();

        match self.token {
            Add => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Add, Box::new(self.arith_expr()))
            }
            Sub => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Sub, Box::new(self.arith_expr()))
            }
            _ => result,
        }
    }

    fn term(&mut self) -> Node {
        let result = self.factor();

        match self.token {
            Mul => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Mul, Box::new(self.term()))
            }
            Div => {
                self.advance();
                Node::Binary(Box::new(result), BinaryOp::Div, Box::new(self.term()))
            }
            _ => result,
        }
    }

    fn factor(&mut self) -> Node {
        match self.token {
            Add => {
                self.advance();
                Node::Unary(UnaryOp::Pos, Box::new(self.factor()))
            }
            Sub => {
                self.advance();
                Node::Unary(UnaryOp::Neg, Box::new(self.factor()))
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> Node {
        let result = self.atom();

        match self.token {
            LParen => {
                self.advance();

                match result {
                    Node::Identifier(name) => {
                        let args = self.list(RParen);
                        Node::Call(name, args)
                    }
                    Node::Type(literal) => {
                        let expr = self.expr();

                        if self.token != RParen {
                            panic!("expected ')'");
                        }
                        self.advance();

                        Node::Cast(literal, Box::new(expr))
                    }
                    _ => panic!("expected identifier or type"),
                }
            }
            _ => result,
        }
    }

    fn atom(&mut self) -> Node {
        match self.token.clone() {
            Int(value) => {
                self.advance();
                Node::Int(value)
            }
            Float(value) => {
                self.advance();
                Node::Float(value)
            }
            Bool(value) => {
                self.advance();
                Node::Bool(value)
            }
            Type(literal) => {
                self.advance();
                Node::Type(literal)
            }
            Identifier(name) => {
                self.advance();
                Node::Identifier(name)
            }
            LParen => {
                self.advance();
                let result = self.expr();

                if self.token != RParen {
                    panic!("expected ')'");
                }
                self.advance();

                result
            }
            If => self.if_expr(),
            For => self.for_expr(),
            Fn => self.fn_expr(),
            EOF => Node::EOF,
            _ => panic!("expected int, identifier, '(', 'if', 'for', or 'fn'"),
        }
    }

    fn if_expr(&mut self) -> Node {
        if self.token != If {
            panic!("expected 'if'");
        }
        self.advance();

        let condition = self.expr();

        let body = match self.token {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            _ => panic!("{}", "expected ':' or '{'"),
        };

        let node = Node::If(
            Box::new(condition),
            Box::new(body),
            match self.token {
                Else => Some(Box::new(self.else_expr())),
                _ => None,
            },
        );
        node
    }

    fn else_expr(&mut self) -> Node {
        if self.token != Else {
            panic!("expected 'else'");
        }
        self.advance();

        match self.token {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            If => self.if_expr(),
            _ => panic!("{}", "expected ':', '{', or 'if'"),
        }
    }

    fn for_expr(&mut self) -> Node {
        if self.token != For {
            panic!("expected 'for'");
        }
        self.advance();

        let identifier = match &self.token {
            Identifier(name) => {
                let n = name.clone();
                self.advance();
                n
            }
            _ => panic!("expected identifier"),
        };

        if self.token != In {
            panic!("expected 'in'");
        }
        self.advance();

        let iterable = self.expr();

        let body = match self.token {
            Colon => {
                self.advance();
                self.statement()
            }
            LBrace => self.block(),
            _ => panic!("{}", "expected ':' or '{'"),
        };

        Node::For(identifier, Box::new(iterable), Box::new(body))
    }

    fn fn_expr(&mut self) -> Node {
        if self.token != Fn {
            panic!("expected 'fn'");
        }
        self.advance();

        let name = match &self.token {
            Identifier(name) => name.clone(),
            _ => panic!("expected identifier"),
        };
        self.advance();

        if self.token != LParen {
            panic!("expected '('");
        }
        self.advance();

        let mut args: Vec<(String, TypeLiteral)> = vec![];

        while self.token != RParen {
            let name = match &self.token {
                Identifier(name) => name.clone(),
                _ => panic!("expected identifier"),
            };
            self.advance();

            if self.token != Colon {
                panic!("expected ':'");
            }
            self.advance();

            let literal = match &self.token {
                Type(literal) => literal.clone(),
                _ => panic!("expected a type"),
            };
            self.advance();

            match &self.token {
                Comma => self.advance(),
                RParen => {}
                _ => panic!("expected ',' or ')'"),
            };

            args.push((name, literal));
        }

        if self.token != RParen {
            panic!("expected '{}'", RParen);
        }
        self.advance();

        if self.token != Colon {
            panic!("expected ':'");
        }
        self.advance();

        let return_type = match &self.token {
            Type(literal) => literal.clone(),
            _ => panic!("expected type"),
        };
        self.advance();

        let body = match self.token {
            LBrace => self.block(),
            _ => panic!("{}", "expected '{'"),
        };

        Node::Fn(name.to_string(), args, return_type, Box::new(body))
    }

    fn list(&mut self, end: Token) -> Vec<Node> {
        let mut nodes: Vec<Node> = vec![];

        while self.token != end {
            nodes.push(self.expr());
            match &self.token {
                Comma => self.advance(),
                t if *t == end => {}
                _ => panic!("expected ',' or '{}'", end),
            };
        }

        if self.token != end {
            panic!("expected '{}'", end);
        }
        self.advance();

        nodes
    }

    fn block(&mut self) -> Node {
        if self.token != LBrace {
            panic!("{}", "expected '{'");
        }
        self.advance();

        let statements = self.statements();

        if self.token != RBrace {
            panic!("{}", "expected '}'");
        }
        self.advance();

        statements
    }
}
