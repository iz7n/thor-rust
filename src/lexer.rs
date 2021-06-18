use crate::Token;
use Token::*;

pub struct Lexer {
    text: String,
    index: usize,
    current_char: char,
}

impl Lexer {
    pub fn new(text: String) -> Self {
        Self {
            index: 0,
            current_char: text.chars().nth(0).unwrap(),
            text,
        }
    }

    fn advance(&mut self) -> Token {
        self.index += 1;
        let next = self.text.chars().nth(self.index);
        self.current_char = match next {
            Some(c) => c,
            _ => '\0',
        };
        EOF
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        let mut token = self.next_token();
        while token != EOF {
            tokens.push(token);
            token = self.next_token();
        }
        tokens.push(token);
        tokens
    }

    pub fn next_token(&mut self) -> Token {
        while self.current_char != '\0' {
            let token = match self.current_char {
                ' ' | '\t' | '\r' => self.advance(),
                '0'..='9' => self.number(),
                'a'..='z' | 'A'..='Z' | '_' | 'Α'..='ω' | '∞' => self.word(),
                '=' => {
                    self.advance();
                    match self.current_char {
                        '=' => {
                            self.advance();
                            EqEq
                        }
                        _ => Eq,
                    }
                }
                '+' => {
                    self.advance();
                    match self.current_char {
                        '=' => {
                            self.advance();
                            AddEq
                        }
                        _ => Add,
                    }
                }
                '-' => {
                    self.advance();
                    match self.current_char {
                        '=' => {
                            self.advance();
                            SubEq
                        }
                        _ => Sub,
                    }
                }
                '*' => {
                    self.advance();
                    match self.current_char {
                        '=' => {
                            self.advance();
                            MulEq
                        }
                        _ => Mul,
                    }
                }
                '/' => {
                    self.advance();
                    match self.current_char {
                        '=' => {
                            self.advance();
                            DivEq
                        }
                        _ => Div,
                    }
                }
                '<' => {
                    self.advance();
                    match self.current_char {
                        '=' => {
                            self.advance();
                            Lte
                        }
                        _ => Lt,
                    }
                }
                '>' => {
                    self.advance();
                    match self.current_char {
                        '=' => {
                            self.advance();
                            Gte
                        }
                        _ => Gt,
                    }
                }
                '(' => {
                    self.advance();
                    LParen
                }
                ')' => {
                    self.advance();
                    RParen
                }
                '{' => {
                    self.advance();
                    LBrace
                }
                '}' => {
                    self.advance();
                    RBrace
                }
                ',' => {
                    self.advance();
                    Comma
                }
                ':' => {
                    self.advance();
                    Colon
                }
                '\n' | ';' => {
                    self.advance();
                    Newline
                }
                '\0' => EOF,
                _ => panic!("Illegal character: '{}'", self.current_char),
            };
            if token != EOF {
                return token;
            }
        }
        EOF
    }

    fn number(&mut self) -> Token {
        let mut num_str: String = self.current_char.to_string();
        self.advance();

        while "0123456789".contains(self.current_char) {
            num_str.push(self.current_char);
            self.advance();
        }

        Int(num_str.parse::<i32>().unwrap())
    }

    fn word(&mut self) -> Token {
        let mut word: String = self.current_char.to_string();
        self.advance();

        while self.current_char != '\0' {
            match self.current_char {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | 'Α'..='ω' | '∞' => {
                    word.push(self.current_char);
                    self.advance();
                }
                _ => break,
            };
        }

        match word.as_str() {
            "not" => Not,
            "and" => And,
            "or" => Or,
            "if" => If,
            "else" => Else,
            "for" => For,
            "in" => In,
            "fn" => Fn,
            "return" => Return,
            _ => Identifier(word),
        }
    }
}
