use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(u8),
    Eof,

    // identifiers + literals
    Ident(String), // add, foobar, x, y, etc
    Int(String),   // 123456

    // operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,
    Eq,
    Ne,

    // delimiters
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Caret,

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v = self.clone();
        match v {
            Self::Ident(s) => write!(f, "Type: Ident, value: '{}'", *s),
            Self::Illegal(c) => write!(f, "Type: Illegal, value: {:?}", *c as char),
            Self::Int(i) => write!(f, "Type: Int, value: '{}'", i),
            _ => write!(f, "Type: {:?}", *self),
        }
    }
}
