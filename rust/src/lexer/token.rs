use std::fmt::Display;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub enum Token {
    Illegal(u8),
    Eof,

    // identifiers + literals
    Ident(String),  // add, foobar, x, y, etc
    Int(String),    // 123456
    String(String), // "vroom!"

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
        match self {
            Self::Ident(ident_str) => write!(f, "{}", ident_str),
            Self::Int(int_str) => write!(f, "{}", int_str),
            _ => write!(f, "{:?}", *self),
        }
    }
}

#[derive(PartialEq, PartialOrd, Debug, Ord, Eq)]
pub enum PrecedencePriority {
    Lowest = 0,
    Equals = 1,
    LessOrGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

pub trait Precedence {
    fn precedence(&self) -> PrecedencePriority;
}

impl Precedence for Token {
    fn precedence(&self) -> PrecedencePriority {
        match self {
            Self::Eq => PrecedencePriority::Equals,
            Self::Ne => PrecedencePriority::Equals,
            Self::Lt => PrecedencePriority::LessOrGreater,
            Self::Gt => PrecedencePriority::LessOrGreater,
            Self::Plus => PrecedencePriority::Sum,
            Self::Minus => PrecedencePriority::Sum,
            Self::Slash => PrecedencePriority::Product,
            Self::Asterisk => PrecedencePriority::Product,
            _ => PrecedencePriority::Lowest,
        }
    }
}
