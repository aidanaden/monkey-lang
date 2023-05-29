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

    // keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
