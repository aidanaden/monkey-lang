enum Token {
    ILLEGAL,
    EOF,

    // identifiers + literals
    IDENT, // add, foobar, x, y, etc
    INT,   // 123456

    // operators
    ASSIGN,
    PLUS,

    // delimiters
    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    // keywords
    FUNCTION,
    LET,
}
