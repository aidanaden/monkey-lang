use anyhow::Result;

use super::token::Token;

pub struct Lexer {
    pos: usize,
    read_pos: usize,
    char: u8,
    input: Vec<u8>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lex = Lexer {
            pos: 0,
            read_pos: 0,
            char: 0,
            input: input.into_bytes(),
        };
        lex.read_char();
        return lex;
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.char = 0;
        } else {
            self.char = self.input[self.read_pos]
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn peek_char(&self) -> Option<&u8> {
        if self.read_pos >= self.input.len() {
            return Some(&0);
        } else {
            return self.input.get(self.read_pos);
        }
    }

    fn skip_whitespace(&mut self) {
        while self.char.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn read_ident(&mut self) -> String {
        let pos = self.pos;
        while self.char.is_ascii_alphabetic() || self.char == b'_' {
            self.read_char();
        }
        return String::from_utf8_lossy(&self.input[pos..self.pos]).to_string();
    }

    fn read_digit(&mut self) -> String {
        let pos = self.pos;
        while self.char.is_ascii_digit() {
            self.read_char();
        }
        return String::from_utf8_lossy(&self.input[pos..self.pos]).to_string();
    }

    fn read_string(&mut self) -> String {
        let pos = self.pos;
        while self.char != b'"' {
            self.read_char();
        }
        return String::from_utf8_lossy(&self.input[pos..self.pos]).to_string();
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();
        let tok: Token;
        match self.char {
            b'=' => {
                if let Some(next_char) = self.peek_char() {
                    if *next_char == b'=' {
                        self.read_char();
                        tok = Token::Eq;
                    } else {
                        tok = Token::Assign
                    }
                } else {
                    tok = Token::Assign
                }
            }
            b'+' => tok = Token::Plus,
            b'-' => tok = Token::Minus,
            b'!' => {
                if let Some(next_char) = self.peek_char() {
                    if *next_char == b'=' {
                        self.read_char();
                        tok = Token::Ne;
                    } else {
                        tok = Token::Bang
                    }
                } else {
                    tok = Token::Bang
                }
            }
            b'*' => tok = Token::Asterisk,
            b'/' => tok = Token::Slash,
            b'<' => tok = Token::Lt,
            b'>' => tok = Token::Gt,
            b',' => tok = Token::Comma,
            b';' => tok = Token::Semicolon,
            b'(' => tok = Token::LParen,
            b')' => tok = Token::RParen,
            b'{' => tok = Token::LBrace,
            b'}' => tok = Token::RBrace,
            b'"' => {
                self.read_char();
                tok = Token::String(self.read_string())
            }
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let ident = self.read_ident();
                return Ok(match ident.as_str() {
                    "fn" => Token::Function,
                    "let" => Token::Let,
                    "true" => Token::True,
                    "false" => Token::False,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "return" => Token::Return,
                    _ => Token::Ident(ident),
                });
            }
            b'0'..=b'9' => return Ok(Token::Int(self.read_digit())),
            0 => tok = Token::Eof,
            _ => tok = Token::Illegal(self.char),
        }
        self.read_char();
        return Ok(tok);
    }
}

#[cfg(test)]
mod test {
    use anyhow::Result;

    use crate::lexer::token::Token;

    use super::Lexer;

    fn test_input_tokens(input: &str, tests: Vec<Token>) {
        let mut l = Lexer::new(input.to_string());
        tests.iter().for_each(|expected| {
            if let Ok(token) = l.next_token() {
                assert_eq!(
                    *expected, token,
                    "expected token {:?} vs generated token {:?}",
                    *expected, token
                );
            }
        })
    }

    #[test]
    fn test_sample_token() -> Result<()> {
        let input = "=+(){},;";
        let expected = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Eof,
        ];
        test_input_tokens(input, expected);
        return Ok(());
    }

    #[test]
    fn test_sample_code() -> Result<()> {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
	x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;

let st = \"hello!\";
";

        let expected: Vec<Token> = vec![
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::Semicolon,
            Token::RBrace,
            Token::Semicolon,
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::Int(String::from("5")),
            Token::Lt,
            Token::Int(String::from("10")),
            Token::Gt,
            Token::Int(String::from("5")),
            Token::Semicolon,
            Token::If,
            Token::LParen,
            Token::Int(String::from("5")),
            Token::Lt,
            Token::Int(String::from("10")),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::RBrace,
            Token::Int(String::from("10")),
            Token::Eq,
            Token::Int(String::from("10")),
            Token::Semicolon,
            Token::Int(String::from("10")),
            Token::Ne,
            Token::Int(String::from("9")),
            Token::Semicolon,
            // string test
            Token::Let,
            Token::Ident(String::from("st")),
            Token::Assign,
            Token::String(String::from("hello!")),
            Token::Semicolon,
            Token::Eof,
        ];

        test_input_tokens(input, expected);
        return Ok(());
    }
}
