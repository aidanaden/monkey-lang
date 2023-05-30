use std::io::{self, Write};

use crate::lexer::{lexer::Lexer, token::Token};

pub fn start() {
    loop {
        print!(">> ");
        let _ = io::stdout().flush();
        let mut lines = io::stdin().lines();
        if let Some(Ok(line)) = lines.next() {
            let mut lex = Lexer::new(line);
            while let Ok(tok) = lex.next_token() {
                println!("{}", tok);
                if tok == Token::Eof {
                    break;
                }
            }
        }
    }
}
