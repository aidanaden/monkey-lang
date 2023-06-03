use std::collections::HashMap;

use crate::lexer::{
    lexer,
    token::{self},
};

use super::ast::{Expression, Identifier, Program, Statement};

type prefix_parse_fn = fn() -> Expression;
type infix_parse_fn = fn(Expression) -> Expression;

pub struct Parser {
    lex: lexer::Lexer,
    errors: Vec<String>,
    curr_token: Option<token::Token>,
    peek_token: Option<token::Token>,

    prefix_parse_fns: HashMap<token::Token, prefix_parse_fn>,
    infix_parse_fns: HashMap<token::Token, infix_parse_fn>,
}

impl Parser {
    pub fn new(lex: lexer::Lexer) -> Parser {
        let mut p = Parser {
            lex: lex,
            errors: vec![],
            curr_token: None,
            peek_token: None,
            infix_parse_fns: HashMap::new(),
            prefix_parse_fns: HashMap::new(),
        };
        p.next_token();
        p.next_token();
        return p;
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        loop {
            if let Some(t) = &self.curr_token {
                if *t == token::Token::Eof {
                    break;
                }

                if let Some(stmt) = self.parse_statement() {
                    program.statements.push(stmt);
                }
                self.next_token();
            } else {
                break;
            }
        }

        return program;
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        if let Some(t) = &self.curr_token {
            match t {
                token::Token::Let => return self.parse_let_statement(),
                token::Token::Return => return self.parse_return_statement(),
                _ => return None,
            }
        } else {
            return None;
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if let Some(let_tok) = self.curr_token.take() {
            if !self.expect_peek(&token::Token::Ident("???".to_string())) {
                return None;
            }

            if let Some(token::Token::Ident(ident_str)) = &self.curr_token {
                let ident = Identifier {
                    token: token::Token::Ident(ident_str.to_string()),
                    value: ident_str.to_string(),
                };

                if !self.expect_peek(&token::Token::Assign) {
                    return None;
                }

                while !self.is_curr_token(&token::Token::Semicolon) {
                    self.next_token();
                }

                let stmt = Statement::LetStatement(let_tok, ident, Expression {});
                return Some(stmt);
            }
        }
        return None;
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        if let Some(tok) = self.curr_token.take() {
            while !self.is_curr_token(&token::Token::Semicolon) {
                self.next_token();
            }
            let stmt = Statement::ReturnStatement(tok, Expression {});
            return Some(stmt);
        }
        return None;
    }

    fn register_prefix_fn(&mut self, tok: token::Token, prefix_fn: prefix_parse_fn) {
        self.prefix_parse_fns.insert(tok, prefix_fn);
    }

    fn register_infix_fn(&mut self, tok: token::Token, infix_fn: infix_parse_fn) {
        self.infix_parse_fns.insert(tok, infix_fn);
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.take();
        if let Ok(token) = self.lex.next_token() {
            self.peek_token = Some(token);
        }
    }

    fn expect_peek(&mut self, t: &token::Token) -> bool {
        if self.is_peek_token(t) {
            self.next_token();
            return true;
        }
        self.peek_token_error(t);
        return false;
    }

    fn peek_token_error(&mut self, t: &token::Token) {
        if let Some(peek) = &self.peek_token {
            let msg = format!(
                "expected next token to be {:?} but got {:?} instead :(",
                t, peek
            );
            self.errors.push(msg);
        } else {
            let msg = format!(
                "expected next token to be {:?} but no valid token found :(",
                t
            );
            self.errors.push(msg);
        }
    }

    fn is_curr_token(&self, t: &token::Token) -> bool {
        if let Some(curr) = &self.curr_token {
            return *curr == *t;
        } else {
            return false;
        }
    }

    fn is_peek_token(&self, t: &token::Token) -> bool {
        if let Some(peek) = &self.peek_token {
            return std::mem::discriminant(peek) == std::mem::discriminant(t);
        } else {
            return false;
        }
    }
}

#[cfg(test)]
mod test {
    use anyhow::Result;

    use crate::{
        lexer::{lexer, token::Token},
        parser::ast::{Node, Program, Statement},
    };

    use super::Parser;

    fn parse_input(input: &str, num_stmts: usize) -> Program {
        let lex = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);
        check_parser_errors(&parser);
        let program = parser.parse_program();
        println!("{}", program);
        assert_eq!(
            program.statements.len(),
            num_stmts,
            "Expected program to have {} statements, received {}",
            num_stmts,
            program.statements.len()
        );
        return program;
    }

    fn check_parser_errors(p: &Parser) {
        let errors = &p.errors;
        if errors.len() == 0 {
            return;
        }
        if errors.len() > 1 {
            println!("parser has {} errors!", errors.len())
        } else {
            println!("parser has {} error!", errors.len())
        }

        for err in errors {
            println!("parser error found: {}", err);
        }

        panic!();
    }

    #[test]
    fn test_valid_let_statements() -> Result<()> {
        let invalid = "
let x 5;
let = 10;
let 838383;
";
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let program = parse_input(input, 3);

        let tests = vec![
            Token::Ident("x".to_string()),
            Token::Ident("y".to_string()),
            Token::Ident("foobar".to_string()),
        ];
        for (i, test) in tests.iter().enumerate() {
            if let Some(stmt) = program.statements.get(i) {
                assert_eq!(
                    stmt.token_literal() == Token::Let.to_string(),
                    true,
                    "invalid stmt token literal found: {}",
                    stmt.token_literal().to_string()
                );
                match stmt {
                    Statement::LetStatement(_, ident, _) => {
                        assert_eq!(
                            test.to_string() == ident.token.to_string(),
                            true,
                            "invalid identifier found, expected {} but found {}",
                            test.to_string(),
                            ident.value
                        )
                    }
                    _ => continue,
                }
            }
        }

        return Ok(());
    }

    #[test]
    fn test_valid_return_statements() -> Result<()> {
        let input = "
return 5;
return 10;
return 993322;
";
        let program = parse_input(input, 3);
        for stmt in program.statements {
            if let Statement::ReturnStatement(tok, _) = stmt {
                match tok {
                    Token::Return => continue,
                    _ => panic!("invalid token found, expected 'return' but found {}", tok),
                }
            } else {
                panic!(
                    "invalid stmt found, expected ReturnStatement but found {:?}",
                    stmt
                )
            }
        }
        return Ok(());
    }
}
