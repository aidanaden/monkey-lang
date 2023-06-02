use crate::lexer::{
    lexer,
    token::{self, Token},
};

use super::ast::{Identifier, Program, Statement};

pub struct Parser {
    lex: lexer::Lexer,

    curr_token: Option<token::Token>,
    peek_token: Option<token::Token>,

    errors: Vec<String>,
}

impl Parser {
    pub fn new(lex: lexer::Lexer) -> Parser {
        let mut p = Parser {
            lex: lex,
            errors: vec![],
            curr_token: None,
            peek_token: None,
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
                _ => return None,
            }
        } else {
            return None;
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if let Some(tok) = self.curr_token.clone() {
            if !self.expect_peek(&token::Token::Ident("???".to_string())) {
                return None;
            }

            match &self.curr_token {
                Some(Token::Ident(ident_str)) => {
                    let ident = Identifier {
                        token: Token::Ident(ident_str.to_string()),
                        value: ident_str.to_string(),
                    };

                    if !self.expect_peek(&token::Token::Assign) {
                        return None;
                    }

                    while !self.is_curr_token(&token::Token::Semicolon) {
                        self.next_token();
                    }

                    let stmt = Statement::LetStatement(tok, ident);
                    return Some(stmt);
                }
                _ => return None,
            }
        }
        return None;
    }

    fn next_token(&mut self) {
        self.curr_token = self.peek_token.clone();
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
        parser::ast::{Node, Statement},
    };

    use super::Parser;

    fn create_parser(input: &str) -> Parser {
        let lex = lexer::Lexer::new(input.to_string());
        let parser = Parser::new(lex);
        return parser;
    }

    // func checkParserErrorss(t *testing.T, p *Parser) {
    // 	errors := p.Errors()
    // 	if len(errors) == 0 {
    // 		return
    // 	}

    // 	if len(errors) > 1 {
    // 		t.Errorf("parser has %d errors", len(errors))
    // 	} else {
    // 		t.Errorf("parser has %d error", len(errors))
    // 	}

    // 	for _, msg := range errors {
    // 		t.Errorf("parser error found: %q", msg)
    // 	}

    // 	t.FailNow()
    // }

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
    fn test_let_statements() -> Result<()> {
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
        let mut parser = create_parser(invalid);
        let program = parser.parse_program();

        check_parser_errors(&parser);
        assert_eq!(
            program.statements.len(),
            3,
            "Expected program to have 3 statements, received {}",
            program.statements.len()
        );

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
                    Statement::LetStatement(_, ident) => {
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
}

// func TestLetStatements(t *testing.T) {
// 	input := `
// let x = 5;
// let y = 10;
// let foobar = 838383;
// `

// 	l := lexer.New(input)
// 	p := New(l)

// 	program := p.ParseProgram()
// 	checkParserErrorss(t, p)
// 	if program == nil {
// 		t.Fatalf("ParseProgram() returned nil")
// 	}
// 	if len(program.Statements) != 3 {
// 		t.Fatalf("program.Statements ddoes not contain 3 statements, got=%d", len(program.Statements))
// 	}

// 	tests := []string{"x", "y", "foobar"}
// 	for i, test := range tests {
// 		stmt := program.Statements[i]
// 		if !testLetStatement(t, stmt, test) {
// 			return
// 		}
// 	}
// }

// func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
// 	if s.TokenLiteral() != "let" {
// 		t.Errorf("s.TokenLiteral() not 'let', got=%q", s.TokenLiteral())
// 		return false
// 	}

// 	letStmt, ok := s.(*ast.LetStatement)
// 	if !ok {
// 		t.Errorf("s not *ast.LetStatement, got=%T", s)
// 		return false
// 	}

// 	if letStmt.Name.Value != name {
// 		t.Errorf("LetStmt.Name.Value not '%s', got=%s", name, letStmt.Name.Value)
// 		return false
// 	}

// 	if letStmt.Name.TokenLiteral() != name {
// 		t.Errorf("s.Name not '%s', got=%s", name, letStmt.Name)
// 		return false
// 	}

// 	return true
// }

// func checkParserErrorss(t *testing.T, p *Parser) {
// 	errors := p.Errors()
// 	if len(errors) == 0 {
// 		return
// 	}

// 	if len(errors) > 1 {
// 		t.Errorf("parser has %d errors", len(errors))
// 	} else {
// 		t.Errorf("parser has %d error", len(errors))
// 	}

// 	for _, msg := range errors {
// 		t.Errorf("parser error found: %q", msg)
// 	}

// 	t.FailNow()
// }
