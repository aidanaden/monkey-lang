use super::ast::{Expression, Identifier, Program, Statement};
use crate::lexer::{
    lexer,
    token::{self, Precedence, PrecedencePriority},
};

pub struct Parser {
    lex: lexer::Lexer,
    errors: Vec<String>,
    curr_token: Option<token::Token>,
    peek_token: Option<token::Token>,
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
                token::Token::Return => return self.parse_return_statement(),
                _ => return self.parse_expression_statement(),
            }
        }
        return None;
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_tok = self.curr_token.take()?;

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

            let stmt = Statement::LetStatement {
                let_token: let_tok,
                name: ident,
                expr: Expression::Test {},
            };
            return Some(stmt);
        }

        return None;
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let tok = self.curr_token.take()?;
        while !self.is_curr_token(&token::Token::Semicolon) {
            self.next_token();
        }
        let stmt = Statement::ReturnStatement {
            return_token: tok,
            expr: Expression::Test {},
        };
        return Some(stmt);
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let tok = self.curr_token.clone()?;
        let expr = self.parse_expression(PrecedencePriority::Lowest)?;
        let stmt = Statement::ExpressionStatement {
            first_token: tok,
            expr: expr,
        };
        return Some(stmt);
    }

    fn parse_expression(&mut self, precedence: PrecedencePriority) -> Option<Expression> {
        let tok = self.curr_token.clone()?;
        let mut left_expr = self.parse_prefix_fn(&tok)?;
        while !self.is_peek_token(&token::Token::Semicolon)
            && precedence < self.peek_token_precedence()
        {
            let peek_tok = self.peek_token.clone()?;
            if let Some(next_left_expr) = self.parse_infix_fn(&peek_tok, left_expr.clone()) {
                left_expr = next_left_expr;
            }
        }
        return Some(left_expr);
    }

    fn parse_prefix_fn(&mut self, tok: &token::Token) -> Option<Expression> {
        match tok {
            token::Token::Ident(_) => self.parse_identifier_expr(),
            token::Token::Int(_) => self.parse_integer_literal_expr(),
            token::Token::Bang | token::Token::Minus => self.parse_prefix_expr(),
            token::Token::True | token::Token::False => self.parse_bool_expr(),
            token::Token::LParen => self.parse_grouped_expr(),
            _ => None,
        }
    }

    fn parse_infix_fn(&mut self, tok: &token::Token, left: Expression) -> Option<Expression> {
        match tok {
            token::Token::Plus
            | token::Token::Minus
            | token::Token::Slash
            | token::Token::Asterisk
            | token::Token::Eq
            | token::Token::Ne
            | token::Token::Lt
            | token::Token::Gt => self.next_token(),
            _ => return None,
        }
        return self.parse_infix_expr(left);
    }

    fn parse_identifier_expr(&mut self) -> Option<Expression> {
        let curr = self.curr_token.take()?;
        return Some(Expression::Identifier {
            value: curr.to_string(),
            token: curr,
        });
    }

    fn parse_integer_literal_expr(&self) -> Option<Expression> {
        if let Some(token::Token::Int(int_value)) = &self.curr_token {
            if let Ok(value) = int_value.parse::<i32>() {
                return Some(Expression::IntegerLiteral {
                    token: token::Token::Int(int_value.to_string()),
                    value: value,
                });
            }
        }
        return None;
    }

    fn parse_bool_expr(&mut self) -> Option<Expression> {
        let tok = self.curr_token.take()?;
        match tok {
            token::Token::True | token::Token::False => {
                return Some(Expression::Boolean {
                    value: tok == token::Token::True,
                    token: tok,
                })
            }
            _ => return None,
        }
    }

    fn parse_grouped_expr(&mut self) -> Option<Expression> {
        self.next_token();
        let expr = self.parse_expression(PrecedencePriority::Lowest);
        if self.expect_peek(&token::Token::RParen) {
            return expr;
        }
        return None;
    }

    fn parse_prefix_expr(&mut self) -> Option<Expression> {
        let tok = self.curr_token.take()?;
        self.next_token();
        let right_expr = self.parse_expression(PrecedencePriority::Prefix)?;
        return Some(Expression::Prefix {
            operator: tok.to_string(),
            token: tok,
            right: Box::new(right_expr),
        });
    }

    fn parse_infix_expr(&mut self, left: Expression) -> Option<Expression> {
        let tok = self.curr_token.clone()?;
        let precedence = self.curr_token_precedence();
        self.next_token();
        let right_expr = self.parse_expression(precedence)?;
        return Some(Expression::Infix {
            operator: tok.to_string(),
            token: tok,
            left: Box::new(left),
            right: Box::new(right_expr),
        });
    }

    fn curr_token_precedence(&self) -> PrecedencePriority {
        if let Some(tok) = &self.curr_token {
            return tok.precedence();
        }
        return PrecedencePriority::Lowest;
    }

    fn peek_token_precedence(&self) -> PrecedencePriority {
        if let Some(tok) = &self.peek_token {
            return tok.precedence();
        }
        return PrecedencePriority::Lowest;
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
        lexer::{
            lexer,
            token::{self, Token},
        },
        parser::ast::{Expression, Node, Program, Statement},
    };

    use super::Parser;

    fn parse_input(input: &str, num_stmts: usize) -> Program {
        let lex = lexer::Lexer::new(input.to_string());
        let mut parser = Parser::new(lex);
        check_parser_errors(&parser);
        let program = parser.parse_program();
        if num_stmts > 0 {
            assert_eq!(
                program.statements.len(),
                num_stmts,
                "Expected program to have {} statements, received {}",
                num_stmts,
                program.statements.len()
            );
        }
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
                    Statement::LetStatement { name, .. } => {
                        assert_eq!(
                            test.to_string() == name.token.to_string(),
                            true,
                            "invalid identifier found, expected {} but found {}",
                            test.to_string(),
                            name.value
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
            if let Statement::ReturnStatement { return_token, .. } = stmt {
                match return_token {
                    Token::Return => continue,
                    _ => panic!(
                        "invalid token found, expected 'return' but found {}",
                        return_token
                    ),
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

    #[test]
    fn test_identifier_expression() -> Result<()> {
        let input = "foobar";
        let program = parse_input(input, 1);
        for stmt in program.statements {
            if let Statement::ExpressionStatement {
                expr:
                    Expression::Identifier {
                        token: token::Token::Ident(ident_str),
                        value,
                    },
                ..
            } = stmt
            {
                assert_eq!(
                    value == input.to_string(),
                    true,
                    "invalid identifier value found, expected {} but found {}",
                    input,
                    value
                );
                assert_eq!(
                    ident_str == input.to_string(),
                    true,
                    "invalid token literal value found, expected {} but found {}",
                    input,
                    ident_str
                )
            } else {
                panic!("failed to parse IdentifierExpression stmt!")
            }
        }

        return Ok(());
    }

    #[test]
    fn test_integer_literal_expression() -> Result<()> {
        let input = "5;";
        let program = parse_input(input, 1);
        for stmt in program.statements {
            if let Statement::ExpressionStatement {
                expr:
                    Expression::IntegerLiteral {
                        token: token::Token::Int(int_str),
                        value,
                    },
                ..
            } = stmt
            {
                assert_eq!(
                    value == 5,
                    true,
                    "invalid identifier value found, expected {} but found {}",
                    input,
                    value
                );
                assert_eq!(
                    int_str == "5",
                    true,
                    "invalid token literal value found, expected {} but found {}",
                    input,
                    int_str
                )
            } else {
                panic!("failed to parse IntegerLiteralExpression stmt!")
            }
        }
        return Ok(());
    }

    #[test]
    fn test_boolean_expression() -> Result<()> {
        let input = "true;";
        let program = parse_input(input, 1);
        for stmt in program.statements {
            if let Statement::ExpressionStatement {
                expr: Expression::Boolean { token, value },
                ..
            } = stmt
            {
                assert_eq!(
                    value == true,
                    true,
                    "invalid boolean value found, expected {} but found {}",
                    true,
                    value
                );
                assert_eq!(
                    token.to_string() == "true",
                    true,
                    "invalid token literal value found, expected {} but found {}",
                    "true",
                    token.to_string()
                )
            } else {
                panic!("failed to parse BooleanExpression stmt!")
            }
        }
        return Ok(());
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct TestData<'a> {
            input: &'a str,
            operator: token::Token,
            integer_value: i32,
        }
        let tests = vec![
            TestData {
                input: "!5;",
                operator: token::Token::Bang,
                integer_value: 5,
            },
            TestData {
                input: "-15;",
                operator: token::Token::Minus,
                integer_value: 15,
            },
        ];

        for test in tests {
            let program = parse_input(test.input, 1);
            for stmt in program.statements {
                if let Statement::ExpressionStatement {
                    expr:
                        Expression::Prefix {
                            operator, right, ..
                        },
                    ..
                } = stmt
                {
                    assert_eq!(
                        operator == test.operator.to_string(),
                        true,
                        "invalid operator found, expected {} but found {}",
                        test.operator,
                        operator
                    );
                    test_integer_literal(&*right, test.integer_value);
                } else {
                    panic!("failed to parse PrefixExpression stmt!")
                }
            }
        }
    }

    fn test_integer_literal(expr: &Expression, value: i32) {
        if let Expression::IntegerLiteral {
            token,
            value: integer_value,
        } = expr
        {
            if value != *integer_value {
                panic!(
                    "IntegerLiteralExpression value {} does not match expected {}",
                    integer_value, value
                );
            }
            if token.to_string() != value.to_string() {
                panic!(
                    "IntegerLiteralExpression token string value {} does not match expected string {}",
                    token.to_string(), value.to_string()
                );
            }
        } else {
            panic!("failed to parse IntegerLiteralExpression");
        }
    }

    fn test_identifier(expr: &Expression, value: &String) {
        if let Expression::Identifier {
            token,
            value: ident_value,
        } = expr
        {
            if value != ident_value {
                panic!(
                    "IdentifierExpression value {} does not match expected {}",
                    ident_value, value
                );
            }
            if token.to_string() != value.to_string() {
                panic!(
                    "IdentifierExpression token string value {} does not match expected string {}",
                    token.to_string(),
                    value.to_string()
                );
            }
        } else {
            panic!("failed to parse IdentifierExpression");
        }
    }

    fn test_boolean(expr: &Expression, value: &bool) {
        if let Expression::Boolean {
            token,
            value: bool_value,
        } = expr
        {
            if value != bool_value {
                panic!(
                    "BooleanExpression value {} does not match expected {}",
                    bool_value, value
                );
            }
            if token.to_string() != value.to_string() {
                panic!(
                    "BooleanExpression token string value {} does not match expected string {}",
                    token.to_string(),
                    bool_value.to_string()
                );
            }
        } else {
            panic!("failed to parse BooleanExpression");
        }
    }

    enum ExpectedValue {
        Int(i32),
        String(String),
        Boolean(bool),
    }

    fn test_literal_expression(expr: &Expression, expected: ExpectedValue) {
        match expected {
            ExpectedValue::Int(expected_int) => test_integer_literal(expr, expected_int),
            ExpectedValue::String(expected_str) => test_identifier(expr, &expected_str),
            ExpectedValue::Boolean(expected_bool) => test_boolean(expr, &expected_bool),
        }
    }

    fn test_infix_expression(
        expr: &Expression,
        left: ExpectedValue,
        operator: &token::Token,
        right: ExpectedValue,
    ) {
        if let Expression::Infix {
            left: infix_left,
            operator: infix_operator,
            right: infix_right,
            ..
        } = expr
        {
            test_literal_expression(&*infix_left, left);
            if operator.to_string() != *infix_operator {
                panic!(
                    "InfixExpression operator {} does not match expected operator {}",
                    infix_operator, operator
                );
            }
            test_literal_expression(&*infix_right, right);
        } else {
            panic!("failed to parse InfixExpression");
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        #[derive(Debug, Copy, Clone)]
        enum TestValue {
            Int(i32),
            Bool(bool),
        }
        struct TestData<'a> {
            input: &'a str,
            left_value: TestValue,
            operator: token::Token,
            right_value: TestValue,
        }
        let tests = vec![
            TestData {
                input: "5 + 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Plus,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "5 - 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Minus,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "5 * 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Asterisk,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "5 / 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Slash,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "5 > 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Gt,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "5 < 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Lt,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "5 == 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Eq,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "5 != 5;",
                left_value: TestValue::Int(5),
                operator: token::Token::Ne,
                right_value: TestValue::Int(5),
            },
            TestData {
                input: "true == true;",
                left_value: TestValue::Bool(true),
                operator: token::Token::Eq,
                right_value: TestValue::Bool(true),
            },
            TestData {
                input: "true == true;",
                left_value: TestValue::Bool(true),
                operator: token::Token::Eq,
                right_value: TestValue::Bool(true),
            },
            TestData {
                input: "true != false;",
                left_value: TestValue::Bool(true),
                operator: token::Token::Ne,
                right_value: TestValue::Bool(false),
            },
            TestData {
                input: "false == false;",
                left_value: TestValue::Bool(false),
                operator: token::Token::Eq,
                right_value: TestValue::Bool(false),
            },
        ];

        for test in tests {
            let program = parse_input(test.input, 1);
            for stmt in program.statements {
                if let Statement::ExpressionStatement { expr, .. } = stmt {
                    match (test.left_value, test.right_value) {
                        // token::Token::True | token::Token::False =>
                        (TestValue::Int(left_int), TestValue::Int(right_int)) => {
                            test_infix_expression(
                                &expr,
                                ExpectedValue::Int(left_int),
                                &test.operator,
                                ExpectedValue::Int(right_int),
                            )
                        }
                        (TestValue::Bool(left_bool), TestValue::Bool(right_bool)) => {
                            test_infix_expression(
                                &expr,
                                ExpectedValue::Boolean(left_bool),
                                &test.operator,
                                ExpectedValue::Boolean(right_bool),
                            )
                        }
                        _ => continue,
                    }
                } else {
                    panic!("failed to parse InfixExpressionStatement");
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestData {
            input: String,
            expected: String,
        }

        let tests = vec![
            TestData {
                input: String::from("-a * b"),
                expected: String::from("((-a) * b)"),
            },
            TestData {
                input: String::from("!-a"),
                expected: String::from("(!(-a))"),
            },
            TestData {
                input: String::from("a + b + c"),
                expected: String::from("((a + b) + c)"),
            },
            TestData {
                input: String::from("a + b - c"),
                expected: String::from("((a + b) - c)"),
            },
            TestData {
                input: String::from("a * b * c"),
                expected: String::from("((a * b) * c)"),
            },
            TestData {
                input: String::from("a * b / c"),
                expected: String::from("((a * b) / c)"),
            },
            TestData {
                input: String::from("a + b / c"),
                expected: String::from("(a + (b / c))"),
            },
            TestData {
                input: String::from("a + b * c + d / e - f"),
                expected: String::from("(((a + (b * c)) + (d / e)) - f)"),
            },
            TestData {
                input: String::from("3 + 4; -5 * 5"),
                expected: String::from("(3 + 4)((-5) * 5)"),
            },
            TestData {
                input: String::from("5 > 4 == 3 < 4"),
                expected: String::from("((5 > 4) == (3 < 4))"),
            },
            TestData {
                input: String::from("5 < 4 != 3 > 4"),
                expected: String::from("((5 < 4) != (3 > 4))"),
            },
            TestData {
                input: String::from("3 + 4 * 5 == 3 * 1 + 4 * 5"),
                expected: String::from("((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            },
            TestData {
                input: String::from("true"),
                expected: String::from("true"),
            },
            TestData {
                input: String::from("false"),
                expected: String::from("false"),
            },
            TestData {
                input: String::from("3 > 5 == false"),
                expected: String::from("((3 > 5) == false)"),
            },
            TestData {
                input: String::from("3 < 5 == true"),
                expected: String::from("((3 < 5) == true)"),
            },
            TestData {
                input: String::from("1 + (2 + 3) + 4"),
                expected: String::from("((1 + (2 + 3)) + 4)"),
            },
            TestData {
                input: String::from("(5 + 5) * 2"),
                expected: String::from("((5 + 5) * 2)"),
            },
            TestData {
                input: String::from("2 / (5 + 5)"),
                expected: String::from("(2 / (5 + 5))"),
            },
            TestData {
                input: String::from("-(5 + 5)"),
                expected: String::from("(-(5 + 5))"),
            },
            TestData {
                input: String::from("!(true == true)"),
                expected: String::from("(!(true == true))"),
            },
        ];

        for test in tests {
            let program = parse_input(&test.input, 0);
            let output = program.to_string();
            if output != test.expected {
                panic!(
                    "invalid output, got {} but expected {}",
                    output, test.expected
                );
            }
        }
    }
}
