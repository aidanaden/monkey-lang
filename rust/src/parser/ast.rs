use std::fmt::Display;

use crate::lexer::token;

#[derive(Debug)]
pub enum Statement {
    LetStatement {
        let_token: token::Token, // token.LET token
        name: Identifier,        // variable name
        expr: Expression,        // variable value/expression
    },
    ReturnStatement {
        return_token: token::Token, // 'return' token
        expr: Expression,           // returned expression
    },
    ExpressionStatement {
        first_token: token::Token, // first token of expression
        expr: Expression,          // expression
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier {
        token: token::Token,
        value: String,
    },
    IntegerLiteral {
        token: token::Token, // the prefix token, e.g. !, -
        value: i32,
    },
    Prefix {
        token: token::Token, // the prefix token, e.g. !, -
        operator: String,
        right: Box<Expression>,
    },
    Infix {
        token: token::Token, // the operator token, e.g. +, -, *, /
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Boolean {
        token: token::Token, // the token::TRUE or token::FALSE token
        value: bool,
    },
    Test {},
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier { value, .. } => write!(f, "{}", *value),
            Expression::IntegerLiteral { value, .. } => write!(f, "{}", value.to_string()),
            Expression::Infix {
                left,
                operator,
                right,
                ..
            } => write!(
                f,
                "({} {} {})",
                left.to_string(),
                operator,
                right.to_string()
            ),
            Expression::Prefix {
                operator, right, ..
            } => write!(f, "({}{})", operator, right.to_string()),
            Expression::Boolean { value, .. } => write!(f, "{}", value.to_string()),
            Expression::Test {} => write!(f, "(test)"),
        }
    }
}

pub trait Node {
    fn token_literal(&self) -> String;
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::LetStatement { let_token, .. } => let_token.to_string(),
            Self::ReturnStatement { return_token, .. } => return_token.to_string(),
            Self::ExpressionStatement { first_token, .. } => first_token.to_string(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LetStatement {
                let_token,
                name,
                expr,
            } => {
                write!(f, "{} {} = {:?}", let_token, name.value, expr)
            }
            Self::ReturnStatement { return_token, expr } => {
                write!(f, "{} {:?}", return_token, expr)
            }
            Self::ExpressionStatement { expr, .. } => {
                write!(f, "{}", expr)
            }
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            return self
                .statements
                .get(0)
                .expect("first stmt expected")
                .token_literal();
        } else {
            return "".to_string();
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let display = self
            .statements
            .iter()
            .map(|stmt| {
                return format!("{}", stmt);
            })
            .collect::<Vec<_>>();
        return write!(f, "{}", display.join(""));
    }
}

#[derive(Debug)]
pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        return self.token.to_string();
    }
}
