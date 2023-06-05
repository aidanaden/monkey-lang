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
    PrefixExpression {
        token: token::Token, // the prefix token, e.g. !, -
        operator: String,
        right: Box<Expression>,
    },
    InfixExpression {
        token: token::Token, // the operator token, e.g. +, -, *, /
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    Test {},
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
                writeln!(f, "{} {} = {:?}", let_token, name.value, expr)
            }
            Self::ReturnStatement { return_token, expr } => {
                writeln!(f, "{} {:?}", return_token, expr)
            }
            Self::ExpressionStatement { expr, .. } => {
                writeln!(f, "{:?}", expr)
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
        return writeln!(f, "{}", display.join("\n"));
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
