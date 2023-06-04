use std::fmt::Display;

use crate::lexer::token;

#[derive(Debug)]
pub struct Expression {}

#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

#[derive(Debug)]
pub struct LetStatement {
    pub let_token: token::Token, // token.LET token
    pub name: Identifier,        // variable name
    pub expr: Expression,        // variable value/expression
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub return_token: token::Token, // 'return' token
    pub expr: Expression,           // returned expression
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub first_token: token::Token, // first token of expression
    pub expr: Expression,          // expression
}

pub trait Node {
    fn token_literal(&self) -> String;
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::LetStatement(let_stmt) => let_stmt.let_token.to_string(),
            Self::ReturnStatement(return_stmt) => return_stmt.return_token.to_string(),
            Self::ExpressionStatement(expr_stmt) => expr_stmt.first_token.to_string(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LetStatement(let_stmt) => {
                writeln!(
                    f,
                    "{} {} = {:?}",
                    let_stmt.let_token, let_stmt.name.value, let_stmt.expr
                )
            }
            Self::ReturnStatement(return_stmt) => {
                writeln!(f, "{} {:?}", return_stmt.return_token, return_stmt.expr)
            }
            Self::ExpressionStatement(expr_stmt) => {
                writeln!(f, "{:?}", expr_stmt.expr)
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
