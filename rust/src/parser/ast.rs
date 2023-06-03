use std::fmt::Display;

use crate::lexer::token;

#[derive(Debug)]
pub struct Expression {}

#[derive(Debug)]
pub enum Statement {
    LetStatement(token::Token, Identifier, Expression), // token.LET token, variable name, variable value/expression
    ReturnStatement(token::Token, Expression),          // 'return' token, returned expression
    ExpressionStatement(token::Token, Expression),      // first token of expression, expression
}

pub trait Node {
    fn token_literal(&self) -> String;
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::LetStatement(tok, _, _) => return tok.to_string(),
            Self::ReturnStatement(tok, _) => return tok.to_string(),
            Self::ExpressionStatement(tok, _) => return tok.to_string(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LetStatement(tok, ident, expr) => {
                return writeln!(
                    f,
                    "{} {} = {:?}",
                    tok.to_string(),
                    ident.token.to_string(),
                    expr
                );
            }
            Self::ReturnStatement(tok, return_expr) => {
                return writeln!(f, "{} {:?}", tok.to_string(), return_expr);
            }
            Self::ExpressionStatement(_, expr) => {
                return writeln!(f, "{:?}", expr);
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
