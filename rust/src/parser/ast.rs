use crate::lexer::token;

pub trait Node {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
pub struct Expression {}

#[derive(Debug)]
pub enum Statement {
    LetStatement(token::Token, Identifier),
    ReturnStatement(token::Token, Expression),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::LetStatement(token, _) => {
                return token.to_string();
            }
            Self::ReturnStatement(token, _) => {
                return token.to_string();
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

// impl Expression for Identifier {
//     // fn expression_node(&self) -> String {}
//     fn expression_node(&self) -> String {
//         return "".to_string();
//     }
// }

// type LetStatement struct {
// 	Token token.Token // the token.LET token
// 	Name  *Identifier
// 	Value Expression
// }

// func (ls *LetStatement) statementNode() {}
// func (ls *LetStatement) TokenLiteral() string {
// 	return ls.Token.Literal
// }

// type Identifier struct {
// 	Token token.Token // the token.IDENT token
// 	Value string
// }

// func (i *Identifier) expressionNode() {}
// func (i *Identifier) TokenLiteral() string {
// 	return i.Token.Literal
// }
