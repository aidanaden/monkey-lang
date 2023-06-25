package parser

import (
	"fmt"
	"strconv"

	"github.com/aidanaden/monkey-lang/pkg/ast"
	"github.com/aidanaden/monkey-lang/pkg/lexer"
	"github.com/aidanaden/monkey-lang/pkg/token"
)

const (
	_ int = iota
	LOWEST
	EQUALS          // ==
	LESS_OR_GREATER // > or <
	SUM             // +
	PRODUCT         // *
	PREFIX          // -X or !X
	CALL            // myFunction(X)
)

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

type Parser struct {
	l      *lexer.Lexer
	errors []string

	currToken token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NE:       EQUALS,
	token.LT:       LESS_OR_GREATER,
	token.GT:       LESS_OR_GREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{l: l, errors: []string{}}

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.registerPrefixFn(token.IDENT, p.parseIdentifier)
	p.registerPrefixFn(token.INT, p.parseIntegerLiteral)
	p.registerPrefixFn(token.BANG, p.parsePrefixExpression)
	p.registerPrefixFn(token.MINUS, p.parsePrefixExpression)
	p.registerPrefixFn(token.TRUE, p.parseBoolean)
	p.registerPrefixFn(token.FALSE, p.parseBoolean)
	p.registerPrefixFn(token.LPAREN, p.parseGroupedExpression)

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.registerInfixFn(token.PLUS, p.parseInfixExpression)
	p.registerInfixFn(token.MINUS, p.parseInfixExpression)
	p.registerInfixFn(token.SLASH, p.parseInfixExpression)
	p.registerInfixFn(token.ASTERISK, p.parseInfixExpression)
	p.registerInfixFn(token.EQ, p.parseInfixExpression)
	p.registerInfixFn(token.NE, p.parseInfixExpression)
	p.registerInfixFn(token.LT, p.parseInfixExpression)
	p.registerInfixFn(token.GT, p.parseInfixExpression)
	// read first 2 tokens to set currToken and peekToken
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.currToken.Type != token.EOF {
		stmt := p.parseStatement()
		program.Statements = append(program.Statements, stmt)
		p.nextToken()
	}

	return program
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) peekTokenError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead :(", t, p.peekToken.Type)
	p.errors = append(p.errors, msg)
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s found", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.currToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.currToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}

	stmt.Name = &ast.Identifier{
		Token: p.currToken,
		Value: p.currToken.Literal,
	}

	if !p.expectPeek(token.ASSIGN) {
		return nil
	}

	// TODO: parse expression, skip till semicolon for now
	for !p.isCurrToken(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.currToken}
	p.nextToken()

	// TODO: parse expression, skip till semicolon for now
	for !p.isCurrToken(token.SEMICOLON) {
		p.nextToken()
	}

	return stmt
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.currToken}
	stmt.Expression = p.parseExpression(LOWEST)
	if p.isPeekToken(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.currToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.currToken.Type)
		return nil
	}
	leftExp := prefix()

	for !p.isPeekToken(token.SEMICOLON) && precedence < p.getPeekTokenPrecedence() {
		infix := p.infixParseFns[p.peekToken.Type]
		if infix == nil {
			return leftExp
		}
		p.nextToken()
		leftExp = infix(leftExp)
	}

	return leftExp
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{Token: p.currToken, Value: p.currToken.Literal}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.currToken}
	value, err := strconv.ParseInt(p.currToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("failed to parse %q as integer", p.currToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	expression := &ast.PrefixExpression{
		Token:    p.currToken,
		Operator: p.currToken.Literal,
	}
	p.nextToken()

	expression.Right = p.parseExpression(PREFIX)
	return expression
}

func (p *Parser) parseInfixExpression(left ast.Expression) ast.Expression {
	expression := &ast.InfixExpression{
		Token:    p.currToken,
		Operator: p.currToken.Literal,
		Left:     left,
	}
	precedence := p.getCurrTokenPrecedence()
	p.nextToken()
	expression.Right = p.parseExpression(precedence)
	return expression
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()
	expr := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return expr
}

func (p *Parser) parseBoolean() ast.Expression {
	return &ast.Boolean{
		Token: p.currToken,
		Value: p.isCurrToken(token.TRUE),
	}
}

func (p *Parser) registerPrefixFn(tokenType token.TokenType, prefixFn prefixParseFn) {
	p.prefixParseFns[tokenType] = prefixFn
}

func (p *Parser) registerInfixFn(tokenType token.TokenType, infixFn infixParseFn) {
	p.infixParseFns[tokenType] = infixFn
}

func (p *Parser) getCurrTokenPrecedence() int {
	if p, ok := precedences[p.currToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) getPeekTokenPrecedence() int {
	if p, ok := precedences[p.peekToken.Type]; ok {
		return p
	}
	return LOWEST
}

func (p *Parser) nextToken() {
	p.currToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) isCurrToken(t token.TokenType) bool {
	return p.currToken.Type == t
}

func (p *Parser) isPeekToken(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.isPeekToken(t) {
		p.nextToken()
		return true
	}
	p.peekTokenError(t)
	return false
}
