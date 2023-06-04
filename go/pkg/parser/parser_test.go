package parser

import (
	"fmt"
	"testing"

	"github.com/aidanaden/monkey-lang/pkg/ast"
	"github.com/aidanaden/monkey-lang/pkg/lexer"
)

func parseInput(t *testing.T, input string, numStmts int) *ast.Program {
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrorss(t, p)
	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}
	if numStmts > 0 && len(program.Statements) != numStmts {
		t.Fatalf("program.Statements does not contain %d statements, got=%d", numStmts, len(program.Statements))
	}
	return program
}

func checkParserErrorss(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	if len(errors) > 1 {
		t.Errorf("parser has %d errors", len(errors))
	} else {
		t.Errorf("parser has %d error", len(errors))
	}

	for _, msg := range errors {
		t.Errorf("parser error found: %q", msg)
	}

	t.FailNow()
}

func TestLetStatements(t *testing.T) {
	input := `
let x = 5;
let y = 10;
let foobar = 838383;
`

	program := parseInput(t, input, 3)
	tests := []string{"x", "y", "foobar"}
	for i, test := range tests {
		stmt := program.Statements[i]
		if !testLetStatement(t, stmt, test) {
			return
		}
	}
}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf("s.TokenLiteral() not 'let', got=%q", s.TokenLiteral())
		return false
	}

	letStmt, ok := s.(*ast.LetStatement)
	if !ok {
		t.Errorf("s not *ast.LetStatement, got=%T", s)
		return false
	}

	if letStmt.Name.Value != name {
		t.Errorf("LetStmt.Name.Value not '%s', got=%s", name, letStmt.Name.Value)
		return false
	}

	if letStmt.Name.TokenLiteral() != name {
		t.Errorf("s.Name not '%s', got=%s", name, letStmt.Name)
		return false
	}

	return true
}

func TestReturnStatements(t *testing.T) {
	input := `
return 5;
return 10;
return 993322;
`

	program := parseInput(t, input, 3)
	for _, stmt := range program.Statements {
		if !testReturnStatement(t, stmt) {
			return
		}
	}
}

func testReturnStatement(t *testing.T, s ast.Statement) bool {
	returnStmt, ok := s.(*ast.ReturnStatement)
	if !ok {
		t.Errorf("s not *ast.ReturnStatement, got=%T", s)
		return false
	}

	if returnStmt.TokenLiteral() != "return" {
		t.Errorf("returnStmt.TokenLiteral() not 'return',  got %q", returnStmt.TokenLiteral())
	}

	return true
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	program := parseInput(t, input, 1)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%q", program.Statements[0])
	}
	ident, ok := stmt.Expression.(*ast.Identifier)
	if !ok {
		t.Fatalf("exp not *ast.Identifier, got=%q", stmt.Expression)
	}
	if ident.Value != "foobar" {
		t.Errorf("ident.Value not %s, got=%s", "foobar", ident.Value)
	}
	if ident.TokenLiteral() != "foobar" {
		t.Errorf("ident.TokenLiteral not %s, got=%s", "foobar", ident.TokenLiteral())
	}
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"

	program := parseInput(t, input, 1)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%q", program.Statements[0])
	}
	literal, ok := stmt.Expression.(*ast.IntegerLiteral)
	if !ok {
		t.Errorf("expression expected to be *ast.IntegerLiteral, got=%T", stmt.Expression)
	}
	if literal.Value != 5 {
		t.Errorf("literal.Value expected to be %d, got=%d", 5, literal.Value)
	}
	if literal.TokenLiteral() != "5" {
		t.Errorf("literal.TokenLiteral expected to be %s, got=%s", "5", literal.TokenLiteral())
	}
}

func TestParsingPrefixExpressions(t *testing.T) {
	prefixTests := []struct {
		input        string
		operator     string
		integerValue int64
	}{
		{input: "!5;", operator: "!", integerValue: 5},
		{input: "-15;", operator: "-", integerValue: 15},
	}

	for _, tt := range prefixTests {
		program := parseInput(t, tt.input, 1)
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T", program.Statements[0])
		}
		expr, ok := stmt.Expression.(*ast.PrefixExpression)
		if !ok {
			t.Fatalf("stmt is not ast.PrefixExpression, got=%T", stmt.Expression)
		}
		if expr.Operator != tt.operator {
			t.Fatalf("expr.Operation is not '%s', got=%s", tt.operator, expr.Operator)
		}
		if !testIntegerLiteral(t, expr.Right, tt.integerValue) {
			return
		}
	}
}

func testIntegerLiteral(t *testing.T, il ast.Expression, value int64) bool {
	integ, ok := il.(*ast.IntegerLiteral)
	if !ok {
		t.Errorf("il not *ast.IntegerLiteral, got=%T", il)
		return false
	}
	if integ.Value != value {
		t.Errorf("integ.Value is not %d, got=%d", value, integ.Value)
		return false
	}
	if integ.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("integ.TokenLiteral not %d, got=%d", value, integ.Value)
		return false
	}
	return true
}

func testIdentifier(t *testing.T, expr ast.Expression, value string) bool {
	ident, ok := expr.(*ast.Identifier)
	if !ok {
		t.Errorf("expression not *ast.Identifier, got=%T", expr)
		return false
	}

	if ident.Value != value {
		t.Errorf("ident.Value not '%s', got='%s'", value, ident.Value)
		return false
	}

	if ident.TokenLiteral() != value {
		t.Errorf("ident.TokenLiteral not '%s', got='%s'", value, ident.TokenLiteral())
		return false
	}

	return true
}

func testLiteralExpression(t *testing.T, expr ast.Expression, expected interface{}) bool {
	switch v := expected.(type) {
	case int:
		return testIntegerLiteral(t, expr, int64(v))
	case int64:
		return testIntegerLiteral(t, expr, v)
	case string:
		return testIdentifier(t, expr, v)
	}
	t.Errorf("invalid expr type, got=%T", expr)
	return false
}

func testInfixExpression(t *testing.T, expr ast.Expression, left interface{}, operator string, right interface{}) bool {
	infixExpr, ok := expr.(*ast.InfixExpression)
	if !ok {
		t.Errorf("expr is not ast.InfixExpression, got=%T(%s)", expr, expr)
		return false
	}
	if !testLiteralExpression(t, infixExpr.Left, left) {
		return false
	}
	if infixExpr.Operator != operator {
		t.Errorf("expr.Operator is not '%s', got=%q", operator, infixExpr.Operator)
		return false
	}
	if !testLiteralExpression(t, infixExpr.Right, right) {
		return false
	}
	return true
}

func TestParsingInfixExpressions(t *testing.T) {
	infixTests := []struct {
		input      string
		leftValue  int64
		operator   string
		rightValue int64
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
	}

	for _, tt := range infixTests {
		program := parseInput(t, tt.input, 1)
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not ast.ExpressionStatement, got=%T", program.Statements[0])
		}
		if !testInfixExpression(t, stmt.Expression, tt.leftValue, tt.operator, tt.rightValue) {
			return
		}
	}
}

func TestOperatorPrecedenceParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			input:    "-a * b",
			expected: "((-a) * b)",
		},
		{
			input:    "!-a",
			expected: "(!(-a))",
		},
		{
			input:    "a + b + c",
			expected: "((a + b) + c)",
		},
		{
			input:    "a + b - c",
			expected: "((a + b) - c)",
		},
		{
			input:    "a * b * c",
			expected: "((a * b) * c)",
		},
		{
			input:    "a * b / c",
			expected: "((a * b) / c)",
		},
		{
			input:    "a + b / c",
			expected: "(a + (b / c))",
		},
		{
			input:    "a + b * c + d / e - f",
			expected: "(((a + (b * c)) + (d / e)) - f)",
		},
		{
			input:    "3 + 4; -5 * 5",
			expected: "(3 + 4)((-5) * 5)",
		},
		{
			input:    "5 > 4 == 3 < 4",
			expected: "((5 > 4) == (3 < 4))",
		},
		{
			input:    "5 < 4 != 3 > 4",
			expected: "((5 < 4) != (3 > 4))",
		},
		{
			input:    "3 + 4 * 5 == 3 * 1 + 4 * 5",
			expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
	}

	for _, tt := range tests {
		program := parseInput(t, tt.input, 0)
		actual := program.String()
		if actual != tt.expected {
			t.Errorf("expected=%q but got=%q", tt.expected, actual)
		}
	}
}
