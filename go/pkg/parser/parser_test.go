package parser

import (
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
	if len(program.Statements) != 3 {
		t.Fatalf("program.Statements ddoes not contain %d statements, got=%d", numStmts, len(program.Statements))
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
