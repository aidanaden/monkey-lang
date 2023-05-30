package lexer

import (
	"fmt"
	"testing"

	"github.com/aidanaden/interpreter-compiler-book/pkg/token"
	"github.com/stretchr/testify/assert"
)

func testInputTokens(t *testing.T, input string, expected []token.Token) {
	lexer := New(input)
	for _, expectedToken := range expected {
		token := lexer.NextToken()
		assert.Equal(t, expectedToken.Literal == token.Literal && expectedToken.Type == token.Type, true, fmt.Sprintf("Produced token is %+v but expected %+v", token, expectedToken))
	}
}

func TestSampleToken(t *testing.T) {
	input := `=+(){},;`
	tests := []token.Token{
		{Type: token.ASSIGN, Literal: "="},
		{Type: token.PLUS, Literal: "+"},
		{Type: token.LPAREN, Literal: "("},
		{Type: token.RPAREN, Literal: ")"},
		{Type: token.LBRACE, Literal: "{"},
		{Type: token.RBRACE, Literal: "}"},
		{Type: token.COMMA, Literal: ","},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.EOF, Literal: ""},
	}
	testInputTokens(t, input, tests)
}

func TestSampleCode(t *testing.T) {
	input := `let five = 5;
let ten = 10;

let add = fn(x, y) {
	x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
`
	tests := []token.Token{
		{Type: token.LET, Literal: "let"},
		{Type: token.IDENT, Literal: "five"},
		{Type: token.ASSIGN, Literal: "="},
		{Type: token.INT, Literal: "5"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.LET, Literal: "let"},
		{Type: token.IDENT, Literal: "ten"},
		{Type: token.ASSIGN, Literal: "="},
		{Type: token.INT, Literal: "10"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.LET, Literal: "let"},
		{Type: token.IDENT, Literal: "add"},
		{Type: token.ASSIGN, Literal: "="},
		{Type: token.FUNCTION, Literal: "fn"},
		{Type: token.LPAREN, Literal: "("},
		{Type: token.IDENT, Literal: "x"},
		{Type: token.COMMA, Literal: ","},
		{Type: token.IDENT, Literal: "y"},
		{Type: token.RPAREN, Literal: ")"},
		{Type: token.LBRACE, Literal: "{"},
		{Type: token.IDENT, Literal: "x"},
		{Type: token.PLUS, Literal: "+"},
		{Type: token.IDENT, Literal: "y"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.RBRACE, Literal: "}"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.LET, Literal: "let"},
		{Type: token.IDENT, Literal: "result"},
		{Type: token.ASSIGN, Literal: "="},
		{Type: token.IDENT, Literal: "add"},
		{Type: token.LPAREN, Literal: "("},
		{Type: token.IDENT, Literal: "five"},
		{Type: token.COMMA, Literal: ","},
		{Type: token.IDENT, Literal: "ten"},
		{Type: token.RPAREN, Literal: ")"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.BANG, Literal: "!"},
		{Type: token.MINUS, Literal: "-"},
		{Type: token.SLASH, Literal: "/"},
		{Type: token.ASTERISK, Literal: "*"},
		{Type: token.INT, Literal: "5"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.INT, Literal: "5"},
		{Type: token.LT, Literal: "<"},
		{Type: token.INT, Literal: "10"},
		{Type: token.GT, Literal: ">"},
		{Type: token.INT, Literal: "5"},
		{Type: token.SEMICOLON, Literal: ";"},

		{Type: token.IF, Literal: "if"},
		{Type: token.LPAREN, Literal: "("},
		{Type: token.INT, Literal: "5"},
		{Type: token.LT, Literal: "<"},
		{Type: token.INT, Literal: "10"},
		{Type: token.RPAREN, Literal: ")"},
		{Type: token.LBRACE, Literal: "{"},
		{Type: token.RETURN, Literal: "return"},
		{Type: token.TRUE, Literal: "true"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.RBRACE, Literal: "}"},
		{Type: token.ELSE, Literal: "else"},
		{Type: token.LBRACE, Literal: "{"},
		{Type: token.RETURN, Literal: "return"},
		{Type: token.FALSE, Literal: "false"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.RBRACE, Literal: "}"},

		{Type: token.INT, Literal: "10"},
		{Type: token.EQ, Literal: "=="},
		{Type: token.INT, Literal: "10"},
		{Type: token.SEMICOLON, Literal: ";"},

		{Type: token.INT, Literal: "10"},
		{Type: token.NE, Literal: "!="},
		{Type: token.INT, Literal: "9"},
		{Type: token.SEMICOLON, Literal: ";"},
		{Type: token.EOF, Literal: ""},
	}
	testInputTokens(t, input, tests)
}
