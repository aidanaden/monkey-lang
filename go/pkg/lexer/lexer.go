package lexer

import (
	"log"

	"github.com/aidanaden/monkey-lang/pkg/token"
)

type Lexer struct {
	input   string
	pos     int  // current position in input (points to current char)
	readPos int  // current reading position in input (after current char)
	char    byte // current char under examination
}

func New(input string) *Lexer {
	lexer := &Lexer{
		input: input,
	}
	lexer.readChar()
	return lexer
}

func (l *Lexer) readChar() {
	if l.readPos >= len(l.input) {
		// 0 is ascii for NUL
		l.char = 0
	} else {
		l.char = l.input[l.readPos]
	}
	l.pos = l.readPos
	l.readPos += 1
}

func (l *Lexer) peekChar() byte {
	if l.readPos >= len(l.input) {
		// 0 is ascii for NUL
		return 0
	} else {
		return l.input[l.readPos]
	}
}

func (l *Lexer) readIdentifierOrDigit(tokenType token.TokenType) string {
	pos := l.pos
	if tokenType == token.IDENT {
		for isLetter(l.char) {
			l.readChar()
		}
	} else if tokenType == token.INT {
		for isDigit(l.char) {
			l.readChar()
		}
	} else {
		log.Fatalf("readIdentifierOrDigit only accepts token.INT or token.IDENT types!")
	}
	return l.input[pos:l.pos]
}

func (l *Lexer) skipWhiteSpace() {
	for l.char == ' ' || l.char == '\t' || l.char == '\n' || l.char == '\r' {
		l.readChar()
	}
}

func (l *Lexer) NextToken() token.Token {
	var tok token.Token
	l.skipWhiteSpace()
	switch l.char {
	case '=':
		if l.peekChar() == '=' {
			char := l.char
			l.readChar()
			tok = token.Token{Type: token.EQ, Literal: string(char) + string(l.char)}
		} else {
			tok = token.NewToken(token.ASSIGN, l.char)
		}
	case '+':
		tok = token.NewToken(token.PLUS, l.char)
	case '-':
		tok = token.NewToken(token.MINUS, l.char)
	case '!':
		if l.peekChar() == '=' {
			char := l.char
			l.readChar()
			tok = token.Token{Type: token.NE, Literal: string(char) + string(l.char)}
		} else {
			tok = token.NewToken(token.BANG, l.char)
		}
	case '*':
		tok = token.NewToken(token.ASTERISK, l.char)
	case '/':
		tok = token.NewToken(token.SLASH, l.char)
	case '<':
		tok = token.NewToken(token.LT, l.char)
	case '>':
		tok = token.NewToken(token.GT, l.char)
	case ',':
		tok = token.NewToken(token.COMMA, l.char)
	case ';':
		tok = token.NewToken(token.SEMICOLON, l.char)
	case '(':
		tok = token.NewToken(token.LPAREN, l.char)
	case ')':
		tok = token.NewToken(token.RPAREN, l.char)
	case '{':
		tok = token.NewToken(token.LBRACE, l.char)
	case '}':
		tok = token.NewToken(token.RBRACE, l.char)
	case 0:
		tok.Type = token.EOF
		tok.Literal = ""
	default:
		if isLetter(l.char) {
			tok.Literal = l.readIdentifierOrDigit(token.IDENT)
			tok.Type = token.LookupIdentType(tok.Literal)
			return tok
		} else if isDigit(l.char) {
			tok.Literal = l.readIdentifierOrDigit(token.INT)
			tok.Type = token.INT
			return tok
		} else {
			tok = token.NewToken(token.ILLEGAL, l.char)
		}
	}
	l.readChar()
	return tok
}

func isLetter(c byte) bool {
	return 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c == '_'
}

func isDigit(c byte) bool {
	return '0' <= c && c <= '9'
}
