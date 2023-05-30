package repl

import (
	"bufio"
	"fmt"
	"io"

	"github.com/aidanaden/monkey-lang/pkg/lexer"
	"github.com/aidanaden/monkey-lang/pkg/token"
)

const PROMPT = ">> "

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	for {
		fmt.Print(PROMPT)
		if scanned := scanner.Scan(); !scanned {
			return
		}

		line := scanner.Text()
		lex := lexer.New(line)

		for tok := lex.NextToken(); tok.Type != token.EOF; tok = lex.NextToken() {
			fmt.Printf("%+v\n", tok)
		}
	}
}
