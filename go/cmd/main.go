package main

import (
	"fmt"
	"os"
	"os/user"

	"github.com/aidanaden/monkey-lang/pkg/repl"
)

func main() {
	user, err := user.Current()
	if err != nil {
		panic(err)
	}

	fmt.Printf("Hello %s! Welcome to the Monkey programming language!\n", user.Username)
	fmt.Printf("Enter commands below:\n")
	repl.Start(os.Stdin, os.Stdout)
}
