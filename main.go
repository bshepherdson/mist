package main

import (
	"fmt"
	"os"

	"github.com/antlr/antlr4/runtime/Go/antlr"
	"github.com/shepheb/mist/parser"
)

func main() {
	for _, file := range os.Args[1:] {
		fmt.Println("Parsing " + file)
		input, _ := antlr.NewFileStream(file)
		lexer := parser.NewSmalltalkLexer(input)
		stream := antlr.NewCommonTokenStream(lexer, 0)
		p := parser.NewSmalltalkParser(stream)
		p.AddErrorListener(antlr.NewDiagnosticErrorListener(true))
		p.BuildParseTrees = true
		tree := p.Methods()
		antlr.ParseTreeWalkerDefault.Walk(NewTreeShapeListener(), tree)
	}
}
