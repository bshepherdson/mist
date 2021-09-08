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
		p.AddErrorListener(antlr.NewConsoleErrorListener())
		p.SetTrace(antlr.NewTraceListener(p))
		// START HERE: Figure out why the tracing isn't working. It might be
		// instructive in debugging the weird errors I'm still seeing.
		p.BuildParseTrees = true
		tree := p.Methods()
		antlr.ParseTreeWalkerDefault.Walk(NewTreeShapeListener(), tree)
	}
}
