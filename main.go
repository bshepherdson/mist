package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/shepheb/mist/parser"
)

func main() {
	for _, file := range os.Args[1:] {
		fmt.Println("Parsing " + file)
		f, err := os.Open(file)
		if err != nil {
			fmt.Printf("Failed to open file: %v", err)
			return
		}

		r := bufio.NewReader(f)
		list := parser.NewDebugListener(parser.NewDummyListener())
		p := parser.NewParser(file, r, list)
		//p.Debug(true)
		p.Parse()
	}
}
