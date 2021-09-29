package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/shepheb/mist/parser"
)

func main() {
	list := NewSTListener()
	for _, file := range os.Args[1:] {
		fmt.Println("Parsing " + file)
		f, err := os.Open(file)
		if err != nil {
			fmt.Printf("Failed to open file: %v", err)
			return
		}

		r := bufio.NewReader(f)
		p := parser.NewParser(file, r, list)
		//p.Debug(true)
		p.Parse()
	}

	arr := list.cp.Emit()
	f, err := os.Create("st.bin")
	defer f.Close()
	if err != nil {
		fmt.Printf("Failed to open output file: %v\n", err)
		return
	}

	for _, b := range arr {
		f.Write([]byte{byte(b & 0xff), byte(b >> 8)})
	}
}
