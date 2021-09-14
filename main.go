package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
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

	str, err := Jsonify(list.cp.bytecodes)
	if err != nil {
		fmt.Printf("Failed to encode JSON: %v\n", err)
		return
	}
	err = ioutil.WriteFile("st.json", []byte(str), 0644)
	if err != nil {
		fmt.Printf("Failed to write JSON file: %v\n", err)
		return
	}
}
