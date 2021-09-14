package main

import (
	"encoding/json"
	"strings"
)

type Bytecode struct {
	Bytecode string  `json:"bytecode"`
	Index    int     `json:"index,omitempty"`
	Name     string  `json:"name,omitempty"`
	Keyword  string  `json:"keyword,omitempty"`
	Value    float64 `json:"value,omitempty"`

	Length   int    `json:"length,omitempty"`
	Argc     int    `json:"argc,omitempty"`
	ArgStart int    `json:"argStart,omitempty"`
	Temps    int    `json:"temps,omitempty"`
	Selector string `json:"selector,omitempty"`
	Super    bool   `json:"super,omitempty"`
}

type Method struct {
	selector string
	params   int
	locals   int
	pcStart  int // PC of the first bytecode, used to compute its length.
}

type Compiler struct {
	bytecodes []*Bytecode
	pc        int
	method    *Method // Currently-under-compilation method.
}

func newCompiler() *Compiler {
	return &Compiler{}
}

func (c *Compiler) compile(bc *Bytecode) {
	c.bytecodes = append(c.bytecodes, bc)
	//fmt.Printf("%04d  %t %+v\n", c.pc, c.bytecodes[c.pc] == bc, bc)
	c.pc++
}

// Bytecode summary:
// pushLocal(index), pushGlobal(name), pushSelf, pushInstVar(index)
// pushLiteral(value), storeLocal(index), storeInstVar(index)
// startBlock(length, argc, argStart)
// send(argc, super bool, selector)
// dup, drop, answer, answerBlock, answerSelf, primitive(name)
func (c *Compiler) pushLocal(index int) {
	c.compile(&Bytecode{Bytecode: "pushLocal", Index: index})
}

func (c *Compiler) pushGlobal(name string) {
	c.compile(&Bytecode{Bytecode: "pushGlobal", Name: name})
}

func (c *Compiler) pushSelf() {
	c.compile(&Bytecode{Bytecode: "pushSelf"})
}

func (c *Compiler) pushInstVar(index int) {
	c.compile(&Bytecode{Bytecode: "pushSelf", Index: index})
}

func (c *Compiler) pushNumber(value float64) {
	c.compile(&Bytecode{Bytecode: "pushNumber", Value: value})
}

func (c *Compiler) pushBool(value bool) {
	c.compile(&Bytecode{Bytecode: "pushBool", Super: value})
}

func (c *Compiler) pushString(value string) {
	c.compile(&Bytecode{Bytecode: "pushString", Name: value})
}

func (c *Compiler) storeLocal(index int) {
	c.compile(&Bytecode{Bytecode: "storeLocal", Index: index})
}

func (c *Compiler) storeInstVar(index int) {
	c.compile(&Bytecode{Bytecode: "storeInstVar", Index: index})
}

func (c *Compiler) startBlock(argStart, argc, length int) {
	c.compile(&Bytecode{
		Bytecode: "startBlock",
		Argc:     argc,
		ArgStart: argStart,
		Length:   length,
	})
}

func (c *Compiler) send(super bool, argc int, selector string) {
	c.compile(&Bytecode{
		Bytecode: "send",
		Argc:     argc,
		Super:    super,
		Selector: selector,
	})
}

func (c *Compiler) primitive(keyword, name string) {
	c.compile(&Bytecode{Bytecode: "primitive", Name: name, Keyword: keyword})
}

func (c *Compiler) dup() {
	c.compile(&Bytecode{Bytecode: "dup"})
}
func (c *Compiler) drop() {
	c.compile(&Bytecode{Bytecode: "drop"})
}
func (c *Compiler) answer() {
	c.compile(&Bytecode{Bytecode: "answer"})
}
func (c *Compiler) answerSelf() {
	c.compile(&Bytecode{Bytecode: "answerSelf"})
}
func (c *Compiler) answerBlock() {
	c.compile(&Bytecode{Bytecode: "answerBlock"})
}

func Jsonify(bytecodes []*Bytecode) (string, error) {
	var output []string
	for _, b := range bytecodes {
		js, err := json.MarshalIndent(b, "  ", "  ")
		if err != nil {
			return "", err
		}
		output = append(output, string(js))
	}
	return "[\n" + strings.Join(output, ",") + "]", nil
}
