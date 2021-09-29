package main

type Method struct {
	selector  string
	params    int
	locals    int
	literals  []STLiteral
	bytecodes []uint16
	pc        int
}

type Compiler struct {
	method   *Method // Currently-under-compilation method.
	commands []STLiteral
	current  litArray
}

func newCompiler() *Compiler {
	return &Compiler{}
}

func (c *Compiler) compile(bc uint16) {
	c.method.bytecodes = append(c.method.bytecodes, bc)
	c.method.pc++
}

// Returns the literal number. Checks whether it's already there!
func (c *Compiler) addLiteral(lit STLiteral) uint16 {
	for i, o := range c.method.literals {
		if lit.Equals(o) {
			return uint16(i)
		}
	}
	ix := len(c.method.literals)
	c.method.literals = append(c.method.literals, lit)
	return uint16(ix)
}

// Rewinds the compiler so the next bytecode goes at to.
func (c *Compiler) rewind(to int) {
	c.method.bytecodes = c.method.bytecodes[0:to]
	c.method.pc = to
}

func (c *Compiler) startMethods(class string, classy bool) {
	if c.current != nil {
		panic("overlapping commands in progress!")
	}
	c.current = []STLiteral{intLit(1), symbolLit(class)}
	if classy {
		c.current[0] = intLit(2)
	}
}

func (c *Compiler) endMethods() {
	c.commands = append(c.commands, c.current)
	c.current = nil
}

func (c *Compiler) startMethod(selector string, argc, locals int) {
	c.method = &Method{
		selector: selector,
		params:   argc,
		locals:   locals,
	}
}

func (c *Compiler) endMethod() {
	// Encode the method into its array form.
	// They are #( #selectorSymbol argc locals #( bytecode ) #( literals ) )
	// TODO Probably want to add byte arrays as another type? It'll make bytecode
	// way more compact, N + 1 words instead of 3N + 1.
	m := []STLiteral{
		symbolLit(c.method.selector),
		intLit(c.method.params),
		intLit(c.method.locals),
		rawArray(c.method.bytecodes),
		litArray(c.method.literals),
	}
	c.current = append(c.current, litArray(m))
}

func (c *Compiler) startTopLevel() {
	if c.current != nil {
		panic("overlapping commands in progress!")
	}
	c.current = []STLiteral{intLit(3)} // Indicates inline code.
	c.method = &Method{}
}

func (c *Compiler) endTopLevel() {
	c.current = append(c.current, rawArray(c.method.bytecodes),
		litArray(c.method.literals))
	c.commands = append(c.commands, c.current)
	c.current = nil
}

// Basic bytecodes. Some of these are smart and choose the right subtype based
// on the argument (eg. literal numbers that fit into the inlined literals).
func (c *Compiler) pushLocal(index int) {
	// [0|1|local index from 0]
	c.compile(0x0100 | uint16(index))
}

func (c *Compiler) pushGlobal(name string) {
	// [0|3|literal index]
	ix := c.addLiteral(symbolLit(name))
	c.compile(0x0300 | ix)
}

func (c *Compiler) pushSelf() {
	c.compile(0x0000)
}

func (c *Compiler) pushInstVar(index int) {
	// [0|2|index]
	c.compile(0x0200 | uint16(index))
}

func (c *Compiler) pushNumber(value int) {
	// Values that fit in signed 8-bit can be inlined.
	if -128 <= value && value <= 127 {
		// [0|6|value]
		c.compile(0x0600 | uint16(value&0xff))
	} else {
		// [0|5|index]
		c.compile(0x0500 | c.addLiteral(intLit(value)))
	}
}

func (c *Compiler) pushBool(value bool) {
	// [0|0| value ? 3 : 4]
	var bc uint16 = 3
	if !value {
		bc = 4
	}
	c.compile(bc)
}

func (c *Compiler) pushNil() {
	c.compile(0x0002)
}

func (c *Compiler) pushContext() {
	c.compile(0x0001)
}

func (c *Compiler) pushString(value string) {
	// [0|5|literal index]
	c.compile(0x0500 | c.addLiteral(stringLit(value)))
}

func (c *Compiler) pushSymbol(value string) {
	// [0|5|literal index]
	c.compile(0x0500 | c.addLiteral(symbolLit(value)))
}

func (c *Compiler) pushCharacter(ch rune) {
	// [0|5|literal index]
	c.compile(0x0500 | c.addLiteral(charLit(ch)))
}

func (c *Compiler) pushClass(index int) {
	// [0|4|index] Pushes a core class by its class index.
	c.compile(0x0400 | uint16(index))
}

func (c *Compiler) storeLocal(index int) {
	// [1|0|index]
	c.compile(0x1000 | uint16(index))
}

func (c *Compiler) storeInstVar(index int) {
	// [1|1|index]
	c.compile(0x1100 | uint16(index))
}

func (c *Compiler) startBlock(argc, locals, argStart, length int) {
	// [5|argc|argStart] + [length]
	c.compile(0x5000 | (uint16(argc) << 8) | uint16(argStart))
	c.compile(uint16(length))
}

func (c *Compiler) send(super bool, argc int, selector string) {
	// [super ? 3 : 2 | argc | selector literal index]
	// TODO $4 is canned message sends, but I need to find out which sends are
	// the most (statically) common before applying this. It can save a lot of
	// space, since there's room for 256 messages each for 0, 1 and 2 args!
	op := uint16(0x2000)
	if super {
		op = 0x3000
	}
	c.compile(op | (uint16(argc) << 8) | c.addLiteral(symbolLit(selector)))
}

func (c *Compiler) primitive(index int) {
	// [7|_|primitive number]
	c.compile(0x7000 | uint16(index))
}

func (c *Compiler) dup() {
	c.compile(0x6000)
}
func (c *Compiler) drop() {
	c.compile(0x6100)
}
func (c *Compiler) answer() {
	c.compile(0x6200)
}
func (c *Compiler) answerSelf() {
	c.compile(0x6300)
}
func (c *Compiler) answerBlock() {
	c.compile(0x6400)
}

func (c *Compiler) Emit() []uint16 {
	arr := []uint16{}
	for _, cmd := range c.commands {
		arr = append(arr, cmd.Encode()...)
	}
	return arr
}
