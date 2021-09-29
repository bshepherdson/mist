package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"unicode"

	"github.com/shepheb/mist/parser"
)

// Smalltalk listener, consuming the parser's stream of changes.
type STL struct {
	cp               *Compiler
	inMethods        bool
	methodName       string
	methodLocals     int  // Number of actual local slots needed.
	methodLocalIndex int  // Index compile-time locals list, includes self, params.
	first            bool // True for the first expression of a block or method.

	blockDepth  int
	blockStarts []int

	answered, superSend bool
	scope               *Scope
	classes             map[string]*class
	classIndexes        map[int]*class
	nextClassIndex      int
}

type class struct {
	name       string
	superclass *class
	index      int
	members    map[string]int
}

func (l *STL) initClasses() {
	l.classes["ProtoObject"] = &class{name: "ProtoObject", index: 0}
	l.classes["Object"] = &class{
		name:       "Object",
		superclass: l.classes["ProtoObject"],
		index:      1,
	}
	l.classes["Behavior"] = &class{
		name:       "Behavior",
		superclass: l.classes["Object"],
		members:    instVars(0, "superclass", "methodDict", "format"),
		index:      2,
	}
	l.classes["ClassDescription"] = &class{
		name:       "ClassDescription",
		superclass: l.classes["Behavior"],
		index:      3,
	}
	l.classes["Class"] = &class{
		name:       "Class",
		superclass: l.classes["ClassDescription"],
		members:    instVars(3, "name", "subclasses"),
		index:      4,
	}
	l.classes["Metaclass"] = &class{
		name:       "Metaclass",
		superclass: l.classes["ClassDescription"],
		members:    instVars(3, "thisClass"),
		index:      5,
	}
	l.classes["UndefinedObject"] = &class{
		name:       "UndefinedObject",
		superclass: l.classes["Object"],
		index:      6,
	}

	l.classes["Collection"] = &class{
		name:       "Collection",
		superclass: l.classes["Object"],
		index:      7,
	}
	l.classes["SequenceableCollection"] = &class{
		name:       "SequenceableCollection",
		superclass: l.classes["Collecton"],
		index:      8,
	}
	l.classes["ArrayedCollection"] = &class{
		name:       "ArrayedCollection",
		superclass: l.classes["SequenceableCollecton"],
		index:      9,
	}
	l.classes["Array"] = &class{
		name:       "Array",
		superclass: l.classes["ArrayedCollection"],
		index:      10,
	}

	l.classes["String"] = &class{
		name:       "String",
		superclass: l.classes["ArrayedCollection"],
		index:      11,
	}
	l.classes["Symbol"] = &class{
		name:       "Symbol",
		superclass: l.classes["String"],
		index:      12,
	}

	l.classes["HashedCollection"] = &class{
		name:       "HashedCollection",
		superclass: l.classes["Collection"],
		members:    instVars(0, "array", "tally"),
		index:      13,
	}
	l.classes["Dictionary"] = &class{
		name:       "Dictionary",
		superclass: l.classes["HashedCollection"],
		index:      14,
	}
	l.classes["IdentityDictionary"] = &class{
		name:       "IdentityDictionary",
		superclass: l.classes["Dictionary"],
		index:      15,
	}

	l.classes["MethodContext"] = &class{
		name:       "MethodContext",
		superclass: l.classes["Object"],
		members:    instVars(0, "method", "locals", "pc", "sender", "sp"),
		index:      16,
	}

	l.classes["CompiledMethod"] = &class{
		name:       "CompiledMethod",
		superclass: l.classes["Object"],
		members:    instVars(0, "bytecode", "literals", "selector", "class", "argc", "locals"),
		index:      17,
	}

	l.classes["BlockClosure"] = &class{
		name:       "BlockClosure",
		superclass: l.classes["Object"],
		members:    instVars(0, "ctx", "pc0", "argc", "argv", "handlerActive"),
		index:      18,
	}

	l.classes["Boolean"] = &class{
		name:       "Boolean",
		superclass: l.classes["Object"],
		index:      19,
	}
	l.classes["True"] = &class{
		name:       "True",
		superclass: l.classes["Boolean"],
		index:      20,
	}
	l.classes["False"] = &class{
		name:       "False",
		superclass: l.classes["Boolean"],
		index:      21,
	}

	l.classes["Magnitude"] = &class{
		name:       "Magnitude",
		superclass: l.classes["Object"],
		index:      22,
	}
	l.classes["Character"] = &class{
		name:       "Character",
		superclass: l.classes["Magnitude"],
		members:    instVars(0, "value"),
		index:      23,
	}
	l.classes["Number"] = &class{
		name:       "Number",
		superclass: l.classes["Magnitude"],
		index:      24,
	}
	l.classes["Integer"] = &class{
		name:       "Integer",
		superclass: l.classes["Number"],
		index:      25,
	}
	l.classes["SmallInteger"] = &class{
		name:       "SmallInteger",
		superclass: l.classes["Integer"],
		index:      26,
	}

	l.classes["Association"] = &class{
		name:       "Association",
		superclass: l.classes["Object"],
		members:    instVars(0, "key", "value"),
		index:      27,
	}

	l.classes["Process"] = &class{
		name:       "Process",
		superclass: l.classes["Object"],
		members:    instVars(0, "context", "next", "prev", "processTable"),
		index:      28,
	}
	l.classes["ProcessTable"] = &class{
		name:       "ProcessTable",
		superclass: l.classes["Object"],
		members:    instVars(0, "ready", "blocked", "nextProriity"),
		index:      28,
	}

	l.nextClassIndex = 29
	l.classIndexes = map[int]*class{}
	for _, cls := range l.classes {
		l.classIndexes[cls.index] = cls
	}
}

func NewSTListener() *STL {
	stl := &STL{
		cp:          newCompiler(),
		blockStarts: make([]int, 16),
		classes:     make(map[string]*class),
	}
	stl.pushScope()
	stl.initClasses()
	return stl
}

type Scope struct {
	vars     map[string]*cell
	parent   *Scope
	listener *STL
}

type cell struct {
	kind, index int
}

const (
	KindGlobal = iota
	KindClass
	KindLocal
	KindInstVar
	KindArg
)

func (s *Scope) lookup(name string) *cell {
	if c, ok := s.vars[name]; ok {
		return c
	}
	if s.parent != nil {
		return s.parent.lookup(name)
	}
	// Try to look it up in the classes list.
	if cls, ok := s.listener.classes[name]; ok {
		return &cell{KindClass, cls.index}
	}
	return nil
}

func instVars(start int, vars ...string) map[string]int {
	m := map[string]int{}
	for i := 0; i < len(vars); i++ {
		m[vars[i]] = i + start
	}
	return m
}

func (l *STL) popScope() {
	l.scope = l.scope.parent
}

func (l *STL) pushScope() {
	l.scope = &Scope{
		vars:     map[string]*cell{},
		parent:   l.scope,
		listener: l,
	}
}

func (l *STL) blockStart(pc int) {
	l.blockStarts[l.blockDepth] = pc
	l.blockDepth++
}

func (l *STL) blockStop(pc int) {
	l.blockDepth--
	start := l.blockStarts[l.blockDepth]

	// At the end of the block's code, rewrite the length at the block start.
	length := pc - start
	// Empty blocks would generate as simply [answer], but that's broken - there's
	// nothing on the stack to return. So [pushNil, answer].
	if length == 1 {
		l.cp.rewind(start)
		l.cp.pushNil()
		l.cp.answer()
		length = 2
	}

	// The length of a block is the word before its first bytecode.
	l.cp.method.bytecodes[start-1] = uint16(length)
}

func (l *STL) Error(err error) {
	fmt.Printf("Error: %v\n", err)
	os.Exit(1)
}

func (l *STL) compileWrite(name string) {
	c := l.scope.lookup(name)
	if c == nil {
		l.Error(fmt.Errorf("Unknown identifier: %s", name))
	}

	switch c.kind {
	case KindGlobal:
		l.Error(fmt.Errorf("Cannot write to global %s", name))
	case KindInstVar:
		l.cp.storeInstVar(c.index)
	case KindLocal:
		l.cp.storeLocal(c.index)
	case KindArg:
		l.Error(fmt.Errorf("Cannot store to method argument %s", name))
	}
}

// Method definition process: we parse a block of possibly many methods for the
// same (meta)class, and then get a series of EnterMethod calls with the
// selector, args and locals.
// The names of those fields are important to the compiler but only indexes are
// used in the kernel.
func (l *STL) EnterMethods(target *parser.MethodsTarget) {
	l.cp.startMethods(target.ClassName, target.ClassLevel)
	l.inMethods = true
	l.pushScope()
	cls := l.classes[target.ClassName]
	if cls != nil && cls.members != nil {
		for name, idx := range cls.members {
			l.scope.vars[name] = &cell{KindInstVar, idx}
		}
	}
}

func (l *STL) LeaveMethods() {
	l.cp.endMethods()
	l.inMethods = false
	l.popScope()
}

func (l *STL) EnterMethod(sig *parser.MessageSignature, locals []string) {
	l.cp.startMethod(sig.Symbol, len(sig.Params), len(locals))
	l.methodLocals = len(locals)
	l.methodLocalIndex = 1 + len(sig.Params) + l.methodLocals
	l.methodName = sig.Symbol

	// Set up the inner scope.
	l.pushScope()

	for i, v := range sig.Params {
		l.scope.vars[v] = &cell{KindArg, i + 1} // 0 is self.
	}
	for i, v := range locals {
		l.scope.vars[v] = &cell{KindLocal, 1 + i + len(sig.Params)}
	}

	l.answered = false
	l.first = true
}

func (l *STL) LeaveMethod() {
	// If there's already been an answer, that's it. If not, we need to add the
	// default answerSelf.
	if !l.answered {
		l.cp.answerSelf()
	}

	l.cp.endMethod()
}

func (l *STL) EnterReturn() {
	// Nothing to do here.
}

func (l *STL) LeaveReturn() {
	// Duplicate TOS and answer it.
	l.answered = true
	if l.blockDepth > 0 {
		l.cp.answerBlock()
	} else {
		l.cp.answer()
	}
}

func (l *STL) EnterAssignment() {
}

func (l *STL) LeaveAssignment(lhs string) {
	// Dupplicate and store TOS in the variable.
	l.cp.dup()
	l.compileWrite(lhs)
}

func (l *STL) EnterExprLine() {
	// Expressions leave the result on the stack, but if they occupy a whole line,
	// then they should be dropped.
	// If this is the first line of a block or method, however, this should not
	// be dropped.
	//
	// Additionally, statements at the top level need to be captured as singular
	// driver statements.
	if !l.inMethods {
		l.cp.startTopLevel()
	} else if l.first {
		l.first = false
	} else {
		l.cp.drop()
	}
}

func (l *STL) LeaveExprLine() {
	if !l.inMethods {
		l.cp.answer() // Ends the pseudo-method.
		l.cp.endTopLevel()
	}
}

func (l *STL) EnterCascade() {
}

func (l *STL) LeaveCascade() {
}

func (l *STL) EnterKeywordSend() {
}

func (l *STL) LeaveKeywordSend(keywords []string) {
	// If the list is empty, nothing to actually send.
	// If there are keywords, concatenate and send.
	if keywords == nil || len(keywords) == 0 {
		return
	}

	combined := strings.Join(keywords, ":") + ":"
	l.cp.send(l.superSend, len(keywords), combined)
	l.superSend = false

	// We need to detect class creation sends to subclass: etc. and get the
	// instance variables.
	switch combined {
	case "subclass:instanceVariableNames:classVariableNames:":
		// These look like: pushGlobal(superclass), pushLiteral(classNameSymbol),
		// pushString("instanceVars"), pushString("classVars"), send("...")
		bcs := l.cp.method.bytecodes[l.cp.method.pc-5 : l.cp.method.pc]
		l.recordSubclass(bcs[0], bcs[1], bcs[2], bcs[3])
	case "subclass:instanceVariableNames:":
		// These look like: pushGlobal(superclass), pushLiteral(symbol),
		// pushString("instanceVars"), send("...")
		bcs := l.cp.method.bytecodes[l.cp.method.pc-4 : l.cp.method.pc]
		l.recordSubclass(bcs[0], bcs[1], bcs[2], 0xffff)
	case "subclass:":
		// These look like: pushGlobal("superclass"), pushLiteral(symbol), send.
		bcs := l.cp.method.bytecodes[l.cp.method.pc-3 : l.cp.method.pc]
		l.recordSubclass(bcs[0], bcs[1], 0xffff, 0xffff)
	}
}

func (l *STL) recordSubclass(superBC, classBC, instVarsBC, classVarsBC uint16) {
	var superclassStr string
	if (superBC & 0xff00) == 0x0400 { // Inlined class index
		superclassStr = l.classIndexes[int(superBC&0xff)].name
	} else {
		superclassStr = string(l.cp.method.literals[superBC&0xff].(symbolLit))
	}
	classStr := string(l.cp.method.literals[classBC&0xff].(symbolLit))
	instVars := map[string]int{}
	classVars := map[string]int{}

	if instVarsBC != 0xffff {
		instVarsStr := l.cp.method.literals[instVarsBC&0xff]
		for i, v := range strings.Split(string(instVarsStr.(stringLit)), " ") {
			instVars[v] = i
		}
	}

	if classVarsBC != 0xffff {
		classVarsStr := l.cp.method.literals[classVarsBC&0xff]
		for i, v := range strings.Split(string(classVarsStr.(stringLit)), " ") {
			classVars[v] = i
		}
	}

	l.classes[classStr+" class"] = &class{
		name:       classStr + " class",
		superclass: l.classes[superclassStr+" class"],
		members:    classVars,
		index:      l.nextClassIndex,
	}
	l.classes[classStr] = &class{
		name:       classStr,
		superclass: l.classes[superclassStr],
		members:    instVars,
		index:      l.nextClassIndex + 1,
	}
	l.nextClassIndex += 2
	l.scope.vars[classStr] = &cell{KindGlobal, 0}
}

func (l *STL) EnterBinarySends() {
}

func (l *STL) LeaveBinarySends() {
}

func (l *STL) EnterBinarySend() {
}

func (l *STL) LeaveBinarySend(selector string) {
	l.cp.send(l.superSend, 1, selector)
	l.superSend = false
}

func (l *STL) EnterUnarySends() {
}

func (l *STL) VisitUnarySend(selector string) {
	l.cp.send(l.superSend, 0, selector)
	l.superSend = false
}

func (l *STL) LeaveUnarySends() {
}

func (l *STL) EnterUnit() {
}

func (l *STL) LeaveUnit() {
}

func (l *STL) EnterBlock(params, locals []string) {
	argStart := l.methodLocalIndex
	l.cp.startBlock(len(params), len(locals), argStart, 0)
	l.blockStart(l.cp.method.pc) // Points to the first bytecode.

	// Nest a scope - the
	l.pushScope()
	for i, v := range params {
		l.scope.vars[v] = &cell{KindArg, l.methodLocalIndex + i}
	}
	for i, v := range locals {
		l.scope.vars[v] = &cell{KindLocal, l.methodLocalIndex + len(params) + i}
	}

	l.methodLocals += len(params) + len(locals)
	l.methodLocalIndex += len(params) + len(locals)
	l.answered = false
	l.first = true
}

func (l *STL) LeaveBlock() {
	// Blocks return the topmost value on the stack if they don't have a ^.
	// If they do have a caret, we never reach this point.
	if !l.answered {
		l.cp.answer()
	}
	l.answered = false
	l.blockStop(l.cp.method.pc)
	l.popScope()
}

func (l *STL) EnterConstArray() {
	// TODO Constant arrays
	panic("Constant arrays are not yet supported")
}

func (l *STL) LeaveConstArray() {
}

func (l *STL) EnterDynArray() {
	// Dynamic arrays are created with a series of message sends.
	// After being created, they get DUPed repeatedly for expressions.
	l.cp.pushGlobal("Array")
	l.cp.send(false, 0, "new")
}

func (l *STL) VisitArrayElement() {
	// add: to the array, then DUP the array for future elements.
	l.cp.send(false, 1, "add:")
}

func (l *STL) LeaveDynArray() {
}

func (l *STL) VisitIdentifier(id *parser.Ident) {
	// Special cases: self, super, thisContext, nil, true and false.
	if id.Text == "self" {
		l.cp.pushSelf()
		return
	}
	if id.Text == "super" {
		l.cp.pushSelf()
		l.superSend = true
		return
	}
	if id.Text == "nil" {
		l.cp.pushNil()
		return
	}
	if id.Text == "thisContext" {
		l.cp.pushContext()
		return
	}
	if id.Text == "true" {
		l.cp.pushBool(true)
		return
	}
	if id.Text == "false" {
		l.cp.pushBool(false)
		return
	}

	// Try to look it up in the scope.
	spec := l.scope.lookup(id.Text)
	if spec == nil {
		if unicode.IsUpper([]rune(id.Text)[0]) {
			// Assume this is a class value, and compile a SystemDictionary at: sym.
			l.cp.pushGlobal("SystemDictionary")
			l.cp.pushString(id.Text)
			l.cp.send(false, 0, "asSymbol")
			l.cp.send(false, 1, "at:")
			return
		}

		// If we're still here, this is an unknown local identifier.
		l.Error(fmt.Errorf("%s: unknown identifier: %s", id.SourceLoc(), id.Text))
	}

	switch spec.kind {
	case KindGlobal:
		l.cp.pushGlobal(id.Text)
	case KindClass:
		l.cp.pushClass(spec.index)
	case KindLocal:
		l.cp.pushLocal(spec.index)
	case KindArg:
		l.cp.pushLocal(spec.index)
	case KindInstVar:
		l.cp.pushInstVar(spec.index)
	default:
		panic("unhandled kind of scope value")
	}
}

func (l *STL) VisitPrimitive(keyword string, index int) {
	if keyword != "primitive" {
		l.Error(fmt.Errorf("Only primitives with 'primitive' are allowed (got %s)", keyword))
	}
	l.cp.primitive(index)
	l.answered = true // Primitives all return something already.
}

func (l *STL) VisitSymbol(sym *parser.Symbol) {
	l.cp.pushSymbol(sym.Str)
}

func (l *STL) VisitStringLit(str *parser.StringLit) {
	l.cp.pushString(str.Str)
}

func (l *STL) VisitCharLit(ch *parser.CharLit) {
	l.cp.pushCharacter(ch.Ch)
}

func (l *STL) VisitNumber(num *parser.Number) {
	// Two cases for numbers: those with alternative bases are limited to integral
	// parts. Those with base 10 can have more parts.
	n, err := strconv.ParseInt(num.Integral, num.Base, 32)
	if err != nil {
		l.Error(fmt.Errorf("Malformed number: %v", err))
	}
	if num.Negative {
		n = -n
	}

	l.cp.pushNumber(int(n))
}
