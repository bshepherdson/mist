package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/shepheb/mist/parser"
)

// Smalltalk listener, consuming the parser's stream of changes.
type STL struct {
	cp                        *Compiler
	methodName                string
	methodStart, methodLocals int

	blockDepth  int
	blockStarts []int

	answered, superSend bool
	scope               *Scope
	classes             map[string]*class
}

type class struct {
	name       string
	superclass *class
	members    map[string]int
}

func (l *STL) initClasses() {
	l.classes["Object"] = &class{name: "Object"}
	l.classes["Behavior"] = &class{
		name:       "Behavior",
		superclass: l.classes["Object"],
	}
	l.classes["ClassDescription"] = &class{
		name:       "ClassDescription",
		superclass: l.classes["Behavior"],
		members:    instVars("name", "superclass", "instanceVariables"),
	}
	l.classes["Class"] = &class{
		name:       "Class",
		superclass: l.classes["ClassDescription"],
	}
	l.classes["Metaclass"] = &class{
		name:       "Metaclass",
		superclass: l.classes["ClassDescription"],
	}
	l.classes["String"] = &class{
		name:       "String",
		superclass: l.classes["Object"],
	}
	l.classes["NullObject"] = &class{
		name:       "NullObject",
		superclass: l.classes["Object"],
	}
	l.classes["Boolean"] = &class{
		name:       "Boolean",
		superclass: l.classes["Object"],
	}
	l.classes["True"] = &class{
		name:       "True",
		superclass: l.classes["Boolean"],
	}
	l.classes["False"] = &class{
		name:       "False",
		superclass: l.classes["Boolean"],
	}

	l.scope.vars["Object"] = &cell{KindGlobal, 0}
	l.scope.vars["Behavior"] = &cell{KindGlobal, 0}
	l.scope.vars["Class"] = &cell{KindGlobal, 0}
	l.scope.vars["ClassDescription"] = &cell{KindGlobal, 0}
	l.scope.vars["Metaclass"] = &cell{KindGlobal, 0}
	l.scope.vars["String"] = &cell{KindGlobal, 0}
	l.scope.vars["NullObject"] = &cell{KindGlobal, 0}
	l.scope.vars["Boolean"] = &cell{KindGlobal, 0}
	l.scope.vars["True"] = &cell{KindGlobal, 0}
	l.scope.vars["False"] = &cell{KindGlobal, 0}
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
	vars   map[string]*cell
	parent *Scope
}

type cell struct {
	kind, index int
}

const (
	KindGlobal = iota
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
	return nil
}

func instVars(vars ...string) map[string]int {
	m := map[string]int{}
	for i := 0; i < len(vars); i++ {
		m[vars[i]] = i
	}
	return m
}

func (l *STL) popScope() {
	l.scope = l.scope.parent
}

func (l *STL) pushScope() {
	l.scope = &Scope{
		vars:   map[string]*cell{},
		parent: l.scope,
	}
}

func (l *STL) blockStart(pc int) {
	l.blockStarts[l.blockDepth] = pc
	l.blockDepth++
}

func (l *STL) blockStop(pc int) {
	l.blockDepth--
	start := l.blockStarts[l.blockDepth]
	// At the end of the method's code, rewrite the length literal at pc - 2.
	l.cp.bytecodes[start].Length = pc - start
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
//
// EnterMethods looks up the (meta)class by name and leaves it on the stack.
// EnterMethod DUPs the class, then does:
//
//   CompiledMethod argc: aNumber locals: aNumber length: inBytecodes.
//   ... bytecodes follow ...
//   theClass method: theMethod selector: aSymbol
//
// Method.pcStart points at the first of the method's bytecodes; the literal
// length is two before it (the send, then that pushNumber).
//
// The second send consumes the DUPed class, but leaves the one under it.
// LeaveMethod has to look up the length (which is a pushNumber(0) initially)
// and adjust it to the actual length in bytecodes.
// LeaveMethods DROPs that trailing class.
//
// This is followed by the length of bytecodes, which that constructor consumes.
// The PC then points after the method's code. The method then gets attached to
// its target class as
//   theClass method: aMethod selector: aSymbol

func (l *STL) EnterMethods(target *parser.MethodsTarget) {
	// Push the class by name and leave it on the stack throughout defining
	// methods on it.
	l.cp.pushGlobal(target.ClassName)
	className := target.ClassName

	if target.ClassLevel {
		l.cp.send(false, 0, "class")
		className = className + " class"
	}

	l.pushScope()
	cls := l.classes[className]
	if cls != nil && cls.members != nil {
		for name, idx := range cls.members {
			l.scope.vars[name] = &cell{KindInstVar, idx}
		}
	}
}

func (l *STL) LeaveMethods() {
	l.cp.drop() // The target class which is on the stack to have methods added.
	l.popScope()
}

func (l *STL) EnterMethod(sig *parser.MessageSignature, locals []string) {
	// The class is on the stack at the start.
	l.cp.pushGlobal("CompiledMethod")
	l.cp.pushNumber(float64(len(sig.Params)))
	l.cp.pushNumber(float64(len(locals)))
	l.cp.pushNumber(0)
	l.cp.send(false, 3, "argc:locals:length:")
	// ( cls method )

	l.methodLocals = len(locals)
	l.methodName = sig.Symbol

	// Set up the inner scope.
	l.pushScope()

	for i, v := range sig.Params {
		l.scope.vars[v] = &cell{KindArg, i + 1} // 0 is self.
	}
	for i, v := range locals {
		l.scope.vars[v] = &cell{KindLocal, 1 + i + len(sig.Params)}
	}

	l.methodStart = l.cp.pc
	l.answered = false
}

func (l *STL) LeaveMethod() {
	// If there's already been an answer, that's it. If not, we need to add the
	// default answerSelf.
	if !l.answered {
		l.cp.answerSelf()
	}

	// At the end of the method's code, rewrite the length literal at pc - 2.
	//fmt.Printf("Method started at %d, now at %d: %v\n", l.methodStart, l.cp.pc, l.cp.bytecodes[l.methodStart-2])
	l.cp.bytecodes[l.methodStart-2].Value = float64(l.cp.pc - l.methodStart)
	// And the locals count at pc - 3.
	l.cp.bytecodes[l.methodStart-3].Value = float64(l.methodLocals)

	// After the method, it will be on the stack as the result of calling
	//   CompiledMethod argc: c locals: t length: len
	// which returns self (the method).
	// We've already got the target class on the stack; send it
	//   aClass method: aMethod selector: aSymbol
	l.cp.pushString(l.methodName)
	l.cp.send(false, 0, "asSymbol")         // ( cls method symbol )
	l.cp.send(false, 2, "method:selector:") // ( cls ) - it answers self
	// This closes the loop for each method: they start and end with the class on
	// the stack. LeaveMethods will DROP that final value.
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
}

func (l *STL) LeaveExprLine() {
	// Expressions leave the result on the stack, but if they occupy a whole line,
	// then they should be dropped.
	// However, if it's already been answered, this won't run and should be
	// dropped.
	if !l.answered {
		l.cp.drop()
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
		// These look like: pushGlobal("superclass"), pushString("symbol"),
		// send("asSymbol"), pushString("instanceVars"), pushString("classVars"),
		// send("...")
		bcs := l.cp.bytecodes[l.cp.pc-6 : l.cp.pc]
		//fmt.Printf("bcs[0]: %v\n", bcs[0])
		//fmt.Printf("bcs[1]: %v\n", bcs[1])
		//fmt.Printf("bcs[3]: %v\n", bcs[3])
		//fmt.Printf("bcs[4]: %v\n", bcs[4])
		l.recordSubclass(bcs[0], bcs[1], bcs[3], bcs[4])
	case "subclass:instanceVariableNames:":
		// These look like: pushGlobal("superclass"), pushString("symbol"),
		// send("asSymbol"), pushString("instanceVars"), send("...")
		bcs := l.cp.bytecodes[l.cp.pc-5 : l.cp.pc]
		//fmt.Printf("bcs[0]: %v\n", bcs[0])
		//fmt.Printf("bcs[1]: %v\n", bcs[1])
		//fmt.Printf("bcs[3]: %v\n", bcs[3])
		l.recordSubclass(bcs[0], bcs[1], bcs[3], nil)
	case "subclass:":
		// These look like: pushGlobal("superclass"), pushString("symbol"),
		// send("asSymbol"), send("...")
		bcs := l.cp.bytecodes[l.cp.pc-4 : l.cp.pc]
		//fmt.Printf("bcs[0]: %v\n", bcs[0])
		//fmt.Printf("bcs[1]: %v\n", bcs[1])
		l.recordSubclass(bcs[0], bcs[1], nil, nil)
	}
}

func (l *STL) recordSubclass(superBC, classBC, instVarsBC, classVarsBC *Bytecode) {
	superclass := superBC.Name
	classSym := classBC.Name
	instVars := map[string]int{}
	if instVarsBC != nil {
		for i, v := range strings.Split(instVarsBC.Name, " ") {
			instVars[v] = i
		}
	}
	classVars := map[string]int{}
	if classVarsBC != nil {
		for i, v := range strings.Split(classVarsBC.Name, " ") {
			classVars[v] = i
		}
	}

	//fmt.Printf("%s subclass: %s instVars: %v classVars: %v\n",
	//superclass, classSym, instVars, classVars)

	l.classes[classSym] = &class{
		name:       classSym,
		superclass: l.classes[superclass],
		members:    instVars,
	}
	l.classes[classSym+" class"] = &class{
		name:       classSym + " class",
		superclass: l.classes[superclass+" class"],
		members:    classVars,
	}
	l.scope.vars[classSym] = &cell{KindGlobal, 0}
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
	l.cp.startBlock(l.methodLocals, len(params), 0)
	l.methodLocals += len(params) + len(locals)
	l.blockStart(l.cp.pc) // Points to the first bytecode.
	l.answered = false
}

func (l *STL) LeaveBlock() {
	// Blocks return the topmost value on the stack if they don't have a ^.
	// If they do have a caret, we never reach this point.
	if !l.answered {
		l.cp.answer()
	}
	l.answered = false
	l.blockStop(l.cp.pc)
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
	l.cp.dup()
}

func (l *STL) VisitArrayElement() {
	// add: to the array, then DUP the array for future elements.
	l.cp.send(false, 1, "add:")
	l.cp.dup()
}

func (l *STL) LeaveDynArray() {
	// There's two copies of the array on the stack, so drop one and we're done.
	l.cp.drop()
}

func (l *STL) VisitIdentifier(id *parser.Ident) {
	// Special cases: self and super.
	if id.Text == "self" {
		l.cp.pushSelf()
		return
	}
	if id.Text == "super" {
		l.cp.pushSelf()
		l.superSend = true
		return
	}

	// Try to look it up in the scope.
	spec := l.scope.lookup(id.Text)
	if spec == nil {
		// Assume this is a class value, and compile a SystemDictionary at: sym.
		l.cp.pushGlobal("SystemDictionary")
		l.cp.pushString(id.Text)
		l.cp.send(false, 0, "asSymbol")
		l.cp.send(false, 1, "at:")
		return
	}

	switch spec.kind {
	case KindGlobal:
		l.cp.pushGlobal(id.Text)
	case KindLocal:
		l.cp.pushLocal(spec.index)
	case KindArg:
		l.cp.pushLocal(spec.index)
	case KindInstVar:
		l.cp.pushInstVar(spec.index)
	}
}

func (l *STL) VisitPrimitive(keyword, parameter string) {
	if keyword != "builtin" {
		l.Error(fmt.Errorf("Only primitives with 'builtin' are allowed (got %s)", keyword))
	}
	l.cp.primitive(keyword+":", parameter)
	l.answered = true // Primitives all return something already.
}

func (l *STL) VisitSymbol(sym *parser.Symbol) {
	// This is compiled as a literal string and asSymbol.
	l.cp.pushString(sym.Str)
	l.cp.send(false, 0, "asSymbol")
}

func (l *STL) VisitStringLit(str *parser.StringLit) {
	l.cp.pushString(str.Str)
}

func (l *STL) VisitCharLit(ch *parser.CharLit) {
	l.cp.pushGlobal("Character")
	l.cp.pushString(string([]rune{ch.Ch}))
	l.cp.send(false, 1, "fromString:")
}

func (l *STL) VisitNumber(num *parser.Number) {
	// Two cases for numbers: those with alternative bases are limited to integral
	// parts. Those with base 10 can have more parts.
	var value float64
	var err error
	if num.Base == 10 {
		s := num.Integral
		if num.Floating != "" {
			s += "." + num.Floating
		}
		if num.Exp != "" {
			s += "e" + num.Exp
		}
		value, err = strconv.ParseFloat(s, 64)
	} else {
		n, e := strconv.ParseInt(num.Integral, num.Base, 32)
		value, err = float64(n), e
	}

	if err != nil {
		l.Error(fmt.Errorf("Malformed number: %v", err))
	}
	if num.Negative {
		value = -value
	}

	l.cp.pushNumber(value)
}
