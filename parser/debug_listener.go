package parser

import (
	"fmt"
	"strings"
)

// Wrapping Listener that logs everything as it goes.
type debugListener struct {
	inner Listener
}

func NewDebugListener(inner Listener) Listener {
	return &debugListener{inner}
}

func (l *debugListener) Error(err error) {
	fmt.Printf("Error: %v\n", err)
	l.inner.Error(err)
}

func (l *debugListener) EnterMethods(target *MethodsTarget) {
	fmt.Printf("EnterMethods: %s (class %t)\n", target.ClassName, target.ClassLevel)
	l.inner.EnterMethods(target)
}

func (l *debugListener) LeaveMethods() {
	fmt.Printf("LeaveMethods\n")
	l.inner.LeaveMethods()
}

func (l *debugListener) EnterMethod(sig *MessageSignature, locals []string) {
	fmt.Printf("EnterMethod: %s %v [%s]\n", sig.Symbol, sig.Params, strings.Join(locals, " "))
	l.inner.EnterMethod(sig, locals)
}

func (l *debugListener) LeaveMethod() {
	fmt.Printf("LeaveMethod\n")
	l.inner.LeaveMethod()
}

func (l *debugListener) EnterReturn() {
	fmt.Printf("EnterReturn\n")
	l.inner.EnterReturn()
}

func (l *debugListener) LeaveReturn() {
	fmt.Printf("LeaveReturn\n")
	l.inner.LeaveReturn()
}

func (l *debugListener) EnterAssignment() {
	fmt.Printf("EnterAssignment\n")
	l.inner.EnterAssignment()
}

func (l *debugListener) LeaveAssignment(lhs string) {
	fmt.Printf("LeaveAssignment(%s)\n", lhs)
	l.inner.LeaveAssignment(lhs)
}

func (l *debugListener) EnterExprLine() {
	fmt.Printf("EnterExprLine\n")
	l.inner.EnterExprLine()
}

func (l *debugListener) LeaveExprLine() {
	fmt.Printf("LeaveExprLine\n")
	l.inner.LeaveExprLine()
}

func (l *debugListener) EnterCascade() {
	fmt.Printf("EnterCascade\n")
	l.inner.EnterCascade()
}

func (l *debugListener) LeaveCascade() {
	fmt.Printf("LeaveCascade\n")
	l.inner.LeaveCascade()
}

func (l *debugListener) EnterKeywordSend() {
	fmt.Printf("EnterKeywordSend\n")
	l.inner.EnterKeywordSend()
}

func (l *debugListener) LeaveKeywordSend(keywords []string) {
	fmt.Printf("LeaveKeywordSend: %s\n", strings.Join(keywords, ":"))
	l.inner.LeaveKeywordSend(keywords)
}

func (l *debugListener) EnterBinarySends() {
	fmt.Printf("EnterBinarySends\n")
	l.inner.EnterBinarySends()
}

func (l *debugListener) LeaveBinarySends() {
	fmt.Printf("LeaveBinarySends\n")
	l.inner.LeaveBinarySends()
}

func (l *debugListener) EnterBinarySend() {
	fmt.Printf("EnterBinarySend\n")
	l.inner.EnterBinarySend()
}

func (l *debugListener) LeaveBinarySend(selector string) {
	fmt.Printf("LeaveBinarySend: %s\n", selector)
	l.inner.LeaveBinarySend(selector)
}

func (l *debugListener) EnterUnarySends() {
	fmt.Printf("EnterUnarySends\n")
	l.inner.EnterUnarySends()
}

func (l *debugListener) VisitUnarySend(selector string) {
	fmt.Printf("VisitUnarySend: %s\n", selector)
	l.inner.VisitUnarySend(selector)
}

func (l *debugListener) LeaveUnarySends() {
	fmt.Printf("LeaveUnarySends\n")
	l.inner.LeaveUnarySends()
}

func (l *debugListener) EnterUnit() {
	fmt.Printf("EnterUnit\n")
	l.inner.EnterUnit()
}

func (l *debugListener) LeaveUnit() {
	fmt.Printf("LeaveUnit\n")
	l.inner.LeaveUnit()
}

func (l *debugListener) EnterBlock(params, locals []string) {
	sParams := ""
	sLocals := ""
	if params != nil && len(params) > 0 {
		sParams = strings.Join(params, " ")
	}
	if locals != nil && len(locals) > 0 {
		sLocals = strings.Join(locals, " ")
	}

	fmt.Printf("EnterBlock: params<%s> locals<%s>\n", sParams, sLocals)
	l.inner.EnterBlock(params, locals)
}

func (l *debugListener) LeaveBlock() {
	fmt.Printf("LeaveBlock\n")
	l.inner.LeaveBlock()
}

func (l *debugListener) EnterConstArray() {
	fmt.Printf("EnterConstArray\n")
	l.inner.EnterConstArray()
}

func (l *debugListener) LeaveConstArray() {
	fmt.Printf("LeaveConstArray\n")
	l.inner.LeaveConstArray()
}

func (l *debugListener) EnterDynArray() {
	fmt.Printf("EnterDynArray\n")
	l.inner.EnterDynArray()
}

func (l *debugListener) LeaveDynArray() {
	fmt.Printf("LeaveDynArray\n")
	l.inner.LeaveDynArray()
}

func (l *debugListener) VisitArrayElement() {
	fmt.Printf("VisitArrayElement\n")
	l.inner.VisitArrayElement()
}

func (l *debugListener) VisitIdentifier(id *Ident) {
	fmt.Printf("VisitIdentifier: %s\n", ident(id))
	l.inner.VisitIdentifier(id)
}

func (l *debugListener) VisitPrimitive(keyword, parameter string) {
	fmt.Printf("VisitPrimitive %s: %s\n", keyword, parameter)
	l.inner.VisitPrimitive(keyword, parameter)
}

func (l *debugListener) VisitSymbol(sym *Symbol) {
	fmt.Printf("VisitSymbol %s\n", sym.Str)
	l.inner.VisitSymbol(sym)
}

func (l *debugListener) VisitStringLit(str *StringLit) {
	fmt.Printf("VisitStringLit '%s'\n", str.Str)
	l.inner.VisitStringLit(str)
}

func (l *debugListener) VisitCharLit(ch *CharLit) {
	fmt.Printf("VisitCharLit $%v\n", ch.Ch)
	l.inner.VisitCharLit(ch)
}

func (l *debugListener) VisitNumber(num *Number) {
	fmt.Printf("VisitNumber (neg %t) %dr%s.%se%s\n",
		num.Negative, num.Base, num.Integral, num.Floating, num.Exp)
	l.inner.VisitNumber(num)
}
