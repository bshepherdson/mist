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
	fmt.Printf("Error: %v", err)
	l.inner.Error(err)
}

func (l *debugListener) EnterMethods(target *MethodsTarget) {
	fmt.Printf("EnterMethods: %s (class %t)", target.ClassName, target.ClassLevel)
	l.inner.EnterMethods(target)
}

func (l *debugListener) LeaveMethods() {
	fmt.Printf("LeaveMethods")
	l.inner.LeaveMethods()
}

func (l *debugListener) EnterMethod(sig *MessageSignature) {
	fmt.Printf("EnterMethod: %s %v", sig.Symbol, sig.Params)
	l.inner.EnterMethod(sig)
}

func (l *debugListener) LeaveMethod() {
	fmt.Printf("LeaveMethod")
	l.inner.LeaveMethod()
}

func (l *debugListener) EnterReturn() {
	fmt.Printf("EnterReturn")
	l.inner.EnterReturn()
}

func (l *debugListener) LeaveReturn() {
	fmt.Printf("LeaveReturn")
	l.inner.LeaveReturn()
}

func (l *debugListener) EnterAssignment() {
	fmt.Printf("EnterAssignment")
	l.inner.EnterAssignment()
}

func (l *debugListener) LeaveAssignment(lhs string) {
	fmt.Printf("LeaveAssignment(%s)", lhs)
	l.inner.LeaveAssignment(lhs)
}

func (l *debugListener) EnterCascade() {
	fmt.Printf("EnterCascade")
	l.inner.EnterCascade()
}

func (l *debugListener) LeaveCascade() {
	fmt.Printf("LeaveCascade")
	l.inner.LeaveCascade()
}

func (l *debugListener) EnterKeywordSend() {
	fmt.Printf("EnterKeywordSend")
	l.inner.EnterKeywordSend()
}

func (l *debugListener) LeaveKeywordSend(keywords []string) {
	fmt.Printf("LeaveKeywordSend: %s", strings.Join(keywords, ":"))
	l.inner.LeaveKeywordSend(keywords)
}

func (l *debugListener) EnterBinarySends() {
	fmt.Printf("EnterBinarySends")
	l.inner.EnterBinarySends()
}

func (l *debugListener) LeaveBinarySends() {
	fmt.Printf("LeaveBinarySends")
	l.inner.LeaveBinarySends()
}

func (l *debugListener) EnterBinarySend() {
	fmt.Printf("EnterBinarySend")
	l.inner.EnterBinarySend()
}

func (l *debugListener) LeaveBinarySend(selector string) {
	fmt.Printf("LeaveBinarySend: %s", selector)
	l.inner.LeaveBinarySend(selector)
}

func (l *debugListener) EnterUnarySends() {
	fmt.Printf("EnterUnarySends")
	l.inner.EnterUnarySends()
}

func (l *debugListener) VisitUnarySend(selector string) {
	fmt.Printf("VisitUnarySend: %s", selector)
	l.inner.VisitUnarySend(selector)
}

func (l *debugListener) LeaveUnarySends() {
	fmt.Printf("LeaveUnarySends")
	l.inner.LeaveUnarySends()
}

func (l *debugListener) EnterUnit() {
	fmt.Printf("EnterUnit")
	l.inner.EnterUnit()
}

func (l *debugListener) LeaveUnit() {
	fmt.Printf("LeaveUnit")
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

	fmt.Printf("EnterBlock: params<%s> locals<%s>", sParams, sLocals)
	l.inner.EnterBlock(params, locals)
}

func (l *debugListener) LeaveBlock() {
	fmt.Printf("LeaveBlock")
	l.inner.LeaveBlock()
}

func (l *debugListener) EnterConstArray() {
	fmt.Printf("EnterConstArray")
	l.inner.EnterConstArray()
}

func (l *debugListener) LeaveConstArray() {
	fmt.Printf("LeaveConstArray")
	l.inner.LeaveConstArray()
}

func (l *debugListener) EnterDynArray() {
	fmt.Printf("EnterDynArray")
	l.inner.EnterDynArray()
}

func (l *debugListener) LeaveDynArray() {
	fmt.Printf("LeaveDynArray")
	l.inner.LeaveDynArray()
}

func (l *debugListener) VisitIdentifier(id *Ident) {
	fmt.Printf("VisitIdentifier: %s", ident(id))
	l.inner.VisitIdentifier(id)
}

func (l *debugListener) VisitPrimitive(keyword, parameter string) {
	fmt.Printf("VisitPrimitive %s: %s", keyword, parameter)
	l.inner.VisitPrimitive(keyword, parameter)
}

func (l *debugListener) VisitSymbol(sym *Symbol) {
	fmt.Printf("VisitSymbol %s", sym.Str)
	l.inner.VisitSymbol(sym)
}

func (l *debugListener) VisitStringLit(str *StringLit) {
	fmt.Printf("VisitStringLit '%s'", str.Str)
	l.inner.VisitStringLit(str)
}

func (l *debugListener) VisitCharLit(ch *CharLit) {
	fmt.Printf("VisitCharLit $%v", ch.Ch)
	l.inner.VisitCharLit(ch)
}

func (l *debugListener) VisitNumber(num *Number) {
	fmt.Printf("VisitNumber (neg %t) %dr%s.%se%s",
		num.Negative, num.Base, num.Integral, num.Floating, num.Exp)
	l.inner.VisitNumber(num)
}
