package parser

import "os"

// Dummy Listener that just silently does nothing.
type dummyListener struct {
}

func NewDummyListener() Listener {
	return &dummyListener{}
}

func (l *dummyListener) Error(err error) {
	os.Exit(1)
}

func (l *dummyListener) EnterMethods(target *MethodsTarget) {
}

func (l *dummyListener) LeaveMethods() {
}

func (l *dummyListener) EnterMethod(sig *MessageSignature, locals []string) {
}

func (l *dummyListener) LeaveMethod() {
}

func (l *dummyListener) EnterReturn() {
}

func (l *dummyListener) LeaveReturn() {
}

func (l *dummyListener) EnterAssignment() {
}

func (l *dummyListener) LeaveAssignment(lhs string) {
}

func (l *dummyListener) EnterExprLine() {
}

func (l *dummyListener) LeaveExprLine() {
}

func (l *dummyListener) EnterCascade() {
}

func (l *dummyListener) LeaveCascade() {
}

func (l *dummyListener) EnterKeywordSend() {
}

func (l *dummyListener) LeaveKeywordSend(keywords []string) {
}

func (l *dummyListener) EnterBinarySends() {
}

func (l *dummyListener) LeaveBinarySends() {
}

func (l *dummyListener) EnterBinarySend() {
}

func (l *dummyListener) LeaveBinarySend(selector string) {
}

func (l *dummyListener) EnterUnarySends() {
}

func (l *dummyListener) VisitUnarySend(selector string) {
}

func (l *dummyListener) LeaveUnarySends() {
}

func (l *dummyListener) EnterUnit() {
}

func (l *dummyListener) LeaveUnit() {
}

func (l *dummyListener) EnterBlock(params, locals []string) {
}

func (l *dummyListener) LeaveBlock() {
}

func (l *dummyListener) EnterConstArray() {
}

func (l *dummyListener) LeaveConstArray() {
}

func (l *dummyListener) EnterDynArray() {
}

func (l *dummyListener) LeaveDynArray() {
}

func (l *dummyListener) VisitArrayElement() {
}

func (l *dummyListener) VisitIdentifier(id *Ident) {
}

func (l *dummyListener) VisitPrimitive(keyword, parameter string) {
}

func (l *dummyListener) VisitSymbol(sym *Symbol) {
}

func (l *dummyListener) VisitStringLit(str *StringLit) {
}

func (l *dummyListener) VisitCharLit(ch *CharLit) {
}

func (l *dummyListener) VisitNumber(num *Number) {
}
