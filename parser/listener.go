package parser

type Listener interface {
	EnterMethods(*MethodsTarget)
	LeaveMethods()

	EnterMethod(sig *MessageSignature, locals []string)
	LeaveMethod()

	EnterReturn()
	LeaveReturn()

	EnterAssignment()
	LeaveAssignment(string)

	// An expression on its own line, leaving nothing on the stack.
	EnterExprLine()
	LeaveExprLine()

	// An inner expression, a single message send. No assignments, etc.
	EnterCascade()
	LeaveCascade()

	EnterKeywordSend()
	LeaveKeywordSend(keywords []string)

	EnterBinarySends()
	LeaveBinarySends()

	EnterBinarySend()
	LeaveBinarySend(selector string)

	EnterUnarySends()
	VisitUnarySend(selector string)
	LeaveUnarySends()

	// TODO These may be uninteresting?
	EnterUnit()
	LeaveUnit()

	EnterBlock(params, locals []string)
	LeaveBlock()

	EnterConstArray()
	LeaveConstArray()
	EnterDynArray()
	VisitArrayElement()
	LeaveDynArray()

	VisitIdentifier(*Ident)
	VisitPrimitive(keyword string, number int)
	VisitSymbol(*Symbol)
	VisitStringLit(*StringLit)
	VisitCharLit(*CharLit)
	VisitNumber(*Number)

	Error(error)
}
