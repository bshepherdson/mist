package parser

type Listener interface {
	EnterMethods(*MethodsTarget)
	LeaveMethods()

	EnterMethod(*MessageSignature)
	LeaveMethod()

	EnterReturn()
	LeaveReturn()

	EnterAssignment()
	LeaveAssignment(string)

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
	LeaveDynArray()

	VisitIdentifier(*Ident)
	VisitPrimitive(keyword, value string)
	VisitSymbol(*Symbol)
	VisitStringLit(*StringLit)
	VisitCharLit(*CharLit)
	VisitNumber(*Number)

	Error(error)
}
