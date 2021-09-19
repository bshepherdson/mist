package parser

import (
	"fmt"
	"io"
	"strings"
)

// Grammar is broadly based on this syntax description, but structured a bit
// more carefully.
//
// The output is listener-style - entering and exiting each rule calls into the
// listener's methods.
//

type Parser struct {
	lexer *Lexer
	out   Listener
}

func NewParser(filename string, reader io.RuneReader, listener Listener) *Parser {
	return &Parser{
		lexer: &Lexer{
			loc:     &Loc{file: filename, line: 1, col: 0},
			scanner: &scanner{reader: reader, buffered: make([]rune, 2, 2)},
		},
		out: listener,
	}
}

func (p *Parser) Debug(d bool) {
	p.lexer.debug = d
}

func (p *Parser) Parse() {
	// The top level is a stream of singular expressions with periods, and method
	// creations with bangs.
	for {
		t := p.lexer.Advance()
		if eof(t) {
			return // All done!
		}

		if t.Id() == TBang {
			p.parseMethods()
		} else {
			p.lexer.Rewind(t)
			p.parseTopExpr()
		}
	}
}

func (p *Parser) unexpected(got Token, expected ...string) {
	p.out.Error(fmt.Errorf("%s: unexpected %s: expected %s", got.SourceLoc(),
		got.Desc(), strings.Join(expected, " or ")))
}

// Consumes and returns a token, erroring if it's not the one we expected.
func (p *Parser) expect(id TokenId, human string) Token {
	t := p.lexer.Advance()
	if t.Id() != id {
		p.unexpected(t, human)
		return nil
	}
	return t
}

func ident(t Token) string {
	if t.Id() != TIdent && t.Id() != TKeyword {
		panic(fmt.Sprintf("tried to ident() a non-Ident: %v", t))
	}
	return t.(*Ident).Text
}

// ! ident 'class'? ! (method !)+ !
func (p *Parser) parseMethods() {
	// Already consumed the first bang.
	methods := &MethodsTarget{}
	className := p.expect(TIdent, "identifier")
	if className == nil {
		return
	}
	methods.ClassName = ident(className)

	t := p.lexer.Advance()
	if t.Id() == TIdent && ident(t) == "class" {
		methods.ClassLevel = true
		t = p.lexer.Advance()
	}

	if t.Id() != TBang {
		p.unexpected(t, "!")
		return
	}

	p.out.EnterMethods(methods)

	// Following is (method !)+, so consume methods until we see an extra bang.
	for {
		p.parseMethod() // Includes the trailing !

		t := p.lexer.Advance()
		if t.Id() == TBang {
			break
		}
		p.lexer.Rewind(t)
	}

	p.out.LeaveMethods()
}

// Starts on the selector, consumes that and the code, and the trailing !
func (p *Parser) parseMethod() {
	signature := p.parseSignature()
	if signature == nil {
		return
	}

	// Possibly | locals |
	var locals []string
	t := p.lexer.Advance()
	if t.Id() == TPipe {
		locals = p.parseLocals()
		t = p.lexer.Advance()
	}

	// Either way t is the next token.
	p.out.EnterMethod(signature, locals)

	// Then 0 or more lines of code, ending with dots or a bang.
	if t.Id() != TBang {
		p.lexer.Rewind(t)
		//fmt.Printf("Parsing exprs\n")
		p.parseExprs()
		//fmt.Printf("Done Parsing exprs\n")

		if p.expect(TBang, "! at end of method's code") == nil {
			return
		}
	}

	p.out.LeaveMethod()
}

// Parses a chain of expressions, expects either a ! or ] at the end but does
// *not* consume it!
func (p *Parser) parseExprs() {
	last := false
	for {
		// First, check for possible caret, since those are only allowed in final
		// position.
		t := p.lexer.Advance()
		//fmt.Printf("\tTop of exprs loop: %v\n", t)
		if t == nil {
			p.out.Error(fmt.Errorf("unexpected EOF in expr block"))
			return
		}
		if t.Id() == TCaret {
			p.out.EnterReturn()
			p.parseExpr()
			p.out.LeaveReturn()
			last = true
		} else {
			p.lexer.Rewind(t)
			p.parseExprLine()
		}

		// If last is set, only bang or ] to end is allowed.
		t = p.lexer.Advance()
		//fmt.Printf("\tAfter expr: %v\n", t)
		if t.Id() == TBang || t.Id() == TBlockClose {
			p.lexer.Rewind(t)
			break
		}
		if last {
			p.unexpected(t, "! or ] at end of method")
			return
		}

		if t.Id() != TDot {
			p.unexpected(t, "dot", "!", "]")
			return
		}

		// Otherwise we've consumed the dot and it's time to go again.
		// TODO Trailing dots allowed? I don't think so.
	}
}

// Three flavours of signature.
// - Unary, just an identifier
// - Binary, binop and identifier
// - Keywords, (keyword ident)+
func (p *Parser) parseSignature() *MessageSignature {
	// See if the next token is a binary operation.
	t := p.lexer.Advance()
	switch t.Id() {
	case TBinary:
		return p.parseBinarySignature(t.(*BinOp))
	case TIdent:
		return &MessageSignature{Symbol: ident(t)}
	case TKeyword:
		p.lexer.Rewind(t)
		return p.parseKeywordSignature()
	default:
		p.unexpected(t, "identifier", "keyword", "binary operation")
		return nil
	}
}

func (p *Parser) parseBinarySignature(bin *BinOp) *MessageSignature {
	param := p.expect(TIdent, "parameter name")
	if param == nil {
		return nil
	}
	return &MessageSignature{
		Symbol: bin.Op,
		Params: []string{ident(param)},
	}
}

func (p *Parser) parseKeywordSignature() *MessageSignature {
	// Keyword message, (keyword ident)+
	var params, keywords []string
	for {
		keyword := p.lexer.Advance()
		if keyword == nil || keyword.Id() != TKeyword {
			p.lexer.Rewind(keyword)
			break
		}
		param := p.expect(TIdent, "parameter name")
		if param == nil {
			return nil
		}

		keywords = append(keywords, ident(keyword))
		params = append(params, ident(param))
	}

	// When we land down here, the lexer state is back to normal and we should
	// have matching keywords and params.
	if len(keywords) != len(params) {
		panic("can't happen: mismatching keywords and params")
	}
	return &MessageSignature{
		Symbol: strings.Join(keywords, ":") + ":",
		Params: params,
	}
}

func (p *Parser) parseTopExpr() {
	p.parseExprLine()
	p.expect(TDot, "dot")
}

// Parses an expression on a whole line. Does not leave anything on the stack.
func (p *Parser) parseExprLine() {
	// First possibility, primitives: <keyword: string>
	t1 := p.lexer.Advance()
	if t1 != nil && t1.Id() == TBinary && t1.(*BinOp).Op == "<" {
		// Keyword, colon and string.
		keyword := p.expect(TKeyword, "keyword")
		str := p.expect(TString, "string literal")
		gt := p.expect(TBinary, "> closing bracket")
		if keyword == nil || str == nil || gt == nil || gt.(*BinOp).Op != ">" {
			p.out.Error(fmt.Errorf("Malformed primitive: got %v %v %v\n", keyword, str, gt))
			return
		}
		p.out.VisitPrimitive(ident(keyword), str.(*StringLit).Str)
		return
	}

	p.lexer.Rewind(t1)
	p.out.EnterExprLine()
	p.parseExpr()
	p.out.LeaveExprLine()
}

// Parses an inner expression, leaving it on the stack.
func (p *Parser) parseExpr() {
	t1 := p.lexer.Advance()
	t2 := p.lexer.Advance()

	if t1 != nil && t1.Id() == TIdent && t2 != nil && t2.Id() == TAssign {
		p.out.EnterAssignment()
		p.parseExpr()
		p.out.LeaveAssignment(ident(t1))
		return
	}

	// Otherwise, begin a cascade and nest in.
	p.lexer.Rewind(t2)
	p.lexer.Rewind(t1)
	p.out.EnterCascade()
	p.parseKeywordSend()

	t := p.lexer.Advance()
	if t.Id() == TSemi {
		// TODO Cascades are useful
		panic("cascades are not supported")
	}
	p.lexer.Rewind(t)
	p.out.LeaveCascade()
}

func (p *Parser) parseKeywordSend() {
	p.out.EnterKeywordSend()
	p.parseBinarySends()

	// Now all the binary sends are done, so look for a keyword.
	t := p.lexer.Advance()
	p.lexer.Rewind(t)
	if t.Id() != TKeyword {
		p.out.LeaveKeywordSend(nil)
		return
	}

	// Have to actually parse the keyword expression.
	var keywords []string
	for {
		t = p.lexer.Advance()
		//fmt.Printf("\tKeyword send loop: %v\n", t)
		if t.Id() != TKeyword {
			p.lexer.Rewind(t)
			break
		}
		keywords = append(keywords, ident(t))
		p.parseBinarySends()
	}

	p.out.LeaveKeywordSend(keywords)
}

// Parses a series of binary message sends, possibly 0 binary sends.
func (p *Parser) parseBinarySends() {
	// A primary, followed by 0 or more binary messages.
	p.out.EnterBinarySends()

	// The left-hand side comes first.
	p.parseUnarySends()

	// Then there may be a binary operation, or something else.
	for {
		t := p.lexer.Advance()
		if t == nil || t.Id() != TBinary {
			p.lexer.Rewind(t)
			break
		}

		// Arguments need to get pushed before the call happens, so we parse the
		// primary first and send the operation symbol when leaving.
		p.out.EnterBinarySend()
		p.parseUnarySends()
		p.out.LeaveBinarySend(t.(*BinOp).Op)
	}

	p.out.LeaveBinarySends()
}

// Parses a receiver (some literal or var) and 0 or more unary message sends.
func (p *Parser) parseUnarySends() {
	p.out.EnterUnarySends()

	// Parse the innermost receiver.
	p.parseUnit()

	// Now parse 0 or more unary sends - these are simply identifiers.
	for {
		t := p.lexer.Advance()
		if t == nil || t.Id() != TIdent {
			p.lexer.Rewind(t)
			break
		}

		p.out.VisitUnarySend(ident(t))
	}

	p.out.LeaveUnarySends()
}

// Parses a "unit" - a singular value that serves as a receiver.
// Several types: identifiers, literals, blocks, arrays, subexpressions
func (p *Parser) parseUnit() {
	p.out.EnterUnit()
	t := p.lexer.Advance()
	switch t.Id() {
	case TParenOpen:
		// Complete subexpression, then a closing paren.
		p.parseExpr()
		if closeParen := p.expect(TParenClose, "closing )"); closeParen == nil {
			return
		}
	case TIdent:
		p.out.VisitIdentifier(t.(*Ident)) // Might be self or super; compiler's job.
	case TBlockOpen:
		p.parseBlock()
	case TBraceOpen:
		p.parseDynArray()
	default:
		// Bunch of cases here, so it's factored out for clarity.
		p.parseLiteral(t)
	}
	p.out.LeaveUnit()
}

// Literals in several forms:
// numbers, strings, char constants, symbol constants, constant arrays.
// TODO binding and eval forms?
func (p *Parser) parseLiteral(t Token) {
	switch t.Id() {
	case TNumber:
		p.out.VisitNumber(t.(*Number))
	case TString:
		p.out.VisitStringLit(t.(*StringLit))
	case TChar:
		p.out.VisitCharLit(t.(*CharLit))
	case TSymbol:
		p.out.VisitSymbol(t.(*Symbol))
	case THash:
		// Two possible things here: #[byte array], #(constant array).
		// Symbols like #at:put: are already handled above.
		t2 := p.lexer.Advance()
		switch t2.Id() {
		case TBlockOpen: // #[byte array]
			p.out.Error(fmt.Errorf("byte arrays are not supported"))
		case TParenOpen: // #(const array)
			p.parseConstArray()
		default:
			p.unexpected(t2, "#[byte array]", "#(constant array)")
		}
	case TBinary:
		// If this is - it's allowed.
		if t.(*BinOp).Op == "-" {
			t2 := p.lexer.Advance()
			if t2.Id() == TNumber {
				n := t2.(*Number)
				n.Negative = !n.Negative
				p.out.VisitNumber(n)
				return
			}
		}
		p.unexpected(t, "literal value")
	}
}

// Parses a block [ :params* | (| locals... |)? exprs ]
// That's 0 or more params, a bar if there's any, optional | locals+ | list,
// and nonempty exprs.
// NB: The initial [ is already consumed.
func (p *Parser) parseBlock() {
	// First, optional parameters. Check if the next char is a colon.

	var params []string
	t := p.lexer.Advance()
	for ; t != nil && t.Id() == TColon; t = p.lexer.Advance() {
		param := p.expect(TIdent, "block parameter")
		if param == nil {
			return
		}
		params = append(params, ident(param))
	}

	// Three possibilities for t now:
	// - If params is empty, it's the first token in the rest of the block.
	// - If params is NOT empty, there must be a bar, then the rest of the block.
	if params != nil && len(params) > 0 {
		if t.Id() != TPipe {
			p.unexpected(t, "pipe after block parameters")
			return
		}
		// Otherwise, consume that separator and continue.
		t = p.lexer.Advance()
	}

	// Now t is the next token after the parameters section.
	// If it's a pipe, there are locals.
	var locals []string
	if t.Id() == TPipe {
		locals = p.parseLocals() // Consumes the final | too.
		t = p.lexer.Advance()
	}

	p.out.EnterBlock(params, locals)
	if t.Id() == TBlockClose {
		p.out.LeaveBlock()
		return
	}
	p.lexer.Rewind(t)

	// Now its the expression chain.
	p.parseExprs()
	p.expect(TBlockClose, "] at end of block")
	p.out.LeaveBlock()
}

// Parses a block of | local variables |, where the opening | is already
// consumed.
// Consumes the closing |
func (p *Parser) parseLocals() []string {
	var locals []string
	t := p.lexer.Advance()
	for ; t != nil && t.Id() == TIdent; t = p.lexer.Advance() {
		locals = append(locals, ident(t))
	}
	// t is now the next token, which must be the pipe or it's an error.
	if t == nil || t.Id() != TPipe {
		p.unexpected(t, "closing | after locals")
		return nil
	}
	return locals
}

// Parses a #(constant array) where the # and ( have already been consumed.
// Consumes the final ) as well.
// The contents are space-separated literals, inner arays, and symbols.
func (p *Parser) parseConstArray() {
	p.lexer.constArrayDepth++
	p.out.EnterConstArray()

	for {
		t := p.lexer.Advance()
		switch t.Id() {
		case TParenOpen:
			// Nest into another inner constant array.
			p.parseConstArray()
		case TParenClose:
			// End of this array, break
			break
		case TSymbol:
			// Symbols (no hash on these)
			p.out.VisitSymbol(t.(*Symbol))
		default:
			p.parseLiteral(t)
		}
	}

	p.out.LeaveConstArray()
	p.lexer.constArrayDepth--
}

// { exprs } with dots just like the body of a method or block.
func (p *Parser) parseDynArray() {
	p.out.EnterDynArray()

	// Can't just call parseExprs(), since it allows ^ returns and such.
	// We just repeatedly parse either expr . or expr }.

	// First a check for an empty array.
	t := p.lexer.Advance()
	if t.Id() != TBraceClose {
		p.lexer.Rewind(t)

		for {
			p.parseExpr()
			p.out.VisitArrayElement()
			t = p.lexer.Advance()
			if t.Id() == TBraceClose {
				break
			} else if t.Id() != TDot {
				p.unexpected(t, "dot", "closing }")
				return
			}
			// Dot, so continue
		}
	}

	p.out.LeaveDynArray()
}

// Represents the header of a method: its selector string and list of argument
// names.
type MessageSignature struct {
	Symbol string
	Params []string
}

// The target of a "methods" block, eg. ! ClassName class? ! selector code... !!
// Gives the class name and whether it's at the class level or not.
type MethodsTarget struct {
	ClassName  string
	ClassLevel bool
}
