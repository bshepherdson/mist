package main

import (
	"fmt"
	"strings"

	participle "github.com/alecthomas/participle/v2"
	"github.com/alecthomas/participle/v2/lexer"
)

var lex = lexer.MustSimple([]lexer.Rule{
	{"CharacterConstant", "\\$[\\[\\]\\-0-9a-zA-Z!@#$%^&*()+=|'\":;,.<>?/~`]", nil},
	{"DynArrStart", "{", nil},
	{"DynArrEnd", "}", nil},
	{"DynDictStart", "@{", nil},
	{"LitArrStart", `#\(`, nil},
	{"Hash", "#", nil},
	{"Assignment", ":=", nil},
	{"Colon", ":", nil},
	{"Bang", "!", nil},
	{"Carrot", `\^`, nil},
	{"ExpE", `e`, nil},
	{"Ident", `[a-zA-Z]+[a-zA-Z0-9_]*`, nil},
	{"Hex", `16r[0-9a-fA-F]+`, nil},
	{"Integer", `[0-9]+`, nil},
	{"ReservedWord", `nil|true|false|self|super`, nil},
	{"BinarySelector", `[\+*/=,@%~&?]`, nil},
	{"LT", `<`, nil},
	{"GT", `>`, nil},
	{"Minus", `-`, nil},
	{"Semicolon", `;`, nil},
	{"Period", `\.`, nil},
	{"Pipe", `\|`, nil},
	{"OpenParen", `\(`, nil},
	{"CloseParen", `\)`, nil},
	{"BlockStart", `\[`, nil},
	{"BlockEnd", `\]`, nil},
	{"comment", `".*?"`, nil},
	{"String", `'.*?'`, nil},
	{"Whitespace", "[ \t\r]+", nil},
	{"WsOneLine", "[ \t]+", nil},
	{"Newline", "\n", nil},
})

type PScript struct {
	Statements *PStatements `Whitespace? @@ Whitespace? EOF`
}

func (p *PScript) String() string {
	return p.Statements.String()
}

type PStatements struct {
	Statements []*PExpression `@@ Whitespace? (Period Whitespace? @@)* Whitespace? Period? Whitespace?`
}

func (p *PStatements) String() string {
	var b strings.Builder
	for _, stmt := range p.Statements {
		fmt.Fprintf(&b, "  %s.\n", stmt.String())
	}
	return b.String()
}

type PMethod struct {
	Header *PMethodHeader `Bang WsOneLine? @@`
	Seq    *PSequence     `Whitespace? @@ Whitespace? Bang Whitespace?`
}

func (p *PMethod) String() string {
	return p.Header.String() + "\n" + p.Seq.String()
}

type PMethodHeader struct {
	Keywords []*PKeywordArg `(@@ |`
	Binary   *PBinary       `@@ |`
	Unary    string         `@Ident) WsOneLine? Newline`
}

func (p *PMethodHeader) String() string {
	if p.Keywords != nil {
		var b strings.Builder
		for _, kw := range p.Keywords {
			fmt.Fprintln(&b, kw.String())
		}
		return b.String()
	}
	if p.Binary != nil {
		return p.Binary.String()
	}
	return " " + p.Unary
}

type PKeywordArg struct {
	Keyword   string `@Ident Colon Whitespace?`
	Parameter string `@Ident`
}

func (p *PKeywordArg) String() string {
	return p.Keyword + ": " + p.Parameter
}

type PBinary struct {
	Selector  string `(@Minus | @LT | @GT | @Pipe | @BinarySelector)+`
	Parameter string `WsOneLine? @Ident`
}

func (p *PBinary) String() string {
	return p.Selector + " " + p.Parameter
}

type PUnary struct {
	Selector string  `@Ident Whitespace?`
	Tail     *PUnary `@@?`
}

func (p *PUnary) String() string {
	if p.Tail == nil {
		return p.Selector
	}
	return p.Selector + " " + p.Tail.String()
}

type PSequence struct {
	Temps []string         `((Pipe (Whitespace? @Ident)+ Whitespace? Pipe)? Whitespace?`
	Block *PStatementBlock `@@?)!`
}

func (p *PSequence) String() string {
	var pre string
	if p.Temps != nil {
		pre = "|" + strings.Join(p.Temps, " ") + "|"
	}
	return pre + " " + p.Block.String()
}

type PStatementBlock struct {
	Statements []*PExpression `((@@ (Period @@)*)?`
	Answer     *PExpression   `(Carrot Whitespace? @@ Whitespace? Period?)?)!`
}

func (p *PStatementBlock) String() string {
	var b strings.Builder
	for _, stmt := range p.Statements {
		fmt.Fprintln(&b, stmt.String())
	}
	if p.Answer != nil {
		fmt.Fprintln(&b, p.Answer)
	}
	return b.String()
}

type PExpression struct {
	Assignment *PAssignment `(@@ |`
	Primitive  *PPrimitive  `@@ |`
	Send       *PSend       `@@ Whitespace?`
	Cascade    []*PMessage  `(Semicolon Whitespace? @@ Whitespace?)*)`
}

func (p *PExpression) String() string {
	if p.Assignment != nil {
		return p.Assignment.String()
	}
	if p.Primitive != nil {
		return p.Primitive.String()
	}
	var b strings.Builder
	fmt.Fprintln(&b, p.Send.String())
	for _, cascade := range p.Cascade {
		fmt.Fprintf(&b, ";\n  %s", cascade.String())
	}
	return b.String()
}

type PAssignment struct {
	Variable   string       `@Ident Whitespace? Assignment`
	Expression *PExpression `Whitespace? @@`
}

func (p *PAssignment) String() string {
	return p.Variable + " := " + p.Expression.String()
}

type PMessage struct {
	Unary    *PUnary        `@@ |`
	Binary   *PBinary       `@@ |`
	Keywords []*PKeywordArg `@@+`
}

func (p *PMessage) String() string {
	if p.Unary != nil {
		return p.Unary.String()
	}
	if p.Binary != nil {
		return p.Binary.String()
	}
	var b strings.Builder
	for _, kw := range p.Keywords {
		fmt.Fprintf(&b, " %s", kw.String())
	}
	return b.String()
}

type PPrimitive struct {
	Keyword string `LT Whitespace? @Ident Colon Whitespace?`
	Value   string `@String Whitespace? GT`
}

func (p *PPrimitive) String() string {
	return fmt.Sprintf("<%s: \"%s\">", p.Keyword, p.Value)
}

type PSend struct {
	Operand      *POperand      `@@ Whitespace?`
	UnaryMethods []string       `(@Ident Whitespace?)*`
	BinaryCalls  []*PBinary     `(@@ Whitespace?)*`
	KeywordCalls []*PKeywordArg `@@*`
}

func (p *PSend) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "%s ", p.Operand)
	if p.UnaryMethods != nil {
		fmt.Fprintf(&b, "%s ", strings.Join(p.UnaryMethods, " "))
	}

	for _, binary := range p.BinaryCalls {
		fmt.Fprintf(&b, "%s ", binary.String())
	}

	for _, kw := range p.KeywordCalls {
		fmt.Fprintf(&b, "%s ", kw.String())
	}
	return b.String()
}

type POperand struct {
	Method    *PMethod     `(@@ |`
	Literal   *PLiteral    `@@ |`
	Reference string       `@Ident |`
	Expr      *PExpression `OpenParen Whitespace? @@ Whitespace? CloseParen)`
}

func (p *POperand) String() string {
	if p.Method != nil {
		return p.Method.String()
	}
	if p.Literal != nil {
		return p.Literal.String()
	}
	if p.Reference != "" {
		return p.Reference
	}
	return p.Expr.String()
}

type PLiteral struct {
	Runtime  *PRuntimeLiteral `@@ |`
	Compiled *PParsedLiteral  `@@`
}

func (p *PLiteral) String() string {
	if p.Runtime != nil {
		return p.Runtime.String()
	}
	return p.Compiled.String()
}

type PRuntimeLiteral struct {
	DictPairs   []*PExpression `(DynDictStart Whitespace? (@@ (Period @@)*)? Whitespace? DynArrEnd |`
	ArrayValues []*PExpression `DynArrStart Whitespace? (@@ (Period @@)*)? Whitespace? DynArrEnd |`
	Block       *PBlock        `@@)`
}

func (p *PRuntimeLiteral) String() string {
	if p.Block != nil {
		return p.Block.String()
	}
	starter := "@{"
	exprs := p.DictPairs
	if exprs == nil {
		starter = "{"
		exprs = p.ArrayValues
	}

	var b strings.Builder
	fmt.Fprintf(&b, "%s ", starter)
	for _, expr := range exprs {
		fmt.Fprintf(&b, "%s. ", expr.String())
	}
	fmt.Fprintf(&b, "}")
	return b.String()
}

type PParsedLiteral struct {
	PseudoVar    string         `@ReservedWord |`
	Number       *PNumber       `@@ |`
	Char         string         `@CharacterConstant |`
	LiteralArray *PLiteralArray `@@ |`
	StringLit    string         `@String |`
	Symbol       *PSymbol       `Hash @@`
}

func (p *PParsedLiteral) String() string {
	if p.PseudoVar != "" {
		return p.PseudoVar
	}
	if p.Number != nil {
		return p.Number.String()
	}
	if p.Char != "" {
		return "$" + p.Char
	}
	if p.LiteralArray != nil {
		return p.LiteralArray.String()
	}
	if p.StringLit != "" {
		return "'" + p.StringLit + "'"
	}
	return p.Symbol.String()
}

type PSymbol struct {
	BinarySelector string   `(@Minus | @LT | @GT | @Pipe | @BinarySelector)+ |`
	Ident          string   `@Ident |`
	Keywords       []string `(@Ident Colon)+ |`
	StringLit      string   `@String`
}

func (p *PSymbol) String() string {
	if p.BinarySelector != "" {
		return "#" + p.BinarySelector
	}
	if p.Ident != "" {
		return "#" + p.Ident
	}
	if p.Keywords != nil {
		return "#" + strings.Join(p.Keywords, ":") + ":"
	}
	return "#'" + p.StringLit + "'"
}

type PLiteralArray struct {
	Values []*PLiteralArrayValue `LitArrStart Whitespace? (@@ Whitespace?)* CloseParen`
}

func (p *PLiteralArray) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "#( ")
	for _, value := range p.Values {
		fmt.Fprintf(&b, "%s ", value.String())
	}
	fmt.Fprintf(&b, ")")
	return b.String()
}

type PLiteralArrayValue struct {
	Literal  *PParsedLiteral       `@@ |`
	Subarray []*PLiteralArrayValue `(OpenParen Whitespace? (@@ Whitespace?)* CloseParen) |`
	Symbol   *PSymbol              `@@`
}

func (p *PLiteralArrayValue) String() string {
	if p.Literal != nil {
		return p.Literal.String()
	}
	if p.Symbol != nil {
		return p.Symbol.String()
	}

	var b strings.Builder
	fmt.Fprintf(&b, "( ")
	for _, val := range p.Subarray {
		fmt.Fprintf(&b, "%s ", val.String())
	}
	fmt.Fprintf(&b, ")")
	return b.String()
}

type PNumber struct {
	Minus   bool    `@Minus?`
	Hex     string  `(@Hex |`
	Exp     *PExp   `@@ |`
	Float   *PFloat `@@ |`
	Integer uint    `@Integer)`
}

func (p *PNumber) String() string {
	var s string
	if p.Hex != "" {
		s = "16r" + p.Hex
	} else if p.Exp != nil {
		s = p.Exp.String()
	} else if p.Float != nil {
		s = p.Float.String()
	} else {
		s = fmt.Sprintf("%d", p.Integer)
	}

	if p.Minus {
		return "-" + s
	}
	return s
}

// Tail
type PBlock struct {
	Params []string   `BlockStart ((Colon @Ident Whitespace?)+ Pipe)? Whitespace?`
	Seq    *PSequence `@@? Whitespace? BlockEnd`
}

func (p *PBlock) String() string {
	var b strings.Builder
	fmt.Fprintf(&b, "[ ")
	if p.Params != nil {
		for _, param := range p.Params {
			fmt.Fprintf(&b, ":%s ", param)
		}
		fmt.Fprintf(&b, "| ")
	}

	return b.String() + p.Seq.String() + " ]"
}

type PFloat struct {
	Whole    uint `@Integer Period`
	Fraction uint `@Integer`
}

func (p *PFloat) String() string {
	return fmt.Sprintf("%d.%d", p.Whole, p.Fraction)
}

type PExp struct {
	Float   *PFloat `(@@ |`
	Integer uint    `@Integer) "e"`
	Exp     uint    `@Integer`
}

func (p *PExp) String() string {
	var base string
	if p.Float != nil {
		base = p.Float.String()
	} else {
		base = fmt.Sprintf("%d", p.Integer)
	}
	return fmt.Sprintf("%se%d", base, p.Exp)
}

var parser = participle.MustBuild(&PScript{},
	participle.Lexer(lex),
)

func main() {
	ast := &PScript{}
	err := parser.ParseString("", `a := 7. b := bar`,
		//`"Builds the basics of the collection hierarchy.
		//Collection
		//  HashedCollection      Wraps a raw Javascript map; uses raw string keys.
		//    Dictionary            Any Smalltalk value as the key, using #hash.
		//  Array                 Native array of Smalltalk values.
		//"
		//
		//Object subclass: #Collection.
		//`
		ast)
	if err != nil {
		fmt.Printf("%v", err)
	} else {
		fmt.Println(ast.String())
	}
}
