package parser

import (
	"fmt"
	"strconv"
)

// Lexer for Smalltalk, used by the parser in parser.go.
// Its main API is Advance(), Rewind(), and AdvanceUntil().

type TokenId int

type Token interface {
	Id() TokenId
	SourceLoc() *Loc
	String() string
	Desc() string // Human-friendly short description, eg. "identifier", "number"
}

type Lexer struct {
	buffer   [2]Token
	buffered int
	loc      *Loc
	scanner  *scanner

	// Unfortunately the lexer needs a bit of state - inside a #(constant array)
	// symbols containing colons are allowed.
	// This value is set by the parser dynamically, as soon as it consumes the
	// opening #( of the array, and cleared when we leave it.
	constArrayDepth int
}

func (l *Lexer) Advance() Token {
	if l.buffered > 0 {
		ret := l.buffer[l.buffered]
		l.buffered--
		return ret
	}

	t, err := l.scan()
	if err != nil {
		return &Eof{}
	}
	return t
}

func (l *Lexer) Rewind(token Token) {
	if token == nil {
		// This happens sometimes; just ignore it.
		return
	}

	if l.buffered >= 2 {
		panic("can't rewind more than 2 step")
	}
	l.buffer[l.buffered] = token
	l.buffered++
}

func (l *Lexer) AdvanceUntil(id TokenId) []Token {
	// I'm actually not sure whether this is more useful if it returns up to a
	// thing but doesn't consume it, or consumes over it.
	// I'll leave it as a stub for now and see how I need to use to
	panic("implement me")
}

const (
	TIdent TokenId = iota
	TNumber
	TChar
	TString
	TKeyword
	TSymbol

	TColon
	TCaret
	TBang
	TPipe
	THash
	TDot
	TAssign

	TBinary

	TBlockOpen
	TBlockClose
	TParenOpen
	TParenClose
	TBraceOpen
	TBraceClose

	TEof
)

func (l *Lexer) read() (rune, error) {
	r, eof := l.scanner.read()
	if eof {
		return rune(0), eofError
	}
	l.loc.bump(r)
	return r, nil
}

func (l *Lexer) peek() (rune, error) {
	r, eof := l.scanner.lookahead()
	if eof {
		return rune(0), eofError
	}
	return r, nil
}

func isWhitespace(r rune) bool {
	return r == ' ' || r == '\t' || r == '\r' || r == '\n'
}

func eof(t Token) bool {
	return t.Id() == TEof
}

// The main entry point for scanning a new token.
// White space is skipped; the only places where whitespace is significant is
// in separating adjacent identifiers.
func (l *Lexer) scan() (Token, error) {
	loc := *l.loc // Copy
	r, err := l.read()
	if err != nil {
		return nil, err
	}

	if isWhitespace(r) {
		return l.scan()
	}

	if t, ok := singletons[r]; ok {
		return &singleton{id: t, loc: &loc, ch: r}, nil
	}

	// If we're still here, it's a more complex token.
	if r == '$' {
		return l.scanCharLiteral(&loc)
	} else if r == '"' {
		l.skipComment()
		return l.scan()
	} else if r == ':' {
		// Might be a singleton colon, or might be an assignment.
		if next, eof := l.scanner.lookahead(); !eof && next == '=' {
			l.scanner.read()
			// Slight abuse.
			return &singleton{id: TAssign, loc: &loc, ch: next}, nil
		}
		return &singleton{id: TColon, loc: &loc, ch: r}, nil
	} else if r == '\'' {
		return l.scanString(&loc)
	} else if r == '#' {
		return l.scanHash(&loc)
	} else if _, ok := binchars[r]; ok {
		return l.scanBinary(&loc, r)
	} else if identStart(r) {
		l.scanner.rewind(r)
		return l.scanIdent(&loc)
	} else if digit(r) {
		l.scanner.rewind(r)
		return l.scanNumber(&loc)
	} else {
		panic("failed to scan")
	}
}

func (l *Lexer) skipComment() {
	l.scanner.readWhile(func(r rune) bool {
		return r != '"'
	})
	// Scanner is now on the quote, which we also want to consume.
	l.scanner.read()
}

type singleton struct {
	id  TokenId
	loc *Loc
	ch  rune
}

// Single-character tokens go in a map for quick checks.
var singletons map[rune]TokenId = map[rune]TokenId{
	'^': TCaret,
	'!': TBang,
	'|': TPipe,
	'.': TDot,
	'[': TBlockOpen,
	']': TBlockClose,
	'(': TParenOpen,
	')': TParenClose,
	'{': TBraceOpen,
	'}': TBraceClose,
}

func (s *singleton) Id() TokenId {
	return s.id
}
func (s *singleton) SourceLoc() *Loc {
	return s.loc
}
func (s *singleton) String() string {
	return string(s.ch)
}
func (s *singleton) Desc() string {
	return string(s.ch)
}

func (l *Lexer) scanCharLiteral(loc *Loc) (Token, error) {
	r, err := l.read()
	if err != nil {
		return nil, err
	}
	return &CharLit{r, loc}, nil
}

type CharLit struct {
	Ch  rune
	loc *Loc
}

func (c *CharLit) Id() TokenId {
	return TChar
}

func (c *CharLit) SourceLoc() *Loc {
	return c.loc
}

func (c *CharLit) String() string {
	return "$" + string(c.Ch)
}

func (c *CharLit) Desc() string {
	return "char literal"
}

func (l *Lexer) scanHash(loc *Loc) (Token, error) {
	// Might be #(...) or #[...], then we emit THash alone.
	// But if it's followed by ' or identifier characters, we need to parse the
	// whole symbol or string literal.
	r, eof := l.scanner.lookahead()
	if eof {
		return nil, eofError
	}

	if r == '\'' {
		l.scanner.read() // Skip opening
		s := string(l.scanner.readWhile(nonQuote))
		return &Symbol{Str: s, loc: loc}, nil
	} else if r == '[' || r == '(' {
		return &singleton{id: THash, loc: loc, ch: '#'}, nil
	} else if identStart(r) {
		s := string(l.scanner.readWhile(symbolContinue))
		return &Symbol{Str: s, loc: loc}, nil
	}
	return nil, fmt.Errorf("Unexpected %v after #", r)
}

type Symbol struct {
	Str string
	loc *Loc
}

func (s *Symbol) Id() TokenId {
	return TSymbol
}

func (s *Symbol) SourceLoc() *Loc {
	return s.loc
}

func (s *Symbol) String() string {
	return "Symbol<" + s.Str + ">"
}

func (s *Symbol) Desc() string {
	return "symbol"
}

type Eof struct {
	loc *Loc
}

func (e *Eof) Id() TokenId {
	return TEof
}
func (e *Eof) SourceLoc() *Loc {
	return e.loc
}
func (e *Eof) String() string {
	return "<EOF>"
}
func (e *Eof) Desc() string {
	return "EOF"
}

// Binary operations
func (l *Lexer) scanBinary(loc *Loc, r rune) (Token, error) {
	// Binary operators are 1 or 2 characters long, so check the second one, and
	// if it's also a binchar they get combined.
	next, err := l.peek()
	s := string(r)
	if err != nil {
		return &BinOp{s, loc}, nil
	}

	if _, ok := binchars[next]; ok {
		s += string(next)
	}

	return &BinOp{s, loc}, nil
}

type BinOp struct {
	Op  string
	loc *Loc
}

func (b *BinOp) Id() TokenId {
	return TBinary
}

func (b *BinOp) SourceLoc() *Loc {
	return b.loc
}

func (b *BinOp) String() string {
	return b.Op
}

func (b *BinOp) Desc() string {
	return "binary selector"
}

var binchars map[rune]bool = map[rune]bool{
	'+':  true,
	'-':  true,
	'*':  true,
	'/':  true,
	'~':  true,
	'|':  true,
	',':  true,
	'<':  true,
	'>':  true,
	'=':  true,
	'&':  true,
	'@':  true,
	'?':  true,
	'%':  true,
	'\\': true,
}

// Identifiers
func (l *Lexer) scanIdent(loc *Loc) (Token, error) {
	runes := l.scanner.readWhile(identContinue)
	if len(runes) == 0 {
		panic("can't happen - first character was already checked")
	}
	keyword := false
	if r, eof := l.scanner.lookahead(); !eof && r == ':' {
		l.scanner.read()
		keyword = true
	}

	return &Ident{keyword, string(runes), loc}, nil
}

type Ident struct {
	Keyword bool
	Text    string
	loc     *Loc
}

func (i *Ident) Id() TokenId {
	if i.Keyword {
		return TKeyword
	}
	return TIdent
}

func (i *Ident) SourceLoc() *Loc {
	return i.loc
}

func (i *Ident) String() string {
	if i.Keyword {
		return "Keyword<" + i.Text + ":>"
	}
	return "Ident<" + i.Text + ">"
}

func (i *Ident) Desc() string {
	if i.Keyword {
		return "keyword"
	}
	return "identifier"
}

// TODO These don't really support Unicode identifiers!
func identStart(r rune) bool {
	return ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || r == '_'
}

func identContinue(r rune) bool {
	return identStart(r) || digit(r)
}

func symbolContinue(r rune) bool {
	return identContinue(r) || r == ':'
}

func digit(r rune) bool {
	return '0' <= r && r <= '9'
}

type Number struct {
	Base     int
	Negative bool
	Integral string
	Floating string
	Exp      string
	loc      *Loc
}

func (n *Number) Id() TokenId {
	return TNumber
}

func (n *Number) SourceLoc() *Loc {
	return n.loc
}

func (n *Number) String() string {
	base := ""
	if n.Base != 10 {
		base = fmt.Sprintf("%dr", n.Base)
	}

	neg := ""
	if n.Negative {
		neg = "-"
	}

	floating := ""
	if n.Floating != "" {
		floating = "." + n.Floating
	}

	exp := ""
	if n.Exp != "" {
		exp = "e" + n.Exp
	}

	return "Number<" + base + neg + n.Integral + floating + exp + ">"
}

func (n *Number) Desc() string {
	return "number literal"
}

func (l *Lexer) scanNumber(loc *Loc) (Token, error) {
	// Numbers have several optional parts. Bases are only allowed for integers,
	// any number with floating or exp parts must be decimal.
	//
	// base? minus? integral floating? exp?

	// They always start with digits, which is either the base or the integral
	// part.
	integral := string(l.scanner.readWhile(digit))
	if r, eof := l.scanner.lookahead(); !eof && r == 'r' {
		l.scanner.read() // Consume the r.
		base, _ := strconv.Atoi(string(integral))
		n := &Number{Base: base, loc: loc}

		// Possibly a minus sign now.
		if r, eof := l.scanner.lookahead(); !eof && r == '-' {
			l.scanner.read()
			n.Negative = true
		}

		// Checking that these characters are valid under the base is the parser's
		// problem.
		n.Integral = string(l.scanner.readWhile(identContinue))
		return n, nil
	}

	// Decimal number with possible float and exp parts.
	n := &Number{Base: 10, loc: loc, Integral: integral}

	r, eof := l.scanner.lookahead()
	if !eof && r == '.' {
		l.scanner.read() // Consume the .
		n.Floating = string(l.scanner.readWhile(digit))

		// Replace the "next character".
		r, eof = l.scanner.lookahead()
	}

	if !eof && r == 'e' {
		l.scanner.read() // Consume the e
		n.Exp = string(l.scanner.readWhile(digit))
	}

	return n, nil
}

func nonQuote(r rune) bool {
	return r != '\''
}

func (l *Lexer) scanString(loc *Loc) (Token, error) {
	// The opening ' has already been consumed.
	var runes []rune
	for {
		runes = append(runes, l.scanner.readWhile(nonQuote)...)
		l.scanner.read() // Consume the ending '

		// Now if the next character is another quote, this is an escaped single
		// quote and we need to keep going.
		if r, eof := l.scanner.lookahead(); eof || r != '\'' {
			// Not a quote, we're done.
			break
		}

		// If we're still here, it's an escaped quote.
		l.scanner.read() // Consume the opening quote too.
		runes = append(runes, '\'')
		// Now we're ready for the next loop, with the scanner on the first
		// post-quote character.
	}

	return &StringLit{string(runes), loc}, nil
}

type StringLit struct {
	Str string
	loc *Loc
}

func (s *StringLit) Id() TokenId {
	return TString
}

func (s *StringLit) SourceLoc() *Loc {
	return s.loc
}

func (s *StringLit) String() string {
	return "String<" + s.Str + ">"
}

func (s *StringLit) Desc() string {
	return "string literal"
}
