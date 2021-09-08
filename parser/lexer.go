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
}

type Lexer struct {
	buffer  Token
	loc     *Loc
	scanner *scanner
}

func (l *Lexer) Advance() Token {
	if l.buffer != nil {
		ret := l.buffer
		l.buffer = nil
		return ret
	}

	t, err := l.scan()
	if err != nil {
		return &eof{}
	}
	return t
}

func (l *Lexer) Rewind(token Token) {
	if l.buffer != nil {
		panic("can't rewind more than 1 step")
	}
	l.buffer = token
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

	TColon
	TCaret
	TBang
	THash

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
	} else if r == '\'' {
		return l.scanString(&loc)
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
	':': TColon,
	'^': TCaret,
	'!': TBang,
	'#': THash,
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

func (l *Lexer) scanCharLiteral(loc *Loc) (Token, error) {
	r, err := l.read()
	if err != nil {
		return nil, err
	}
	return &charLit{r, loc}, nil
}

type charLit struct {
	ch  rune
	loc *Loc
}

func (c *charLit) Id() TokenId {
	return TChar
}

func (c *charLit) SourceLoc() *Loc {
	return c.loc
}

func (c *charLit) String() string {
	return "$" + string(c.ch)
}

type eof struct {
	loc *Loc
}

func (e *eof) Id() TokenId {
	return TEof
}
func (e *eof) SourceLoc() *Loc {
	return e.loc
}
func (e *eof) String() string {
	return "<EOF>"
}

// Binary operations
func (l *Lexer) scanBinary(loc *Loc, r rune) (Token, error) {
	// Binary operators are 1 or 2 characters long, so check the second one, and
	// if it's also a binchar they get combined.
	next, err := l.peek()
	s := string(r)
	if err != nil {
		return &binOp{s, loc}, nil
	}

	if _, ok := binchars[next]; ok {
		s += string(next)
	}

	return &binOp{s, loc}, nil
}

type binOp struct {
	op  string
	loc *Loc
}

func (b *binOp) Id() TokenId {
	return TBinary
}

func (b *binOp) SourceLoc() *Loc {
	return b.loc
}

func (b *binOp) String() string {
	return b.op
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
	return &ident{string(runes), loc}, nil
}

type ident struct {
	text string
	loc  *Loc
}

func (i *ident) Id() TokenId {
	return TIdent
}

func (i *ident) SourceLoc() *Loc {
	return i.loc
}

func (i *ident) String() string {
	return "Ident<" + i.text + ">"
}

// TODO These don't really support Unicode identifiers!
func identStart(r rune) bool {
	return ('a' <= r && r <= 'z') || ('A' <= r && r <= 'Z') || r == '_'
}

func identContinue(r rune) bool {
	return identStart(r) || digit(r)
}

func digit(r rune) bool {
	return '0' <= r && r <= '9'
}

type number struct {
	base     int
	negative bool
	integral string
	floating string
	exp      string
	loc      *Loc
}

func (n *number) Id() TokenId {
	return TNumber
}

func (n *number) SourceLoc() *Loc {
	return n.loc
}

func (n *number) String() string {
	base := ""
	if n.base != 10 {
		base = fmt.Sprintf("%dr", n.base)
	}

	neg := ""
	if n.negative {
		neg = "-"
	}

	floating := ""
	if n.floating != "" {
		floating = "." + n.floating
	}

	exp := ""
	if n.exp != "" {
		exp = "e" + n.exp
	}

	return "Number<" + base + neg + n.integral + floating + exp + ">"
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
		n := &number{base: base, loc: loc}

		// Possibly a minus sign now.
		if r, eof := l.scanner.lookahead(); !eof && r == '-' {
			l.scanner.read()
			n.negative = true
		}

		// Checking that these characters are valid under the base is the parser's
		// problem.
		n.integral = string(l.scanner.readWhile(identContinue))
		return n, nil
	}

	// Decimal number with possible float and exp parts.
	n := &number{base: 10, loc: loc, integral: integral}

	r, eof := l.scanner.lookahead()
	if !eof && r == '.' {
		l.scanner.read() // Consume the .
		n.floating = string(l.scanner.readWhile(digit))

		// Replace the "next character".
		r, eof = l.scanner.lookahead()
	}

	if !eof && r == 'e' {
		l.scanner.read() // Consume the e
		n.exp = string(l.scanner.readWhile(digit))
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

	return &stringLit{string(runes), loc}, nil
}

type stringLit struct {
	str string
	loc *Loc
}

func (s *stringLit) Id() TokenId {
	return TString
}

func (s *stringLit) SourceLoc() *Loc {
	return s.loc
}

func (s *stringLit) String() string {
	return "String<" + s.str + ">"
}
