package parser

import (
	"fmt"
	"io"
)

type scanner struct {
	buffered    []rune
	bufferCount int
	eof         bool
	reader      io.RuneReader
}

var eofError = fmt.Errorf("EOF")

func (s *scanner) lookahead() (rune, bool) {
	s.buffer()

	if s.eof {
		return 0, true
	}

	return s.buffered[s.bufferCount-1], false
}

func (s *scanner) read() (rune, bool) {
	s.buffer()
	if s.eof {
		return 0, true
	}

	s.bufferCount--
	r := s.buffered[s.bufferCount]
	return r, false
}

func (s *scanner) rewind(r rune) {
	if s.bufferCount > 1 {
		panic("can't buffer more than two characters")
	}

	s.buffered[s.bufferCount] = r
	s.bufferCount++
}

// Reads 0 or more runes that pass the predicate and returns them.
// The last value
func (s *scanner) readWhile(pred func(rune) bool) []rune {
	runes := []rune{}
	for {
		r, eof := s.read()
		if eof {
			return runes
		}
		if pred(r) {
			runes = append(runes, r)
		} else {
			s.rewind(r)
			return runes
		}
	}
}

// Ensures there's a rune in the buffer, reading one if there's nothing
// buffered now.
func (s *scanner) buffer() {
	if s.eof || s.bufferCount > 0 {
		return
	}

	r, _, err := s.reader.ReadRune()
	if err != nil {
		s.eof = true
		return
	}

	s.buffered[s.bufferCount] = r
	s.bufferCount++
}
