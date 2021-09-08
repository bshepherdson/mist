package parser

import "fmt"

type Loc struct {
	file      string
	line, col int
}

func (loc *Loc) String() string {
	return fmt.Sprintf("%s %d:%d", loc.file, loc.line, loc.col)
}

func (loc *Loc) bump(ch rune) {
	if ch == '\n' {
		loc.line++
		loc.col = 0
	} else {
		loc.col++
	}
}
