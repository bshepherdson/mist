package main

type STLiteral interface {
	Encode() []uint16
	Equals(other STLiteral) bool
}

const (
	TypeInt = uint16(iota + 1)
	TypeChar
	TypeString
	TypeSymbol
	TypeArray
	TypeTrue
	TypeFalse
	TypeNil
	TypeWordArray
)

type intLit int

func (i intLit) Encode() []uint16 {
	return []uint16{TypeInt, uint16(i >> 16), uint16(i)}
}

func (i intLit) Equals(other STLiteral) bool {
	o, ok := other.(intLit)
	return ok && o == i
}

type charLit rune

func (c charLit) Encode() []uint16 {
	return []uint16{TypeChar, uint16(c)}
}

func (i charLit) Equals(other STLiteral) bool {
	o, ok := other.(charLit)
	return ok && o == i
}

type stringLit string

func (s stringLit) Encode() []uint16 {
	return stringLike(TypeString, string(s))
}

func (i stringLit) Equals(other STLiteral) bool {
	o, ok := other.(stringLit)
	return ok && o == i
}

type symbolLit string

func (s symbolLit) Encode() []uint16 {
	return stringLike(TypeSymbol, string(s))
}

func (i symbolLit) Equals(other STLiteral) bool {
	o, ok := other.(symbolLit)
	return ok && o == i
}

func stringLike(t uint16, s string) []uint16 {
	ret := []uint16{t, uint16(len(s))}
	for _, c := range s {
		ret = append(ret, uint16(c))
	}
	return ret
}

type litArray []STLiteral

func (arr litArray) Encode() []uint16 {
	ret := []uint16{TypeArray, uint16(len(arr))}
	for _, element := range arr {
		ret = append(ret, element.Encode()...)
	}
	return ret
}

func (arr litArray) Equals(other STLiteral) bool {
	o, ok := other.(litArray)
	if !ok {
		return false
	}

	for i, element := range arr {
		if !element.Equals(o[i]) {
			return false
		}
	}
	return true
}

type litSingleton string

func (s litSingleton) Encode() []uint16 {
	if s == "true" {
		return []uint16{TypeTrue}
	}
	if s == "false" {
		return []uint16{TypeFalse}
	}
	if s == "nil" {
		return []uint16{TypeNil}
	}
	panic("unknown literal singleton: " + s)
}

func (i litSingleton) Equals(other STLiteral) bool {
	o, ok := other.(litSingleton)
	return ok && o == i
}

type rawArray []uint16

func (arr rawArray) Encode() []uint16 {
	ret := []uint16{TypeWordArray, uint16(len(arr))}
	return append(ret, arr...)
}

func (arr rawArray) Equals(other STLiteral) bool {
	o, ok := other.(rawArray)
	if !ok {
		return false
	}
	for i, w := range arr {
		if w != o[i] {
			return false
		}
	}
	return true
}
