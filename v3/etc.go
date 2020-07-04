// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"fmt"
	"math"
	"math/big"
	"strings"

	"modernc.org/cc/v3"
)

var (
	reservedNames = map[string]bool{
		"bool":        false, // ccgo can use
		"break":       true,  // keyword
		"case":        true,  // keyword
		"chan":        true,  // keyword
		"const":       true,  // keyword
		"continue":    true,  // keyword
		"default":     true,  // keyword
		"defer":       true,  // keyword
		"else":        true,  // keyword
		"fallthrough": true,  // keyword
		"false":       false, // ccgo can use
		"float32":     false, // ccgo can use
		"float64":     false, // ccgo can use
		"for":         true,  // keyword
		"func":        true,  // keyword
		"go":          true,  // keyword
		"goto":        true,  // keyword
		"if":          true,  // keyword
		"import":      true,  // keyword
		"init":        false, // special name
		"int16":       false, // ccgo can use
		"int32":       false, // ccgo can use
		"int64":       false, // ccgo can use
		"int8":        false, // ccgo can use
		"interface":   true,  // keyword
		"map":         true,  // keyword
		"math":        false, // package name
		"nil":         false, // ccgo can use
		"package":     true,  // keyword
		"range":       true,  // keyword
		"return":      true,  // keyword
		"select":      true,  // keyword
		"struct":      true,  // keyword
		"switch":      true,  // keyword
		"true":        false, // ccgo can use
		"type":        true,  // keyword
		"uint16":      false, // ccgo can use
		"uint32":      false, // ccgo can use
		"uint64":      false, // ccgo can use
		"uint8":       false, // ccgo can use
		"uintptr":     false, // ccgo can use
		"unsafe":      false, // package name
		"var":         true,  // keyword
	}

	maxInt32  = big.NewInt(math.MaxInt32)
	maxInt64  = big.NewInt(math.MaxInt64)
	maxUint32 = big.NewInt(math.MaxUint32)
	maxUint64 = big.NewInt(0).SetUint64(math.MaxUint64)
	minInt32  = big.NewInt(math.MinInt32)
	minInt64  = big.NewInt(math.MinInt64)
)

type scope map[string]int

func newScope() scope {
	s := scope{}
	for k := range reservedNames {
		s.taken(k)
	}
	return s
}

func (s scope) taken(t string) {
	if t == "" {
		panic(todo("internal error"))
	}

	if _, ok := s[t]; ok {
		panic(todo("internal error"))
	}

	s[t] = 0
}

func (s scope) take(t string) string {
	if t == "" {
		panic(todo("internal error"))
	}

	n, ok := s[t]
	if !ok {
		s[t] = 0
		return t
	}

	for {
		n++
		s[t] = n
		r := fmt.Sprintf("%s%d", t, n)
		if _, ok := s[r]; !ok {
			s[r] = 0
			return r
		}
	}
}

func dumpLayout(t cc.Type) string {
	switch t.Kind() {
	case cc.Struct, cc.Union:
		// ok
	default:
		return t.String()
	}

	nf := t.NumField()
	var a []string
	w := 0
	for i := 0; i < nf; i++ {
		if n := len(t.FieldByIndex([]int{i}).Name().String()); n > w {
			w = n
		}
	}
	for i := 0; i < nf; i++ {
		f := t.FieldByIndex([]int{i})
		var bf cc.StringID
		if f.IsBitField() {
			if bfbf := f.BitFieldBlockFirst(); bfbf != nil {
				bf = bfbf.Name()
			}
		}
		a = append(a, fmt.Sprintf("%3d: %*q: BitFieldOffset %3v, BitFieldWidth %3v, IsBitField %5v, Mask: %#016x, off: %3v, pad %2v, BitFieldBlockWidth: %2d, BitFieldBlockFirst: %s, %v",
			i, w+2, f.Name(), f.BitFieldOffset(), f.BitFieldWidth(),
			f.IsBitField(), f.Mask(), f.Offset(), f.Padding(),
			f.BitFieldBlockWidth(), bf, f.Type(),
		))
	}
	return t.String() + "\n" + strings.Join(a, "\n")
}

func typeSignature(n cc.Node, t cc.Type) (r string) {
	var b strings.Builder
	typeSignature2(n, &b, t)
	return b.String()
}

func typeSignature2(n cc.Node, b *strings.Builder, t cc.Type) {
	t = t.Alias()
	if t.IsIntegerType() {
		if !t.IsSignedType() {
			b.WriteByte('u')
		}
		fmt.Fprintf(b, "int%d", t.Size()*8)
		return
	}

	if t.IsArithmeticType() {
		b.WriteString(t.Kind().String())
		return
	}

	structOrUnion := "struct"
	switch t.Kind() {
	case cc.Ptr:
		fmt.Sprintf("*%s", t.Elem())
	case cc.Array:
		fmt.Sprintf("[%d]%s", t.Len(), t.Elem())
	case cc.Union:
		structOrUnion = "union"
		fallthrough
	case cc.Struct:
		b.WriteString(structOrUnion)
		nf := t.NumField()
		fmt.Fprintf(b, " %d{", nf)
		b.WriteByte('{')
		for idx := []int{0}; idx[0] < nf; idx[0]++ {
			f := t.FieldByIndex(idx)
			fmt.Fprintf(b, "%s:%d:%d:%v:%d:%d:",
				f.Name(), f.BitFieldOffset(), f.BitFieldWidth(), f.IsBitField(), f.Offset(), f.Padding(),
			)
			typeSignature2(n, b, f.Type())
			b.WriteByte(';')
		}
		b.WriteByte('}')
	case cc.Void:
		b.WriteString("void")
	case cc.Invalid:
		b.WriteString("invalid") //TODO fix cc/v3
	default:
		panic(todo("", pos(n), t, t.Kind()))
	}
}
