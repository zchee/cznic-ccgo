// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"fmt"
)

var (
	reservedNames = map[string]bool{
		"bool":        true, // ccgo can use
		"break":       true, // keyword
		"case":        true, // keyword
		"chan":        true, // keyword
		"const":       true, // keyword
		"continue":    true, // keyword
		"default":     true, // keyword
		"defer":       true, // keyword
		"else":        true, // keyword
		"fallthrough": true, // keyword
		"false":       true, // ccgo can use
		"float32":     true, // ccgo can use
		"float64":     true, // ccgo can use
		"for":         true, // keyword
		"func":        true, // keyword
		"go":          true, // keyword
		"goto":        true, // keyword
		"if":          true, // keyword
		"import":      true, // keyword
		"int16":       true, // ccgo can use
		"int32":       true, // ccgo can use
		"int64":       true, // ccgo can use
		"int8":        true, // ccgo can use
		"interface":   true, // keyword
		"map":         true, // keyword
		"math":        true, // package name
		"nil":         true, // ccgo can use
		"package":     true, // keyword
		"range":       true, // keyword
		"return":      true, // keyword
		"select":      true, // keyword
		"struct":      true, // keyword
		"switch":      true, // keyword
		"true":        true, // ccgo can use
		"type":        true, // keyword
		"uint16":      true, // ccgo can use
		"uint32":      true, // ccgo can use
		"uint64":      true, // ccgo can use
		"uint8":       true, // ccgo can use
		"uintptr":     true, // ccgo can use
		"unsafe":      true, // package name
		"var":         true, // keyword
	}
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
	if _, ok := s[t]; ok {
		panic(todo("internal error"))
	}

	s[t] = 0
}

func (s scope) take(t string) string {
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
