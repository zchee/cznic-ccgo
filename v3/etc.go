// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"fmt"
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
