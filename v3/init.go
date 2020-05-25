// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"modernc.org/cc/v3"
)

func (p *project) initializer(f *function, n *cc.Initializer, t cc.Type, flags flags) {
	switch n.Case {
	case cc.InitializerExpr: // AssignmentExpression
		et := n.AssignmentExpression.Operand.Type()
		switch {
		case isArray(t) && isArray(et):
			p.initializerExprArray(f, n, t, flags)
		default:
			p.assignmentExpression(f, n.AssignmentExpression, t, exprValue, flags|fOutermost)
		}
	case cc.InitializerInitList: // '{' InitializerList ',' '}'
		defer p.w("%s", p.convert(n.Type(), t, flags))
		p.w("%s{", p.typ(n.Type()))
		for list := n.InitializerList; list != nil; list = list.InitializerList {
			if list.Designation != nil {
				panic(todo(""))
			}
			p.initializer(f, list.Initializer, list.Initializer.Type(), flags|fOutermost)
			p.w(", ")
		}
		p.w("}")
	default:
		panic(todo("internal error"))
	}
}

func (p *project) initializerExprArray(f *function, n *cc.Initializer, t cc.Type, flags flags) {
	switch t.Elem().Kind() {
	case cc.Char, cc.UChar, cc.SChar:
		switch x := n.AssignmentExpression.Operand.Value().(type) {
		case cc.StringValue:
			s := cc.StringID(x).String()
			n := len(s) + 1
			if t.Len() != uintptr(n) {
				panic(todo(""))
			}

			p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(t), p.stringLiteral(x))
			return
		default:
			panic(todo("%T", x))
		}
	case cc.Int:
		switch x := n.AssignmentExpression.Operand.Value().(type) {
		case cc.WideStringValue:
			s := []rune(cc.StringID(x).String())
			n := len(s) + 1
			if t.Len() != uintptr(n) {
				panic(todo(""))
			}

			p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(t), p.wideStringLiteral(x))
			return
		default:
			panic(todo("%T", x))
		}
		panic(todo(""))
	default:
		panic(todo("", t.Elem().Kind()))
	}
}
