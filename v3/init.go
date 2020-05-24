// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"modernc.org/cc/v3"
)

func (p *project) initializer(f *function, n *cc.Initializer, t cc.Type) {
	switch n.Case {
	case cc.InitializerExpr: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, exprValue, true)
	case cc.InitializerInitList: // '{' InitializerList ',' '}'
		defer p.w("%s", p.convert(n.Type(), t))
		p.w("%s{", p.typ(n.Type()))
		for list := n.InitializerList; list != nil; list = list.InitializerList {
			if list.Designation != nil {
				panic(todo(""))
			}
			p.initializer(f, list.Initializer, list.Initializer.Type())
			p.w(", ")
		}
		p.w("}")
	default:
		panic(todo("internal error"))
	}
}
