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
		p.assignmentExpression(f, n.AssignmentExpression, t, false, true)
	case cc.InitializerInitList: // '{' InitializerList ',' '}'
		panic(todo("", n.Position()))
	default:
		panic(todo("internal error"))
	}
}
