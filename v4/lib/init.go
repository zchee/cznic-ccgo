// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"modernc.org/cc/v4"
)

func (c *ctx) initializer(w writer, n *cc.Initializer, t cc.Type) {
	switch n.Case {
	case cc.InitializerExpr: // AssignmentExpression
		switch v := n.AssignmentExpression.Value(); {
		case v != cc.Unknown:
			c.value(w, v, t)
		default:
			c.err(errorf("TODO %T %v", n, n.Case))
		}
	case cc.InitializerInitList: // '{' InitializerList ',' '}'
		c.err(errorf("TODO %T %v", n, n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}
