// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"modernc.org/cc/v4"
)

func (c *ctx) initializer(w writer, n *cc.Initializer, t cc.Type) (r []byte) {
	switch n.Case {
	case cc.InitializerExpr: // AssignmentExpression
		return c.expr(w, n.AssignmentExpression, t, value)
	case cc.InitializerInitList: // '{' InitializerList ',' '}'
		c.err(errorf("TODO %T %v", n, n.Case))
	}
	c.err(errorf("internal error %T %v", n, n.Case))
	return nil
}
