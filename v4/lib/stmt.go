// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"modernc.org/cc/v4"
)

func (c *ctx) statement(w writer, n *cc.Statement) {
	switch n.Case {
	case cc.StatementLabeled: // LabeledStatement
		c.err(errorf("TODO %v", n.Case))
	case cc.StatementCompound: // CompoundStatement
		c.err(errorf("TODO %v", n.Case))
	case cc.StatementExpr: // ExpressionStatement
		c.expressionStatement(w, n.ExpressionStatement)
	case cc.StatementSelection: // SelectionStatement
		c.err(errorf("TODO %v", n.Case))
	case cc.StatementIteration: // IterationStatement
		c.err(errorf("TODO %v", n.Case))
	case cc.StatementJump: // JumpStatement
		c.jumpStatement(w, n.JumpStatement)
	case cc.StatementAsm: // AsmStatement
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) jumpStatement(w writer, n *cc.JumpStatement) {
	switch n.Case {
	case cc.JumpStatementGoto: // "goto" IDENTIFIER ';'
		c.err(errorf("TODO %v", n.Case))
	case cc.JumpStatementGotoExpr: // "goto" '*' ExpressionList ';'
		c.err(errorf("TODO %v", n.Case))
	case cc.JumpStatementContinue: // "continue" ';'
		c.err(errorf("TODO %v", n.Case))
	case cc.JumpStatementBreak: // "break" ';'
		c.err(errorf("TODO %v", n.Case))
	case cc.JumpStatementReturn: // "return" ExpressionList ';'
		w.w("\nreturn %s", c.convertExpressionList(w, n.ExpressionList, c.ft.Result()))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	w.w(" // %v:", c.pos(n))
}

func (c *ctx) expressionStatement(w writer, n *cc.ExpressionStatement) {
	w.w("\n%s // %v:", c.expressionList(w, n.ExpressionList, void), c.pos(n))
}
