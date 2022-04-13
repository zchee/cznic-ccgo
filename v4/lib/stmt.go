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
		c.iterationStatement(w, n.IterationStatement)
	case cc.StatementJump: // JumpStatement
		c.jumpStatement(w, n.JumpStatement)
	case cc.StatementAsm: // AsmStatement
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) iterationStatement(w writer, n *cc.IterationStatement) {
	switch n.Case {
	case cc.IterationStatementWhile: // "while" '(' ExpressionList ')' Statement
		c.err(errorf("TODO %v", n.Case))
	case cc.IterationStatementDo: // "do" Statement "while" '(' ExpressionList ')' ';'
		c.err(errorf("TODO %v", n.Case))
	case cc.IterationStatementFor: // "for" '(' ExpressionList ';' ExpressionList ';' ExpressionList ')' Statement
		var a1, a2, a3 buf
		var b1, b2, b3 []byte
		if n.ExpressionList != nil {
			b1 = c.expr(&a1, n.ExpressionList, c.void, void)
		}
		switch {
		case a1.len() == 0:
			if n.ExpressionList2 != nil {
				b2 = c.expr(&a2, n.ExpressionList2, n.ExpressionList2.Type(), boolean)
			}
			switch {
			case a2.len() == 0:
				if n.ExpressionList3 != nil {
					b3 = c.expr(&a3, n.ExpressionList3, c.void, void)
				}
				switch {
				case a3.len() == 0:
					w.w("\nfor %s; %s; %s {", b1, b2, b3)
					c.statement(w, n.Statement)
					w.w("\n}")
				default:
					c.err(errorf("TODO %v", n.Case))
				}
			default:
				c.err(errorf("TODO %v", n.Case))
			}
		default:
			c.err(errorf("TODO %v", n.Case))
		}
	case cc.IterationStatementForDecl: // "for" '(' Declaration ExpressionList ';' ExpressionList ')' Statement
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	w.w(" // %v:", c.pos(n))
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
		w.w("\nreturn %s", c.expressionList(w, n.ExpressionList, c.f.t.Result(), value))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	w.w(" // %v:", c.pos(n))
}

func (c *ctx) expressionStatement(w writer, n *cc.ExpressionStatement) {
	w.w("\n%s // %v:", c.expressionList(w, n.ExpressionList, c.void, void), c.pos(n))
}
