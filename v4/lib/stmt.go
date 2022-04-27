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
		c.labeledStatement(w, n.LabeledStatement)
	case cc.StatementCompound: // CompoundStatement
		c.compoundStatement(w, n.CompoundStatement, false)
	case cc.StatementExpr: // ExpressionStatement
		c.expressionStatement(w, n.ExpressionStatement)
	case cc.StatementSelection: // SelectionStatement
		c.selectionStatement(w, n.SelectionStatement)
	case cc.StatementIteration: // IterationStatement
		c.iterationStatement(w, n.IterationStatement)
	case cc.StatementJump: // JumpStatement
		c.jumpStatement(w, n.JumpStatement)
	case cc.StatementAsm: // AsmStatement
		c.err(errorf("%v: assembler statements not supported", n.Position()))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	w.w(" // %v:", c.pos(n))
}

func (c *ctx) labeledStatement(w writer, n *cc.LabeledStatement) {
	switch n.Case {
	case cc.LabeledStatementLabel: // IDENTIFIER ':' Statement
		c.err(errorf("TODO %v", n.Case))
	case cc.LabeledStatementCaseLabel: // "case" ConstantExpression ':' Statement
		if n.CaseOrdinal() != 0 {
			w.w("\nfallthrough")
		}
		w.w("\ncase %s:", c.expr(nil, n.ConstantExpression, nil, exprDefault))
		c.statement(w, n.Statement)
	case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
		if n.CaseOrdinal() != 0 {
			w.w("\nfallthrough")
		}
		c.err(errorf("TODO %v", n.Case))
	case cc.LabeledStatementDefault: // "default" ':' Statement
		if n.CaseOrdinal() != 0 {
			w.w("\nfallthrough")
		}
		w.w("\ndefault:")
		c.statement(w, n.Statement)
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) compoundStatement(w writer, n *cc.CompoundStatement, fnBlock bool) {
	w.w(" { // %v:", c.pos(n))
	if fnBlock && c.f.tlsAllocs+int64(c.f.maxValist) != 0 {
		c.f.tlsAllocs = roundup(c.f.tlsAllocs, 8)
		v := c.f.tlsAllocs
		if c.f.maxValist != 0 {
			v += 8 * int64((c.f.maxValist + 1))
		}
		w.w("\n%sbp := %[1]stls.Alloc(%d) // tlsAllocs %v maxValist %v", tag(ccgo), v, c.f.tlsAllocs, c.f.maxValist)
		w.w("\ndefer %stls.Free(%d)", tag(ccgo), v)
		for _, v := range c.f.t.Parameters() {
			if d := v.Declarator; d != nil && c.f.declInfos.info(d).pinned() {
				w.w("\n*(*%s)(unsafe.Pointer(%sbp)) = %[2]s_%s", c.typ(d.Type()), tag(ccgo), d.Name())
			}
		}
	}
	var bi *cc.BlockItem
	for l := n.BlockItemList; l != nil; l = l.BlockItemList {
		bi = l.BlockItem
		c.blockItem(w, bi)
	}
	if fnBlock && c.f.t.Result().Kind() != cc.Void && !c.isReturn(bi) {
		w.w("\nreturn %sr", tag(ccgo))
	}
	w.w("\n}")
}

func (c *ctx) isReturn(n *cc.BlockItem) bool {
	if n == nil || n.Case != cc.BlockItemStmt {
		return false
	}

	return n.Statement.Case == cc.StatementJump && n.Statement.JumpStatement.Case == cc.JumpStatementReturn
}

func (c *ctx) blockItem(w writer, n *cc.BlockItem) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		c.declaration(w, n.Declaration, false)
	case cc.BlockItemLabel: // LabelDeclaration
		c.err(errorf("TODO %v", n.Case))
	case cc.BlockItemStmt: // Statement
		c.statement(w, n.Statement)
	case cc.BlockItemFuncDef: // DeclarationSpecifiers Declarator CompoundStatement
		if c.pass == 2 {
			c.functionDefinition0(w, n.Declarator, n.CompoundStatement, true) //TODO does not really work yet
		}
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) selectionStatement(w writer, n *cc.SelectionStatement) {
	switch n.Case {
	case cc.SelectionStatementIf: // "if" '(' ExpressionList ')' Statement
		w.w("\nif %s", c.expr(w, n.ExpressionList, nil, exprBool))
		c.bracedStatement(w, n.Statement)
	case cc.SelectionStatementIfElse: // "if" '(' ExpressionList ')' Statement "else" Statement
		w.w("\nif %s {", c.expr(w, n.ExpressionList, nil, exprBool))
		c.statement(w, n.Statement)
		w.w("\n} else {")
		c.statement(w, n.Statement2)
		w.w("\n}")
	case cc.SelectionStatementSwitch: // "switch" '(' ExpressionList ')' Statement
		w.w("\nswitch %s", c.expr(w, n.ExpressionList, nil, exprDefault))
		c.statement(w, n.Statement)
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) bracedStatement(w writer, n *cc.Statement) {
	switch n.Case {
	case cc.StatementCompound:
		c.statement(w, n)
	default:
		w.w("{")
		c.statement(w, n)
		w.w("\n}")
	}
}

func (c *ctx) unbracedStatement(w writer, n *cc.Statement) {
	switch n.Case {
	case cc.StatementCompound:
		for l := n.CompoundStatement.BlockItemList; l != nil; l = l.BlockItemList {
			c.blockItem(w, l.BlockItem)
		}
	default:
		c.statement(w, n)
	}
}

func (c *ctx) iterationStatement(w writer, n *cc.IterationStatement) {
	switch n.Case {
	case cc.IterationStatementWhile: // "while" '(' ExpressionList ')' Statement
		var a buf
		switch b := c.expr(&a, n.ExpressionList, nil, exprBool); {
		case a.len() != 0:
			w.w("\nfor {")
			w.w("%s", a.bytes())
			w.w("\nif !(%s) { break }", b)
			c.unbracedStatement(w, n.Statement)
			w.w("\n}")
		default:
			w.w("\nfor %s", b)
			c.bracedStatement(w, n.Statement)
		}
	case cc.IterationStatementDo: // "do" Statement "while" '(' ExpressionList ')' ';'
		var a buf
		switch b := c.expr(&a, n.ExpressionList, nil, exprBool); {
		case a.len() != 0:
			w.w("\nfor {")
			c.unbracedStatement(w, n.Statement)
			w.w("%s", a.bytes())
			w.w("\nif !(%s) { break }", b)
			w.w("\n}")
		default:
			w.w("\nfor %scond := true; %[1]scond; %[1]scond = %s", tag(ccgo), b)
			c.bracedStatement(w, n.Statement)
		}
	case cc.IterationStatementFor: // "for" '(' ExpressionList ';' ExpressionList ';' ExpressionList ')' Statement
		var a, a2, a3 buf
		var b2 []byte
		if n.ExpressionList2 != nil {
			b2 = c.expr(&a2, n.ExpressionList2, nil, exprBool).bytes()
		}
		switch b, b3 := c.expr(&a, n.ExpressionList, nil, exprVoid), c.expr(&a3, n.ExpressionList3, nil, exprVoid); {
		case a.len() == 0 && a2.len() == 0 && a3.len() == 0:
			w.w("\nfor %s; %s; %s", b, b2, b3)
			c.bracedStatement(w, n.Statement)
		default:
			c.err(errorf("TODO"))
		}
	case cc.IterationStatementForDecl: // "for" '(' Declaration ExpressionList ';' ExpressionList ')' Statement
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
		w.w("\ncontinue")
	case cc.JumpStatementBreak: // "break" ';'
		w.w("\nbreak")
	case cc.JumpStatementReturn: // "return" ExpressionList ';'
		switch {
		case n.ExpressionList != nil:
			w.w("\nreturn %s", c.expr(w, n.ExpressionList, c.f.t.Result(), exprDefault))
		default:
			w.w("\nreturn")
		}
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) expressionStatement(w writer, n *cc.ExpressionStatement) {
	w.w("\n%s", c.expr(w, n.ExpressionList, nil, exprVoid))
}
