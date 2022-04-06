// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"modernc.org/cc/v4"
)

type mode int

const (
	void   mode = iota
	lvalue      // soit can stand on LHS of lhs = rhs
	value       // so it can stand on RHS of lhs = rhs
)

func (c *ctx) expressionList(w writer, n *cc.ExpressionList, mode mode) []byte {
	var b buf
	for ; n != nil; n = n.ExpressionList {
		switch {
		case n.ExpressionList == nil:
			b.w("%s", c.expr(w, n.AssignmentExpression, mode))
		default:
			c.expr(w, n.AssignmentExpression, void)
		}
	}
	return b.bytes()
}

func (c *ctx) expr(w writer, n cc.ExpressionNode, mode mode) []byte {
	switch x := n.(type) {
	case *cc.AssignmentExpression:
		return c.assignmentExpression(w, x, mode)
	case *cc.PrimaryExpression:
		return c.primaryExpression(w, x, mode)
	case *cc.PostfixExpression:
		return c.postfixExpression(w, x, mode)
	default:
		c.err(errorf("%v: TODO %T %s", c.pos(n), x, cc.NodeSource(n)))
		return nil
	}
}

func (c *ctx) postfixExpression(w writer, n *cc.PostfixExpression, mode mode) (r []byte) {
	switch mode {
	case void:
		switch n.Case {
		case cc.PostfixExpressionPrimary: // PrimaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionIndex: // PostfixExpression '[' ExpressionList ']'
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
			p, ok := n.PostfixExpression.Type().(*cc.PointerType)
			if !ok {
				c.err(errorf("%v: internal error: %T", c.pos(n.PostfixExpression), n.PostfixExpression.Type()))
				return nil
			}

			ft, ok := p.Elem().(*cc.FunctionType)
			if !ok {
				c.err(errorf("%v: internal error: %T", c.pos(n.PostfixExpression), n.PostfixExpression.Type()))
				return nil
			}

			var args []cc.ExpressionNode
			for l := n.ArgumentExpressionList; l != nil; l = l.ArgumentExpressionList {
				args = append(args, l.AssignmentExpression)
			}
			if len(args) < ft.MinArgs() {
				c.err(errorf("%v: too few arguments to function '%s'", c.pos(n.PostfixExpression), cc.NodeSource(n.PostfixExpression)))
				break
			}

			if len(args) > ft.MaxArgs() && ft.MaxArgs() >= 0 {
				c.err(errorf("%v: too many arguments to function '%s'", c.pos(n.PostfixExpression), cc.NodeSource(n.PostfixExpression)))
				break
			}

			var xargs [][]byte
			for _, v := range args {
				xargs = append(xargs, c.expr(w, v, value))
			}
			var b buf
			b.w("%s(", c.expr(w, n.PostfixExpression, value))
			for _, v := range xargs {
				b.w("%s,", v)
			}
			b.w(")")
			return b.bytes()
		case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionInc: // PostfixExpression "++"
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionDec: // PostfixExpression "--"
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return r
}

func (c *ctx) primaryExpression(w writer, n *cc.PrimaryExpression, mode mode) (r []byte) {
	var b buf
	switch mode {
	case lvalue:
		switch n.Case {
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			return c.primaryExpressionIdent(w, n, mode)
		case cc.PrimaryExpressionInt: // INTCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionFloat: // FLOATCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionChar: // CHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionLChar: // LONGCHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionString: // STRINGLITERAL
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionExpr: // '(' ExpressionList ')'
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionGeneric: // GenericSelection
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	case value:
		switch n.Case {
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			return c.primaryExpressionIdent(w, n, mode)
		case cc.PrimaryExpressionInt: // INTCONST
			c.value(&b, n.Value(), n.Type())
			return b.bytes()
		case cc.PrimaryExpressionFloat: // FLOATCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionChar: // CHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionLChar: // LONGCHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionString: // STRINGLITERAL
			c.value(&b, n.Value(), n.Type())
			return b.bytes()
		case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionExpr: // '(' ExpressionList ')'
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionGeneric: // GenericSelection
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return r
}

func (c *ctx) primaryExpressionIdent(w writer, n *cc.PrimaryExpression, mode mode) (r []byte) {
	var b buf
	switch mode {
	case lvalue:
		switch x := n.ResolvedTo().(type) {
		case *cc.Declarator:
			switch x.Linkage() {
			case cc.External:
				c.err(errorf("TODO"))
			case cc.Internal:
				c.err(errorf("TODO"))
			case cc.None:
				b.w("%s%s", tag(none), n.Token.Src())
				return b.bytes()
			default:
				c.err(errorf("%v: internal error: %v", c.pos(x), x.Linkage()))
			}
		default:
			c.err(errorf("TODO %T", x))
		}
	case value:
		switch x := n.ResolvedTo().(type) {
		case *cc.Declarator:
			switch x.Linkage() {
			case cc.External:
				b.w("%s%s", tag(external), n.Token.Src())
				return b.bytes()
			case cc.Internal:
				c.err(errorf("TODO"))
			case cc.None:
				b.w("%s%s", tag(none), n.Token.Src())
				return b.bytes()
			default:
				c.err(errorf("%v: internal error: %v", c.pos(x), x.Linkage()))
			}
		default:
			c.err(errorf("TODO %T", x))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return r
}

func (c *ctx) assignmentExpression(w writer, n *cc.AssignmentExpression, mode mode) (r []byte) {
	switch mode {
	case void:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			r = c.expr(w, n.UnaryExpression, lvalue)
			r = append(r, '=')
			return append(r, c.expr(w, n.AssignmentExpression, value)...)
		case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return r
}

func (c *ctx) convert(w writer, n cc.ExpressionNode, t cc.Type) (r []byte) {
	switch x := n.Type().(type) {
	case *cc.PredefinedType:
		switch y := t.(type) {
		case *cc.PredefinedType:
			switch {
			case x.Kind() == y.Kind():
				switch {
				case cc.IsIntegerType(x) && cc.IsIntegerType(y):
					return c.expr(w, n, value)
				}
			}
		}
	}
	c.err(errorf("TODO %T(%[1]v) -> %T(%[2]v)", n.Type(), t))
	return nil
}

func (c *ctx) convertExpressionList(w writer, n *cc.ExpressionList, t cc.Type) []byte {
	var b buf
	for ; n != nil; n = n.ExpressionList {
		switch {
		case n.ExpressionList == nil:
			return c.convert(w, n.AssignmentExpression, t)
		default:
			c.expr(w, n.AssignmentExpression, void)
		}
	}
	return b.bytes()
}
