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
	call        // so it can stand in expr(...)
)

func (c *ctx) expressionList(w writer, n *cc.ExpressionList, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = void
	}
	for ; n != nil; n = n.ExpressionList {
		switch {
		case n.ExpressionList == nil:
			return c.expr(w, n.AssignmentExpression, t, mode)
		default:
			c.expr(w, n.AssignmentExpression, cc.Invalid, void)
		}
	}
	return nil
}

func (c *ctx) expr(w writer, n cc.ExpressionNode, t cc.Type, mode mode) (r []byte) {
	switch x := n.(type) {
	case *cc.AssignmentExpression:
		return c.assignmentExpression(w, x, t, mode)
	case *cc.PostfixExpression:
		return c.postfixExpression(w, x, t, mode)
	case *cc.PrimaryExpression:
		return c.primaryExpression(w, x, t, mode)
	default:
		c.err(errorf("%v: TODO %T %s", c.pos(n), x, cc.NodeSource(n)))
		return nil
	}
}

func (c *ctx) postfixExpression(w writer, n *cc.PostfixExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = void
	}
	var b buf
	switch mode {
	case void:
		switch n.Case {
		case cc.PostfixExpressionPrimary: // PrimaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionIndex: // PostfixExpression '[' ExpressionList ']'
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
			ft := n.PostfixExpression.Type().(*cc.PointerType).Elem().(*cc.FunctionType)
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

			params := ft.Parameters()
			var xargs [][]byte
			for i, v := range args {
				var t cc.Type
				switch {
				case i < len(params):
					t = params[i].Type()
				default:
					switch t = v.Type(); {
					case cc.IsIntegerType(t):
						t = cc.IntegerPromotion(t)
					case t.Kind() == cc.Float:
						c.err(errorf("TODO"))
					}
				}
				xargs = append(xargs, c.expr(w, v, t, value))
			}
			b.w("%s(", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type(), call))
			for _, v := range xargs {
				b.w("%s,", v)
			}
			b.w(")")
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
	return b.bytes()
}

func (c *ctx) primaryExpression(w writer, n *cc.PrimaryExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = void
	}
	var b buf
	switch mode {
	case value:
		switch n.Case {
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			switch x := n.ResolvedTo().(type) {
			case *cc.Declarator:
				switch x.Linkage() {
				case cc.None:
					b.w("%s%s", tag(none), x.Name())
				default:
					c.err(errorf("TODO %v", x.Linkage()))
				}
			default:
				c.err(errorf("TODO %T", x))
			}
		case cc.PrimaryExpressionInt: // INTCONST
			b.w("%s", c.primaryExpressionValue(w, n.Value(), n.Type(), t))
		case cc.PrimaryExpressionFloat: // FLOATCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionChar: // CHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionLChar: // LONGCHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionString: // STRINGLITERAL
			b.w("%s", c.primaryExpressionValue(w, n.Value(), n.Type(), t))
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
	case lvalue:
		switch n.Case {
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			switch x := n.ResolvedTo().(type) {
			case *cc.Declarator:
				switch x.Linkage() {
				case cc.None:
					b.w("%s%s", tag(none), x.Name())
				default:
					c.err(errorf("TODO %v", x.Linkage()))
				}
			default:
				c.err(errorf("TODO %T", x))
			}
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
	case call:
		switch n.Case {
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			switch x := n.ResolvedTo().(type) {
			case *cc.Declarator:
				switch x.Linkage() {
				case cc.External:
					b.w("%s%s", tag(external), x.Name())
				default:
					c.err(errorf("TODO %v", x.Linkage()))
				}
			case nil:
				b.w("%s%s", tag(external), n.Token.Src())
			default:
				c.err(errorf("TODO %T", x))
			}
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
	default:
		c.err(errorf("TODO %v", mode))
	}
	return b.bytes()
}

func (c *ctx) primaryExpressionValue(w writer, v cc.Value, from, to cc.Type) (r []byte) {
	var b buf

	if to.Kind() == cc.Enum {
		to = to.(*cc.EnumType).UnderlyingType()
	}
	switch {
	case cc.IsIntegerType(to):
		switch {
		case to.Size() > 8:
			c.err(errorf("TODO %v", to))
		case cc.IsSignedInteger(to):
			m := cc.Int64Value(-1)
			if to.Size() < 8 {
				m = cc.Int64Value(1)<<(8*to.Size()) - 1
			}
			switch x := v.(type) {
			case cc.Int64Value:
				switch {
				case x < 0:
					b.w("%s(%d)", c.typ(to), x|^m)
				default:
					b.w("%s(%d)", c.typ(to), x&m)
				}
			case cc.UInt64Value:
				switch y := cc.Int64Value(x); {
				case y < 0:
					b.w("%s(%d)", c.typ(to), y|^m)
				default:
					b.w("%s(%d)", c.typ(to), y&m)
				}
			}
		default:
			m := ^cc.UInt64Value(0)
			if to.Size() < 8 {
				m = cc.UInt64Value(1)<<(8*to.Size()) - 1
			}
			switch x := v.(type) {
			case cc.Int64Value:
				b.w("%s(%d)", c.typ(to), cc.UInt64Value(x)&m)
			case cc.UInt64Value:
				b.w("%s(%d)", c.typ(to), x&m)
			}
		}
	default:
		switch x := v.(type) {
		case cc.StringValue:
			if to.Decay().Kind() == cc.Ptr {
				b.w("%q", x)
			}
		default:
			c.err(errorf("TODO %T %v -> %v", v, from, to))
		}
	}
	return b.bytes()
}

func (c *ctx) assignmentExpression(w writer, n *cc.AssignmentExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = void
	}
	var b buf
	switch mode {
	case void:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			ae := c.expr(w, n.AssignmentExpression, n.UnaryExpression.Type(), value)
			b.w("%s = %s", c.expr(w, n.UnaryExpression, n.UnaryExpression.Type(), lvalue), ae)
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
	return b.bytes()
}
