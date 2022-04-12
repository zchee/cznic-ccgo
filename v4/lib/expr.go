// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"strconv"

	"modernc.org/cc/v4"
	"modernc.org/mathutil"
)

type mode int

const (
	void   mode = iota
	lvalue      // so it can stand on LHS of lhs = rhs
	value       // so it can stand on RHS of lhs = rhs
	call        // so it can stand in expr(...)
)

func (c *ctx) convert(n cc.Node, b []byte, from, to cc.Type) (r []byte) {
	if from == to || to.Kind() == cc.Void {
		return b
	}

	c.err(errorf("%v: TODO %q %s -> %s", c.pos(n), b, from, to))
	return b //TODO
}

func (c *ctx) expressionList(w writer, n *cc.ExpressionList, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = void
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	for ; n != nil; n = n.ExpressionList {
		switch {
		case n.ExpressionList == nil:
			return c.expr(w, n.AssignmentExpression, t, mode)
		default:
			c.expr(w, n.AssignmentExpression, c.void, void)
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
	defer func() { r = c.convert(n, r, n.Type(), t) }()
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
			b.w("%s(%stls", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type(), call), tag(ccgoAutomatic))
			switch {
			case ft.MaxArgs() < 0:
				for _, v := range xargs[:ft.MinArgs()] {
					b.w(", %s", v)
				}
				b.w(", %s%sVaList(%sbp+%d", c.task.tlsQualifier, tag(preserve), tag(ccgoAutomatic), c.f.tlsAllocs+8)
				for _, v := range xargs[ft.MinArgs():] {
					b.w(", %s", v)
				}
				b.w(")")
			default:
				for _, v := range xargs {
					b.w(", %s", v)
				}
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
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	var b buf
	switch mode {
	case value:
		switch n.Case {
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			switch x := n.ResolvedTo().(type) {
			case *cc.Declarator:
				b.w("%s%s", c.declaratorTag(x), x.Name())
			default:
				c.err(errorf("TODO %T", x))
			}
		case cc.PrimaryExpressionInt: // INTCONST
			switch x := n.Value().(type) {
			case cc.Int64Value:
				switch {
				case cc.IsSignedInteger(t):
					if t.Size() < 8 {
						m := uint64(1)<<(t.Size()*8) - 1
						switch {
						case x < 0:
							x |= ^cc.Int64Value(m)
						default:
							x &= cc.Int64Value(m)
						}
					}
					b.w("int%d(%d)", 8*t.Size(), x)
				default:
					c.err(errorf("TODO %v", n.Case))
				}
			default:
				c.err(errorf("TODO %T", x))
			}
		case cc.PrimaryExpressionFloat: // FLOATCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionChar: // CHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionLChar: // LONGCHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionString: // STRINGLITERAL
			switch x := t.(type) {
			case *cc.ArrayType:
				v := n.Value().(cc.StringValue)
				v = v[:mathutil.MaxInt64(x.Len(), int64(len(v)))]
				for len(v) != 0 && v[len(v)-1] == 0 {
					v = v[:len(v)-1]
				}
				b.w("%s{", c.typ(t))
				for i := 0; i < len(v); i++ {
					b.w("%s, ", c.charConst(v[i]))
				}
				b.w("}")
			case *cc.PointerType:
				b.w("%q", n.Value())
			default:
				c.err(errorf("TODO %T", x))
			}
			t = n.Type()
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
				b.w("%s%s", c.declaratorTag(x), x.Name())
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
				b.w("%s%s", c.declaratorTag(x), x.Name())
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

func (c *ctx) charConst(b byte) string {
	switch {
	case b >= ' ' && b < 0x7f:
		return strconv.QuoteRuneToASCII(rune(b))
	case c.ast.ABI.SignedChar:
		return fmt.Sprint(int8(b))
	default:
		return fmt.Sprint(b)
	}
}

func (c *ctx) assignmentExpression(w writer, n *cc.AssignmentExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = void
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
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
