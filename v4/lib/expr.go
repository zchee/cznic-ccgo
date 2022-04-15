// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"strconv"

	"modernc.org/cc/v4"
)

type mode int

const (
	_ mode = iota
	expr
	exprAddr
	exprBool // Go bool value
	exprVoid
)

func (c *ctx) convert(n cc.Node, b []byte, from, to cc.Type) (r []byte) {
	if from == to || to.Kind() == cc.Void {
		return b
	}

	if from.Kind() == cc.Ptr && to.Kind() == cc.Ptr {
		return b
	}

	c.err(errorf("%v: TODO %q %s -> %s", c.pos(n), b, from, to))
	return b //TODO
}

func (c *ctx) expressionList(w writer, n *cc.ExpressionList, t cc.Type, mode mode) (r []byte) {
	if n == nil {
		return nil
	}

	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	from := n.Type()
	defer func(n *cc.ExpressionList) {
		if n != nil {
			r = c.convert(n, r, from, t)
		}
	}(n)
	switch mode {
	case expr, exprBool, exprVoid:
		for ; n != nil; n = n.ExpressionList {
			switch {
			case n.ExpressionList == nil:
				return c.expr(w, n.AssignmentExpression, t, mode)
			default:
				c.expr(w, n.AssignmentExpression, c.void, exprVoid)
			}
		}
	case exprAddr:
		from = from.Pointer()
		for ; n != nil; n = n.ExpressionList {
			switch {
			case n.ExpressionList == nil:
				return c.expr(w, n.AssignmentExpression, t, mode)
			default:
				c.expr(w, n.AssignmentExpression, c.void, exprVoid)
			}
		}
	default:
		c.err(errorf("TODO %v", mode))
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
	case *cc.ExpressionList:
		return c.expressionList(w, x, t, mode)
	case *cc.UnaryExpression:
		return c.unaryExpression(w, x, t, mode)
	case *cc.RelationalExpression:
		return c.relationExpression(w, x, t, mode)
	case *cc.MultiplicativeExpression:
		return c.multiplicativeExpression(w, x, t, mode)
	case *cc.AdditiveExpression:
		return c.additiveExpression(w, x, t, mode)
	case *cc.ConstantExpression:
		return c.constantExpression(w, x, t, mode)
	case *cc.CastExpression:
		return c.castExpression(w, x, t, mode)
	default:
		c.err(errorf("%v: TODO %T %s", c.pos(n), x, cc.NodeSource(n)))
		return nil
	}
}

func (c *ctx) castExpression(w writer, n *cc.CastExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	from := n.Type()
	defer func() { r = c.convert(n, r, from, t) }()
	var b buf
	switch mode {
	case exprAddr:
		from = from.Pointer()
		switch n.Case {
		case cc.CastExpressionUnary: // UnaryExpression
			return c.expr(w, n.UnaryExpression, n.Type(), mode)
		case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return b.bytes()
}

func (c *ctx) constantExpression(w writer, n *cc.ConstantExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	var b buf
	switch mode {
	case expr:
		//TODO dedup with primaryExpression intconst
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
				c.err(errorf("TODO"))
			}
		default:
			c.err(errorf("TODO %T", x))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return b.bytes()
}

func (c *ctx) additiveExpression(w writer, n *cc.AdditiveExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	var b buf
	switch mode {
	case expr:
		switch n.Case {
		case cc.AdditiveExpressionMul: // MultiplicativeExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
			switch x, y := n.AdditiveExpression.Type(), n.MultiplicativeExpression.Type(); {
			case cc.IsArithmeticType(x) && cc.IsArithmeticType(y):
				b.w("(%s + %s)", c.expr(w, n.AdditiveExpression, n.Type(), expr), c.expr(w, n.MultiplicativeExpression, n.Type(), expr))
			default:
				c.err(errorf("TODO %v - %v", x, y))
			}
		case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
			switch x, y := n.AdditiveExpression.Type(), n.MultiplicativeExpression.Type(); {
			case cc.IsArithmeticType(x) && cc.IsArithmeticType(y):
				b.w("(%s - %s)", c.expr(w, n.AdditiveExpression, n.Type(), expr), c.expr(w, n.MultiplicativeExpression, n.Type(), expr))
			default:
				c.err(errorf("TODO %v - %v", x, y))
			}
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return b.bytes()
}

func (c *ctx) multiplicativeExpression(w writer, n *cc.MultiplicativeExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	var b buf
	switch mode {
	case expr:
		switch n.Case {
		case cc.MultiplicativeExpressionCast: // CastExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
			b.w("(%s * %s)", c.expr(w, n.MultiplicativeExpression, n.Type(), expr), c.expr(w, n.CastExpression, n.Type(), expr))
		case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return b.bytes()
}

func (c *ctx) relationExpression(w writer, n *cc.RelationalExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	var b buf
	switch mode {
	case exprBool:
		switch n.Case {
		case cc.RelationalExpressionShift: // ShiftExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
			ct := cc.UsualArithmeticConversions(n.RelationalExpression.Type(), n.ShiftExpression.Type())
			b.w("(%s < %s)", c.expr(w, n.RelationalExpression, ct, expr), c.expr(w, n.ShiftExpression, ct, expr))
		case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
			ct := cc.UsualArithmeticConversions(n.RelationalExpression.Type(), n.ShiftExpression.Type())
			b.w("(%s <= %s)", c.expr(w, n.RelationalExpression, ct, expr), c.expr(w, n.ShiftExpression, ct, expr))
		case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return b.bytes()
}

func (c *ctx) unaryExpression(w writer, n *cc.UnaryExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	var b buf
	switch mode {
	case expr:
		switch n.Case {
		case cc.UnaryExpressionPostfix: // PostfixExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionInc: // "++" UnaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionDec: // "--" UnaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionAddrof: // '&' CastExpression
			b.w("%s", c.expr(w, n.CastExpression, n.Type().Pointer(), exprAddr))
		case cc.UnaryExpressionDeref: // '*' CastExpression
			b.w("*%s", c.expr(w, n.CastExpression, n.CastExpression.Type(), expr))
		case cc.UnaryExpressionPlus: // '+' CastExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionMinus: // '-' CastExpression
			b.w("-(")
			b.w("%s", c.expr(w, n.CastExpression, n.CastExpression.Type(), expr))
			b.w(")")
		case cc.UnaryExpressionCpl: // '~' CastExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionNot: // '!' CastExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.UnaryExpressionReal: // "__real__" UnaryExpression
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	default:
		c.err(errorf("TODO %v", mode))
	}
	return b.bytes()
}

func (c *ctx) postfixExpression(w writer, n *cc.PostfixExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	from := n.Type()
	defer func() { r = c.convert(n, r, from, t) }()
	var b buf
	switch mode {
	case exprVoid:
		switch n.Case {
		case cc.PostfixExpressionPrimary: // PrimaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionIndex: // PostfixExpression '[' ExpressionList ']'
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
			return c.postfixExpressionCall(w, n)
		case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionInc: // PostfixExpression "++"
			b.w("%s++", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type(), expr))
		case cc.PostfixExpressionDec: // PostfixExpression "--"
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	case expr:
		switch n.Case {
		case cc.PostfixExpressionPrimary: // PrimaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionIndex: // PostfixExpression '[' ExpressionList ']'
			switch {
			case cc.IsIntegerType(n.PostfixExpression.Type()):
				c.err(errorf("TODO %v", n.Case))
			default:
				var a1 buf
				b1 := c.expressionList(&a1, n.ExpressionList, n.ExpressionList.Type(), expr)
				switch {
				case a1.len() != 0:
					c.err(errorf("TODO %v", n.Case))
				default:
					b.w("%s[%s]", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type(), expr), b1)
				}
			}
		case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
			return c.postfixExpressionCall(w, n)
		case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
			b.w("%s.", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type(), expr))
			switch f := n.Field(); {
			case f.Parent() != nil:
				c.err(errorf("TODO %v", n.Case))
			default:
				b.w("%s%s", tag(field), f.Name())
			}
		case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
			b.w("%s.", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type(), expr))
			switch f := n.Field(); {
			case f.Parent() != nil:
				c.err(errorf("TODO %v", n.Case))
			default:
				b.w("%s%s", tag(field), f.Name())
			}
		case cc.PostfixExpressionInc: // PostfixExpression "++"
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionDec: // PostfixExpression "--"
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
			c.err(errorf("TODO %v", n.Case))
		default:
			c.err(errorf("internal error %T %v", n, n.Case))
		}
	case exprAddr:
		from = from.Pointer()
		switch n.Case {
		case cc.PostfixExpressionPrimary: // PrimaryExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionIndex: // PostfixExpression '[' ExpressionList ']'
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
			c.err(errorf("TODO %v", n.Case))
		case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
			b.w("uintptr(unsafe.Pointer(&%s))", c.expr(w, n, n.Type(), expr))
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
	trc("%v: case %v, src %s, mode %v, n.Type %v from %v to %v", n.Position(), n.Case, cc.NodeSource(n), mode, n.Type(), from, t)
	return b.bytes()
}

func (c *ctx) postfixExpressionCall(w writer, n *cc.PostfixExpression) (r []byte) {
	var b buf
	ft := n.PostfixExpression.Type().(*cc.PointerType).Elem().(*cc.FunctionType)
	var args []cc.ExpressionNode
	for l := n.ArgumentExpressionList; l != nil; l = l.ArgumentExpressionList {
		args = append(args, l.AssignmentExpression)
	}
	if len(args) < ft.MinArgs() {
		c.err(errorf("%v: too few arguments to function '%s'", c.pos(n.PostfixExpression), cc.NodeSource(n.PostfixExpression)))
		return nil
	}

	if len(args) > ft.MaxArgs() && ft.MaxArgs() >= 0 {
		c.err(errorf("%v: too many arguments to function '%s'", c.pos(n.PostfixExpression), cc.NodeSource(n.PostfixExpression)))
		return nil
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
		xargs = append(xargs, c.expr(w, v, t, expr))
	}
	b.w("%s(%stls", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type(), expr), tag(ccgoAutomatic))
	switch {
	case ft.IsVariadic():
		for _, v := range xargs[:ft.MinArgs()] {
			b.w(", %s", v)
		}
		switch {
		case len(xargs) == ft.MinArgs():
			b.w(", 0")
		default:
			b.w(", %s%sVaList(%sbp+%d", c.task.tlsQualifier, tag(preserve), tag(ccgoAutomatic), c.f.tlsAllocs+8)
			for _, v := range xargs[ft.MinArgs():] {
				b.w(", %s", v)
			}
			b.w(")")
		}
	default:
		for _, v := range xargs {
			b.w(", %s", v)
		}
	}
	b.w(")")
	return b.bytes()
}

func (c *ctx) primaryExpression(w writer, n *cc.PrimaryExpression, t cc.Type, mode mode) (r []byte) {
	if t.Kind() == cc.Void {
		mode = exprVoid
	}
	from := n.Type()
	defer func() { r = c.convert(n, r, from, t) }()
	var b buf
	switch mode {
	case expr:
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
		case cc.PrimaryExpressionLChar: // LONGCHARCONST
			c.err(errorf("TODO %v", n.Case))
		case cc.PrimaryExpressionString: // STRINGLITERAL
			switch x := t.(type) {
			case *cc.ArrayType:
				v := n.Value().(cc.StringValue)
				max := x.Len()
				for len(v) != 0 && v[len(v)-1] == 0 {
					v = v[:len(v)-1]
				}
				b.w("%s{", c.typ(t))
				for i := 0; i < len(v) && int64(i) < max; i++ {
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
	case exprAddr:
		from = from.Pointer()
		switch n.Case {
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			switch x := n.ResolvedTo().(type) {
			case *cc.Declarator:
				switch x.StorageDuration() {
				case cc.Automatic:
					b.w("(%sbp+%d)", tag(ccgoAutomatic), c.f.declInfos[x].bpOff)
				case cc.Static:
					b.w("uintptr(unsafe.Pointer(&%s%s))", c.declaratorTag(x), x.Name())
				default:
					c.err(errorf("TODO %v", x.StorageDuration()))
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
			return c.expr(w, n.ExpressionList, n.ExpressionList.Type(), mode)
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
		mode = exprVoid
	}
	defer func() { r = c.convert(n, r, n.Type(), t) }()
	var b buf
	switch mode {
	case exprVoid:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			c.err(errorf("TODO %v", n.Case))
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			ae := c.expr(w, n.AssignmentExpression, n.UnaryExpression.Type(), expr)
			b.w("%s = %s", c.expr(w, n.UnaryExpression, n.UnaryExpression.Type(), expr), ae)
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
