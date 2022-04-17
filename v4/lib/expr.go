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
	exprBool
	exprIndex
	exprPointer
	exprUintpr
	exprVoid
)

func (c *ctx) expr(w writer, n cc.ExpressionNode, to cc.Type, toMode mode) (r []byte) {
	if n == nil {
		if toMode != exprVoid {
			c.err(errorf("TODO"))
		}
		return nil
	}

	if x, ok := n.(*cc.ExpressionList); ok && x == nil {
		if toMode != exprVoid {
			c.err(errorf("TODO"))
		}
		return nil
	}

	from := n.Type()
	var fromMode mode
	switch {
	case to == nil:
		to = from
	case to.Kind() == cc.Void:
		toMode = exprVoid
	case toMode == exprVoid:
		to = c.void
	}
	if r, from, fromMode = c.expr0(w, n, to, toMode); from == nil {
		from = n.Type()
	}
	if fromMode == 0 {
		fromMode = expr
	}
	return c.convert(n, r, from, to, fromMode, toMode)
}

func (c *ctx) convert(n cc.Node, b []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
	if from == to {
		if fromMode == expr && toMode == exprVoid ||
			fromMode == toMode {
			return b
		}
	}

	if from.Kind() == cc.Ptr {
		return c.convertFromPointer(n, b, from.(*cc.PointerType), to, fromMode, toMode)
	}

	if to.Kind() == cc.Ptr {
		return c.convertToPointer(n, b, from, to.(*cc.PointerType), fromMode, toMode)
	}

	if toMode == exprBool {
		return c.convertToBool(n, b, from, to, fromMode, toMode)
	}

	if fromMode == exprBool {
		return c.convertFromBool(n, b, from, to, fromMode, toMode)
	}

	c.err(errorf("%v: TODO %q %s %s, %s -> %s %s, %s", c.pos(n), b, from, from.Kind(), fromMode, to, to.Kind(), toMode))
	return b //TODO
}

func (c *ctx) convertFromBool(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
	var b buf
	if toMode == expr && to.Kind() == cc.Int && to.Size() == 4 {
		b.w("%sBool32(%s)", c.task.tlsQualifier, s)
		return b.bytes()
	}

	c.err(errorf("%v: TODO %q %s %s, %s -> %s %s, %s", c.pos(n), s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
	return s //TODO
}

func (c *ctx) convertToBool(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
	var b buf
	b.w("(%s != 0)", s)
	return b.bytes()
}

func (c *ctx) convertToPointer(n cc.Node, s []byte, from cc.Type, to *cc.PointerType, fromMode, toMode mode) (r []byte) {
	var b buf
	if from.Kind() == cc.Struct && fromMode == expr && to.Elem().Kind() == cc.Struct && toMode == exprUintpr {
		b.w("uintptr(unsafe.Pointer(&%s))", s)
		return b.bytes()
	}

	if cc.IsScalarType(from) && fromMode == expr && to.Elem().Kind() == from.Kind() && toMode == exprUintpr {
		b.w("uintptr(unsafe.Pointer(&%s))", s)
		return b.bytes()
	}

	c.err(errorf("%v: TODO %q %s %s, %s -> %s %s, %s", c.pos(n), s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
	return s //TODO
}

func (c *ctx) convertFromPointer(n cc.Node, s []byte, from *cc.PointerType, to cc.Type, fromMode, toMode mode) (r []byte) {
	var b buf
	if to.Kind() == cc.Ptr {
		if fromMode == expr && toMode == expr {
			return s
		}
	}

	if to.Kind() == cc.Array {
		if ufrom := from.Undecay(); ufrom.Kind() == cc.Array {
			if fromMode == expr && toMode == exprIndex {
				return s
			}
		}
	}

	if from.Elem().Kind() == to.Kind() && fromMode == exprUintpr && toMode == expr {
		b.w("*(*%s)(unsafe.Pointer(%s))", c.typ(to), s)
		return b.bytes()
	}

	if to.Kind() == cc.Ptr {
		tpt := to.(*cc.PointerType)
		if from.Elem().Kind() == tpt.Elem().Kind() && fromMode == expr && toMode == exprPointer {
			b.w("(*%s)(unsafe.Pointer(%s))", c.typ(tpt.Elem()), s)
			return b.bytes()
		}
	}

	c.err(errorf("%v: TODO %q %s %s, %s -> %s %s, %s", c.pos(n), s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
	return s //TODO
}

func (c *ctx) expr0(w writer, n cc.ExpressionNode, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	switch x := n.(type) {
	case *cc.AdditiveExpression:
		return c.additiveExpression(w, x, t, mode)
	case *cc.AssignmentExpression:
		return c.assignmentExpression(w, x, t, mode)
	case *cc.CastExpression:
		return c.castExpression(w, x, t, mode)
	case *cc.ConditionalExpression:
		return c.conditionalExpression(w, x, t, mode)
	case *cc.ConstantExpression:
		return c.constantExpression(w, x, t, mode)
	case *cc.EqualityExpression:
		return c.equalityExpression(w, x, t, mode)
	case *cc.ExpressionList:
		return c.expressionList(w, x, t, mode)
	case *cc.LogicalOrExpression:
		return c.logicalOrExpression(w, x, t, mode)
	case *cc.MultiplicativeExpression:
		return c.multiplicativeExpression(w, x, t, mode)
	case *cc.PostfixExpression:
		return c.postfixExpression(w, x, t, mode)
	case *cc.PrimaryExpression:
		return c.primaryExpression(w, x, t, mode)
	case *cc.RelationalExpression:
		return c.relationExpression(w, x, t, mode)
	case *cc.UnaryExpression:
		return c.unaryExpression(w, x, t, mode)
	default:
		c.err(errorf("TODO %T", n))
		return nil, nil, 0
	}
}

func (c *ctx) logicalOrExpression(w writer, n *cc.LogicalOrExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) conditionalExpression(w writer, n *cc.ConditionalExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' ExpressionList ':' ConditionalExpression
		v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.f.id())
		w.w("\nvar %s %s", v, c.typ(n.Type()))
		w.w("\nif %s {", c.expr(w, n.LogicalOrExpression, nil, exprBool))
		w.w("\n%s = %s", v, c.expr(w, n.ExpressionList, nil, expr))
		w.w("\n} else {")
		w.w("\n%s = %s", v, c.expr(w, n.ConditionalExpression, nil, expr))
		w.w("\n}")
		b.w("%s", v)
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) castExpression(w writer, n *cc.CastExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		return c.expr0(w, n.UnaryExpression, t, mode)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		b.w("%s", c.expr(w, n.CastExpression, n.TypeName.Type(), expr))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) constantExpression(w writer, n *cc.ConstantExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	switch x := n.Value().(type) {
	case cc.Int64Value:
		return c.intConst(x, t)
	default:
		c.err(errorf("TODO %T", x))
	}
	return nil, rt, rmode
}

func (c *ctx) multiplicativeExpression(w writer, n *cc.MultiplicativeExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		b.w("(%s * %s)", c.expr(w, n.MultiplicativeExpression, n.Type(), expr), c.expr(w, n.CastExpression, n.Type(), expr))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		b.w("(%s / %s)", c.expr(w, n.MultiplicativeExpression, n.Type(), expr), c.expr(w, n.CastExpression, n.Type(), expr))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		b.w("(%s %% %s)", c.expr(w, n.MultiplicativeExpression, n.Type(), expr), c.expr(w, n.CastExpression, n.Type(), expr))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) additiveExpression(w writer, n *cc.AdditiveExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		switch x, y := n.AdditiveExpression.Type(), n.MultiplicativeExpression.Type(); {
		case cc.IsArithmeticType(x) && cc.IsArithmeticType(y):
			b.w("(%s + %s)", c.expr(w, n.AdditiveExpression, n.Type(), expr), c.expr(w, n.MultiplicativeExpression, n.Type(), expr))
		default:
			c.err(errorf("TODO %v + %v", x, y))
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
	return b.bytes(), rt, rmode
}

func (c *ctx) equalityExpression(w writer, n *cc.EqualityExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	if n.Case == cc.EqualityExpressionRel { // RelationalExpression
		c.err(errorf("TODO %v", n.Case))
		return b.bytes(), nil, exprBool
	}

	var ct cc.Type
	switch a, b := n.EqualityExpression.Type(), n.RelationalExpression.Type(); {
	case cc.IsRealType(a) && cc.IsRealType(b):
		ct = cc.UsualArithmeticConversions(a, b)
	case a.Kind() == cc.Ptr:
		ct = a
	case b.Kind() == cc.Ptr:
		ct = b
	}
	switch n.Case {
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		b.w("(%s == %s)", c.expr(w, n.EqualityExpression, ct, expr), c.expr(w, n.RelationalExpression, ct, expr))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		b.w("(%s != %s)", c.expr(w, n.EqualityExpression, ct, expr), c.expr(w, n.RelationalExpression, ct, expr))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), nil, exprBool
}

func (c *ctx) relationExpression(w writer, n *cc.RelationalExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	if n.Case == cc.RelationalExpressionShift { // ShiftExpression
		c.err(errorf("TODO %v", n.Case))
		return b.bytes(), nil, exprBool
	}

	var ct cc.Type
	switch a, b := n.RelationalExpression.Type(), n.ShiftExpression.Type(); {
	case cc.IsRealType(a) && cc.IsRealType(b):
		ct = cc.UsualArithmeticConversions(a, b)
	case a.Kind() == cc.Ptr:
		ct = a
	case b.Kind() == cc.Ptr:
		ct = b
	}
	switch n.Case {
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		b.w("(%s < %s)", c.expr(w, n.RelationalExpression, ct, expr), c.expr(w, n.ShiftExpression, ct, expr))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		b.w("(%s > %s)", c.expr(w, n.RelationalExpression, ct, expr), c.expr(w, n.ShiftExpression, ct, expr))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		b.w("(%s <= %s)", c.expr(w, n.RelationalExpression, ct, expr), c.expr(w, n.ShiftExpression, ct, expr))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		b.w("(%s >= %s)", c.expr(w, n.RelationalExpression, ct, expr), c.expr(w, n.ShiftExpression, ct, expr))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), nil, exprBool
}

func (c *ctx) unaryExpression(w writer, n *cc.UnaryExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		b.w("%s", c.expr(w, n.CastExpression, n.CastExpression.Type().Pointer(), exprUintpr))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		b.w("*%s", c.expr(w, n.CastExpression, nil, exprPointer))
	case cc.UnaryExpressionPlus: // '+' CastExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		b.w("-%s", c.expr(w, n.CastExpression, nil, expr))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.UnaryExpressionNot: // '!' CastExpression
		b.w("!%s", c.expr(w, n.CastExpression, nil, exprBool))
		rmode = exprBool
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		b.w("unsafe.Sizeof(%s)", c.expr(w, n.UnaryExpression, nil, expr))
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
	return b.bytes(), rt, rmode
}

func (c *ctx) postfixExpression(w writer, n *cc.PostfixExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.PostfixExpressionIndex: // PostfixExpression '[' ExpressionList ']'
		switch {
		case cc.IsIntegerType(n.PostfixExpression.Type()):
			c.err(errorf("TODO %v", n.Case))
		default:
			var a1 buf
			b1 := c.expr(&a1, n.ExpressionList, nil, expr)
			switch {
			case a1.len() != 0:
				c.err(errorf("TODO %v", n.Case))
			default:
				b.w("%s[%s]", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type().Undecay(), exprIndex), b1)
			}
		}
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		return c.postfixExpressionCall(w, n)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		b.w("%s.", c.expr(w, n.PostfixExpression, nil, expr))
		switch f := n.Field(); {
		case f.Parent() != nil:
			c.err(errorf("TODO %v", n.Case))
		default:
			b.w("%s%s", tag(field), f.Name())
		}
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		b.w("%s.", c.expr(w, n.PostfixExpression, nil, exprPointer))
		switch f := n.Field(); {
		case f.Parent() != nil:
			c.err(errorf("TODO %v", n.Case))
		default:
			b.w("%s%s", tag(field), f.Name())
		}
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		b.w("%s++", c.expr(w, n.PostfixExpression, nil, expr))
		rmode = exprVoid
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		b.w("%s--", c.expr(w, n.PostfixExpression, nil, expr))
		rmode = exprVoid
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) postfixExpressionCall(w writer, n *cc.PostfixExpression) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	ft := n.PostfixExpression.Type().(*cc.PointerType).Elem().(*cc.FunctionType)
	var args []cc.ExpressionNode
	for l := n.ArgumentExpressionList; l != nil; l = l.ArgumentExpressionList {
		args = append(args, l.AssignmentExpression)
	}
	if len(args) < ft.MinArgs() {
		c.err(errorf("%v: too few arguments to function '%s'", c.pos(n.PostfixExpression), cc.NodeSource(n.PostfixExpression)))
		return nil, nil, 0
	}

	if len(args) > ft.MaxArgs() && ft.MaxArgs() >= 0 {
		c.err(errorf("%v: too many arguments to function '%s'", c.pos(n.PostfixExpression), cc.NodeSource(n.PostfixExpression)))
		return nil, nil, 0
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
	b.w("%s(%stls", c.expr(w, n.PostfixExpression, nil, expr), tag(ccgoAutomatic))
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
	rt = ft.Result()
	rmode = expr
	if rt.Kind() == cc.Void {
		rmode = exprVoid
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) assignmentExpression(w writer, n *cc.AssignmentExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		b.w("%s = %s", c.expr(w, n.UnaryExpression, nil, expr), c.expr(w, n.AssignmentExpression, n.UnaryExpression.Type(), expr))
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
	return b.bytes(), rt, rmode
}

func (c *ctx) expressionList(w writer, n *cc.ExpressionList, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	for ; n != nil; n = n.ExpressionList {
		switch {
		case n.ExpressionList == nil:
			return c.expr0(w, n.AssignmentExpression, t, mode)
		default:
			c.expr(w, n.AssignmentExpression, nil, exprVoid)
		}
	}
	c.err(errorf("TODO internal error", n))
	return nil, nil, expr
}

func (c *ctx) primaryExpression(w writer, n *cc.PrimaryExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch x := n.ResolvedTo().(type) {
		case *cc.Declarator:
			if x.StorageDuration() == cc.Automatic {
				if info := c.f.declInfos.info(x); info.escapes() {
					b.w("(%sbp+%d)", tag(ccgoAutomatic), info.bpOff)
					rt = x.Type().Pointer()
					rmode = exprUintpr
					break
				}
			}

			b.w("%s%s", c.declaratorTag(x), x.Name())
		default:
			c.err(errorf("TODO %T", x))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		return c.intConst(n.Value(), t)
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
				switch {
				case x >= ' ' && x < 0x7f:
					b.w("int%d(%s)", 8*t.Size(), (strconv.QuoteRuneToASCII(rune(x))))
				default:
					b.w("int%d(%d)", 8*t.Size(), x)
				}
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
			rt = t
			v := n.Value().(cc.StringValue)
			max := x.Len()
			for len(v) != 0 && v[len(v)-1] == 0 {
				v = v[:len(v)-1]
			}
			b.w("%s{", c.typ(rt))
			for i := 0; i < len(v) && int64(i) < max; i++ {
				b.w("%s, ", c.charConst(v[i]))
			}
			b.w("}")
		case *cc.PointerType:
			b.w("%q", n.Value())
		default:
			c.err(errorf("TODO %T", x))
		}
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		c.err(errorf("TODO %v", n.Case))
	case cc.PrimaryExpressionExpr: // '(' ExpressionList ')'
		return c.expr0(w, n.ExpressionList, t, mode)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		c.err(errorf("TODO %v", n.Case))
	case cc.PrimaryExpressionGeneric: // GenericSelection
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) intConst(v cc.Value, t cc.Type) (r []byte, rt cc.Type, rmode mode) {
	if !cc.IsIntegerType(t) && t.Kind() != cc.Ptr {
		c.err(errorf("TODO"))
		return nil, rt, rmode
	}

	var b buf
	switch x := v.(type) {
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
			if t.Size() < 8 {
				m := uint64(1)<<(t.Size()*8) - 1
				x &= cc.Int64Value(m)
			}
			switch {
			case t.Kind() == cc.Ptr:
				b.w("uintptr(%d)", uint64(x))
			default:
				b.w("uint%d(%d)", 8*t.Size(), uint64(x))
			}
		}
	case cc.UInt64Value:
		switch {
		case cc.IsSignedInteger(t):
			c.err(errorf("TODO"))
		default:
			if t.Size() < 8 {
				m := uint64(1)<<(t.Size()*8) - 1
				x &= cc.UInt64Value(m)
			}
			switch {
			case t.Kind() == cc.Ptr:
				b.w("uintptr(%d)", uint64(x))
			default:
				b.w("uint%d(%d)", 8*t.Size(), uint64(x))
			}
		}
	default:
		c.err(errorf("TODO %T", x))
	}
	return b.bytes(), t, rmode
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
