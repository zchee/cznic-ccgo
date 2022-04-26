// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"strconv"
	"unicode"

	"modernc.org/cc/v4"
)

type mode int

const (
	_           mode = iota
	exprBool         // C scalar type, Go bool
	exprCall         // C func pointer, Go function value
	exprDefault      //
	exprIndex        // C pointer, Go array
	exprLvalue       //
	exprSelect       // C struct, Go struct
	exprUintpr       // C pointer, Go uintptr
	exprUntyped      // C primary expr literal, Go typed literal
	exprVoid         // C void, no Go equivalent
)

func (c *ctx) expr(w writer, n cc.ExpressionNode, to cc.Type, toMode mode) *buf {
	if toMode == 0 {
		c.err(errorf("internal error"))
		return nil
	}

	if n == nil {
		if toMode != exprVoid {
			c.err(errorf("TODO"))
		}
		return &buf{}
	}

	if x, ok := n.(*cc.ExpressionList); ok && x == nil {
		if toMode != exprVoid {
			c.err(errorf("TODO"))
		}
		return &buf{}
	}

	if to == nil {
		to = n.Type()
	}
	r, from, fromMode := c.expr0(w, n, to, toMode)
	if assert && (from == nil || fromMode == 0) {
		//trc("", cpos(n))
		c.err(errorf("TODO %T %v %v", n, from, fromMode))
		return r
	}

	return c.convert(n, w, r, from, to, fromMode, toMode)
}

func (c *ctx) convert(n cc.Node, w writer, s *buf, from, to cc.Type, fromMode, toMode mode) (r *buf) {
	// defer func() {
	// 	trc("%v: from %v: %v, to %v: %v %q %T -> %q %T (%q)", c.pos(n), from, fromMode, to, toMode, s.bytes(), s.n, r.bytes(), r.n, cc.NodeSource(n))
	// }()
	if assert && to == nil {
		c.err(errorf("TODO"))
		return s
	}
	if from != nil && from.Kind() == cc.Enum {
		from = from.(*cc.EnumType).UnderlyingType()
	}
	if to.Kind() == cc.Enum {
		to = to.(*cc.EnumType).UnderlyingType()
	}
	if fromMode == exprUntyped {
		return c.convertUntyped(n, s, from, to, fromMode, toMode)
	}

	if from == to || from != nil && from.IsCompatible(to) {
		if from == c.ast.SizeT || to == c.ast.SizeT {
			if toMode != exprVoid {
				return c.convertType(n, s, from, to, fromMode, toMode)
			}
		}

		if fromMode == toMode {
			return s
		}

		return c.convertMode(n, w, s, from, to, fromMode, toMode)
	}

	if fromMode == toMode {
		return c.convertType(n, s, from, to, fromMode, toMode)
	}

	if from != nil && from.Kind() == cc.Ptr {
		return c.convertFromPointer(n, s, from.(*cc.PointerType), to, fromMode, toMode)
	}

	if toMode == exprVoid {
		return s
	}

	if to.Kind() == cc.Ptr {
		return c.convertToPointer(n, s, from, to.(*cc.PointerType), fromMode, toMode)
	}

	c.err(errorf("TODO %q %s %s -> %s %s", s, from, fromMode, to, toMode))
	return s //TODO
}

func (c *ctx) convertToPointer(n cc.Node, s *buf, from cc.Type, to *cc.PointerType, fromMode, toMode mode) (r *buf) {
	var b buf
	switch fromMode {
	case exprDefault:
		switch toMode {
		case exprUintpr:
			b.w("uintptr(unsafe.%sPointer(&%s))", tag(preserve), c.pin(n, s))
			return &b
		}
	}

	c.err(errorf("TODO %q %s %s -> %s %s", s, from, fromMode, to, toMode))
	return s //TODO
}

func (c *ctx) pin(n cc.Node, b *buf) *buf {
	switch x := b.n.(type) {
	case *cc.Declarator:
		switch c.pass {
		case 0:
			// ok
		case 1:
			switch symKind(string(b.bytes())) {
			case automatic, ccgoAutomatic:
				c.f.declInfos.takeAddress(x)
				//trc("%v: PIN %v at %v (%v: %v:)", c.pos(n), x.Name(), c.pos(x), origin(3), origin(2))
			}
		case 2:
			// ok
		default:
			c.err(errorf("%v: internal error: %d", n.Position(), c.pass))
		}
	}
	return b
}

func (c *ctx) convertUntyped(n cc.Node, s *buf, from, to cc.Type, fromMode, toMode mode) (r *buf) {
	// defer func() { trc("%v: from %v: %v, to %v: %v %q -> %q", c.pos(n), from, fromMode, to, toMode, s, r) }()
	if toMode != exprDefault {
		c.err(errorf("TODO %v: %v", n.Position(), toMode))
	}
	var b buf
	from = from.Undecay()
	switch {
	case from.Kind() == cc.Array:
		switch from := from.(*cc.ArrayType); {
		case c.isCharType(from.Elem()):
			switch to.Kind() {
			case cc.Array:
				to := to.(*cc.ArrayType)
				max := to.Len()
				a := s.bytes()
				for len(a) != 0 && a[len(a)-1] == 0 {
					a = a[:len(a)-1]
				}
				b.w("%s{", c.typ(to))
				for i := 0; i < len(a) && int64(i) < max; i++ {
					b.w("%s, ", c.stringCharConst(a[i]))
				}
				b.w("}")
				return &b
			case cc.Ptr:
				to := to.(*cc.PointerType)
				if c.isCharType(to.Elem()) || to.Elem().Kind() == cc.Void {
					b.w("%q", s)
					return &b
				}
			default:
				c.err(errorf("TODO"))
			}
		default:
			c.err(errorf("TODO"))
		}
	case cc.IsIntegerType(from):
		switch {
		case cc.IsIntegerType(to):
			var val uint64
			switch {
			case cc.IsSignedInteger(from):
				v, err := strconv.ParseInt(string(s.bytes()), 10, 64)
				if err != nil {
					c.err(errorf("internal error: %v", err))
					break
				}

				val = uint64(v)
			default:
				v, err := strconv.ParseUint(string(s.bytes()), 10, 64)
				if err != nil {
					c.err(errorf("internal error: %v", err))
					break
				}

				val = uint64(v)
			}
			switch {
			case cc.IsSignedInteger(to):
				if to.Size() < 8 {
					m := uint64(1)<<(to.Size()*8) - 1
					switch {
					case int64(val) < 0:
						val |= m
					default:
						val &= m
					}
				}
				b.w("int%d(%d)", 8*to.Size(), int64(val))
			default:
				if to.Size() < 8 {
					m := uint64(1)<<(to.Size()*8) - 1
					val &= m
				}
				b.w("uint%d(%d)", 8*to.Size(), uint64(val))
			}
		case to.Kind() == cc.Ptr:
			b.w("uintptr(%s)", s)
		case cc.IsArithmeticType(to):
			b.w("%s(%s)", c.typ(to), s)
		case to.Kind() == cc.Struct:
			c.err(errorf("TODO"))
		case to.Kind() == cc.Union:
			c.err(errorf("TODO"))
		case to.Kind() == cc.Array:
			if !isZeroString(string(s.bytes())) {
				c.err(errorf("TODO"))
			}
			return s
		case to.Kind() == cc.Void:
			return nil
		default:
			// trc("%v: %q %s %s -> %s %s", cpos(n), s, from, fromMode, to, toMode)
			c.err(errorf("TODO"))
		}
		return &b
	case cc.IsArithmeticType(from):
		switch {
		case cc.IsArithmeticType(to):
			b.w("%s(%s)", c.typ(to), s)
		default:
			c.err(errorf("TODO"))
		}
		return &b
	}

	c.err(errorf("TODO %q %s %s -> %s %s", s, from, fromMode, to, toMode))
	return s //TODO
}

func isZeroString(s string) bool { return s == "0" }

// type unchanged
func (c *ctx) convertMode(n cc.Node, w writer, s *buf, from, to cc.Type, fromMode, toMode mode) (r *buf) {
	// defer func() { trc("%v: from %v: %v, to %v: %v %q -> %q", c.pos(n), from, fromMode, to, toMode, b, r) }()
	var b buf
	switch fromMode {
	case exprDefault:
		switch toMode {
		case exprLvalue:
			return s
		case exprCall:
			return s
		case exprVoid:
			return s
		case exprBool:
			b.w("(%s != 0)", s)
			return &b
		}
	case exprUintpr:
		switch toMode {
		case exprDefault:
			return s
		case exprCall:
			v := fmt.Sprintf("%sf%d", tag(ccgo), c.id())
			ft := from.(*cc.PointerType).Elem().(*cc.FunctionType)
			w.w("\nvar %s func%s", v, c.signature(ft, false, false))
			w.w("\n*(*uintptr)(unsafe.%sPointer(&%s)) = %s", tag(preserve), v, s) // Free pass from .pin
			var b buf
			b.w("%s", v)
			return &b
		}
	case exprBool:
		switch toMode {
		case exprDefault:
			switch {
			case cc.IsIntegerType(to):
				b.w("%s%sBool%s(%s)", c.task.tlsQualifier, tag(preserve), c.typeSuffix(to), s)
				return &b
			}
		}
	}
	c.err(errorf("TODO %q %s %s -> %s %s", s, from, fromMode, to, toMode))
	return s //TODO
}

func (c *ctx) isIdent(s string) bool {
	for i, v := range s {
		switch {
		case i == 0:
			if !unicode.IsLetter(v) && v != '_' {
				return false
			}
		default:
			if !unicode.IsLetter(v) && v != '_' && !unicode.IsDigit(v) {
				return false
			}
		}
	}
	return len(s) != 0
}

// mode unchanged
func (c *ctx) convertType(n cc.Node, s *buf, from, to cc.Type, fromMode, toMode mode) (r *buf) {
	// defer func() { trc("%v: from %v: %v, to %v: %v %q -> %q", c.pos(n), from, fromMode, to, toMode, s, r) }()
	var b buf
	if from.Kind() == cc.Ptr && to.Kind() == cc.Ptr {
		return s
	}

	if to.Kind() == cc.Void {
		return s
	}

	if cc.IsScalarType(from) && cc.IsScalarType(to) {
		switch {
		case cc.IsIntegerType(from) && cc.IsIntegerType(to) && cc.IsSignedInteger(from) != cc.IsSignedInteger(to):
			b.w("%s%s%sFrom%s(%s)", c.task.tlsQualifier, tag(preserve), c.export(c.noNameType(to)), c.export(c.noNameType(from)), s)
		default:
			b.w("%s(%s)", c.typ(to), s)
		}
		return &b
	}

	c.err(errorf("TODO %q %s %s -> %s %s (%v:)", s, from, fromMode, to, toMode, c.pos(n)))
	return s //TODO
}

func (c *ctx) isCharType(t cc.Type) bool {
	switch t.Kind() {
	case cc.Char, cc.UChar, cc.SChar:
		return true
	}

	return false
}

func (c *ctx) convertFromPointer(n cc.Node, s *buf, from *cc.PointerType, to cc.Type, fromMode, toMode mode) (r *buf) {
	var b buf
	if to.Kind() == cc.Ptr {
		if fromMode == exprUintpr && toMode == exprDefault {
			return s
		}

		if fromMode == exprDefault && toMode == exprUintpr {
			b.w("uintptr(unsafe.%sPointer(&%s))", tag(preserve), c.pin(n, s))
			return &b
		}
	}

	c.err(errorf("TODO %q %s %s, %s -> %s %s, %s", s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
	return s //TODO
}

func (c *ctx) expr0(w writer, n cc.ExpressionNode, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	// trc("%v: %T (%q)", n.Position(), n, cc.NodeSource(n))
	if mode == exprBool {
		mode = exprDefault
	}
	switch x := n.(type) {
	case *cc.AdditiveExpression:
		return c.additiveExpression(w, x, t, mode)
	case *cc.AndExpression:
		return c.andExpression(w, x, t, mode)
	case *cc.AssignmentExpression:
		return c.assignmentExpression(w, x, t, mode)
	case *cc.CastExpression:
		return c.castExpression(w, x, t, mode)
	case *cc.ConstantExpression:
		return c.expr0(w, x.ConditionalExpression, t, mode)
	case *cc.ConditionalExpression:
		return c.conditionalExpression(w, x, t, mode)
	case *cc.EqualityExpression:
		return c.equalityExpression(w, x, t, mode)
	case *cc.ExclusiveOrExpression:
		return c.exclusiveOrExpression(w, x, t, mode)
	case *cc.ExpressionList:
		return c.expressionList(w, x, t, mode)
	case *cc.InclusiveOrExpression:
		return c.inclusiveOrExpression(w, x, t, mode)
	case *cc.LogicalAndExpression:
		return c.logicalAndExpression(w, x, t, mode)
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
	case *cc.ShiftExpression:
		return c.shiftExpression(w, x, t, mode)
	case *cc.UnaryExpression:
		return c.unaryExpression(w, x, t, mode)
	default:
		c.err(errorf("TODO %T", x))
		return nil, nil, 0
	}
}

func (c *ctx) andExpression(w writer, n *cc.AndExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		b.w("(%s & %s)", c.expr(w, n.AndExpression, n.Type(), exprDefault), c.expr(w, n.EqualityExpression, n.Type(), exprDefault))
		rt, rmode = n.Type(), exprDefault
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) exclusiveOrExpression(w writer, n *cc.ExclusiveOrExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		b.w("(%s ^ %s)", c.expr(w, n.ExclusiveOrExpression, n.Type(), exprDefault), c.expr(w, n.AndExpression, n.Type(), exprDefault))
		rt, rmode = n.Type(), exprDefault
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) inclusiveOrExpression(w writer, n *cc.InclusiveOrExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		b.w("(%s | %s)", c.expr(w, n.InclusiveOrExpression, n.Type(), exprDefault), c.expr(w, n.ExclusiveOrExpression, n.Type(), exprDefault))
		rt, rmode = n.Type(), exprDefault
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) shiftExpression(w writer, n *cc.ShiftExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		b.w("(%s << %s)", c.expr(w, n.ShiftExpression, nil, exprDefault), c.expr(w, n.AdditiveExpression, nil, exprDefault))
		rt, rmode = n.Type(), exprDefault
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		b.w("(%s >> %s)", c.expr(w, n.ShiftExpression, nil, exprDefault), c.expr(w, n.AdditiveExpression, nil, exprDefault))
		rt, rmode = n.Type(), exprDefault
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) logicalAndExpression(w writer, n *cc.LogicalAndExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		b.w("(%s && %s)", c.expr(w, n.LogicalAndExpression, nil, exprBool), c.expr(w, n.InclusiveOrExpression, nil, exprBool))
		rt, rmode = n.Type(), exprBool
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) logicalOrExpression(w writer, n *cc.LogicalOrExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		b.w("(%s || %s)", c.expr(w, n.LogicalOrExpression, nil, exprBool), c.expr(w, n.LogicalAndExpression, nil, exprBool))
		rt, rmode = n.Type(), exprBool
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) conditionalExpression(w writer, n *cc.ConditionalExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' ExpressionList ':' ConditionalExpression
		rt, rmode = n.Type(), exprDefault
		v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
		w.w("\nvar %s %s", v, c.typ(n.Type()))
		w.w("\nif %s {", c.expr(w, n.LogicalOrExpression, nil, exprBool))
		w.w("\n%s = %s", v, c.expr(w, n.ExpressionList, nil, exprDefault))
		w.w("\n} else {")
		w.w("\n%s = %s", v, c.expr(w, n.ConditionalExpression, nil, exprDefault))
		w.w("\n}")
		b.w("%s", v)
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) castExpression(w writer, n *cc.CastExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		return c.expr0(w, n.UnaryExpression, t, mode)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		rt, rmode = n.Type(), mode
		b.w("%s", c.expr(w, n.CastExpression, n.Type(), exprDefault))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) multiplicativeExpression(w writer, n *cc.MultiplicativeExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	rt, rmode = n.Type(), exprDefault
	var b buf
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		b.w("(%s * %s)", c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault), c.expr(w, n.CastExpression, n.Type(), exprDefault))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		b.w("(%s / %s)", c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault), c.expr(w, n.CastExpression, n.Type(), exprDefault))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		b.w("(%s %% %s)", c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault), c.expr(w, n.CastExpression, n.Type(), exprDefault))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) additiveExpression(w writer, n *cc.AdditiveExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	rt, rmode = n.Type(), mode
	var b buf
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		switch x, y := n.AdditiveExpression.Type(), n.MultiplicativeExpression.Type(); {
		case cc.IsArithmeticType(x) && cc.IsArithmeticType(y):
			b.w("(%s + %s)", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault))
		case x.Kind() == cc.Ptr && cc.IsIntegerType(y):
			s := ""
			if sz := x.(*cc.PointerType).Elem().Size(); sz != 1 {
				s = fmt.Sprintf("*%d", sz)
			}
			b.w("(%s + ((%s)%s))", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault), s)
		case cc.IsIntegerType(x) && y.Kind() == cc.Ptr:
			s := ""
			if sz := y.(*cc.PointerType).Elem().Size(); sz != 1 {
				s = fmt.Sprintf("*%d", sz)
			}
			b.w("(((%s)%s)+%s)", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), s, c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault))
		default:
			c.err(errorf("TODO %v + %v", x, y)) // -
		}
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		switch x, y := n.AdditiveExpression.Type(), n.MultiplicativeExpression.Type(); {
		case cc.IsArithmeticType(x) && cc.IsArithmeticType(y):
			b.w("(%s - %s)", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault))
		case x.Kind() == cc.Ptr && y.Kind() == cc.Ptr:
			b.w("((%s - %s)", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault))
			if v := x.(*cc.PointerType).Elem().Size(); v > 1 {
				b.w("/%d", v)
			}
			b.w(")")
		case x.Kind() == cc.Ptr && cc.IsIntegerType(y):
			s := ""
			if sz := x.(*cc.PointerType).Elem().Size(); sz != 1 {
				s = fmt.Sprintf("*%d", sz)
			}
			b.w("(%s - ((%s)%s))", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault), s)
		default:
			c.err(errorf("TODO %v - %v", x, y))
		}
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) equalityExpression(w writer, n *cc.EqualityExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	if n.Case == cc.EqualityExpressionRel { // RelationalExpression
		c.err(errorf("TODO %v", n.Case))
		return &b, nil, exprBool
	}

	ct := c.usualArithmeticConversions(n.EqualityExpression.Type(), n.RelationalExpression.Type())
	// emode := expr
	// if x.Undecay().Kind() == cc.Array || y.Undecay().Kind() == cc.Array {
	// 	emode = exprUintpr
	// }
	switch n.Case {
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		b.w("(%s == %s)", c.expr(w, n.EqualityExpression, ct, exprDefault), c.expr(w, n.RelationalExpression, ct, exprDefault))
		rt, rmode = n.Type(), exprBool
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		b.w("(%s != %s)", c.expr(w, n.EqualityExpression, ct, exprDefault), c.expr(w, n.RelationalExpression, ct, exprDefault))
		rt, rmode = n.Type(), exprBool
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) relationExpression(w writer, n *cc.RelationalExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	if n.Case == cc.RelationalExpressionShift { // ShiftExpression
		c.err(errorf("TODO %v", n.Case))
		return &b, nil, exprBool
	}

	ct := c.usualArithmeticConversions(n.RelationalExpression.Type(), n.ShiftExpression.Type())
	rt, rmode = n.Type(), exprBool
	switch n.Case {
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		b.w("(%s < %s)", c.expr(w, n.RelationalExpression, ct, exprDefault), c.expr(w, n.ShiftExpression, ct, exprDefault))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		b.w("(%s > %s)", c.expr(w, n.RelationalExpression, ct, exprDefault), c.expr(w, n.ShiftExpression, ct, exprDefault))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		b.w("(%s <= %s)", c.expr(w, n.RelationalExpression, ct, exprDefault), c.expr(w, n.ShiftExpression, ct, exprDefault))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		b.w("(%s >= %s)", c.expr(w, n.RelationalExpression, ct, exprDefault), c.expr(w, n.ShiftExpression, ct, exprDefault))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) usualArithmeticConversions(a, b cc.Type) (r cc.Type) {
	if a.Kind() == cc.Ptr && (cc.IsIntegerType(b) || b.Kind() == cc.Ptr) {
		return a
	}

	if b.Kind() == cc.Ptr && (cc.IsIntegerType(a) || a.Kind() == cc.Ptr) {
		return b
	}

	return cc.UsualArithmeticConversions(a, b)
}

func (c *ctx) unaryExpression(w writer, n *cc.UnaryExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
out:
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		rt, rmode = n.Type(), mode
		switch ue := n.UnaryExpression.Type(); {
		case ue.Kind() == cc.Ptr && ue.(*cc.PointerType).Elem().Size() != 1:
			sz := ue.(*cc.PointerType).Elem().Size()
			switch mode {
			case exprVoid:
				b.w("%s += %d", c.expr(w, n.UnaryExpression, nil, exprDefault), sz)
			case exprDefault:
				switch d := c.declaratorOf(n.UnaryExpression); {
				case d != nil:
					ds := c.expr(w, n.UnaryExpression, nil, exprDefault)
					w.w("\n%s += %d", ds, sz)
					b.w("%s", ds)
				default:
					c.err(errorf("TODO")) // -
				}
			default:
				c.err(errorf("TODO %v", mode)) // -
			}
		default:
			switch mode {
			case exprVoid:
				b.w("%s++", c.expr(w, n.UnaryExpression, nil, exprDefault))
			case exprDefault:
				switch d := c.declaratorOf(n.UnaryExpression); {
				case d != nil:
					ds := c.expr(w, n.UnaryExpression, nil, exprDefault)
					w.w("\n%s++", ds)
					b.w("%s", ds)
				default:
					c.err(errorf("TODO")) // 1: bit field
				}
			default:
				c.err(errorf("TODO %v", mode)) // -
			}
		}
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		rt, rmode = n.Type(), mode
		switch ue := n.UnaryExpression.Type(); {
		case ue.Kind() == cc.Ptr && ue.(*cc.PointerType).Elem().Size() != 1:
			sz := ue.(*cc.PointerType).Elem().Size()
			switch mode {
			case exprVoid:
				c.err(errorf("TODO"))
			case exprDefault:
				switch d := c.declaratorOf(n.UnaryExpression); {
				case d != nil:
					ds := c.expr(w, n.UnaryExpression, nil, exprDefault)
					w.w("\n%s -= %d", ds, sz)
					b.w("%s", ds)
				default:
					c.err(errorf("TODO")) // -
				}
			default:
				c.err(errorf("TODO %v", mode)) // -
			}
		default:
			switch mode {
			case exprVoid:
				b.w("%s--", c.expr(w, n.UnaryExpression, nil, exprDefault))
			case exprDefault:
				switch d := c.declaratorOf(n.UnaryExpression); {
				case d != nil:
					ds := c.expr(w, n.UnaryExpression, nil, exprDefault)
					w.w("\n%s--", ds)
					b.w("%s", ds)
				default:
					v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
					v2 := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
					w.w("\n%s := %s", v2, c.expr(w, n.UnaryExpression, n.UnaryExpression.Type().Pointer(), exprUintpr))
					w.w("\n(*(*%s)(unsafe.Pointer(%s)))--", c.typ(n.UnaryExpression.Type()), v2)
					w.w("\n%s := (*(*%s)(unsafe.Pointer(%s)))", v, c.typ(n.UnaryExpression.Type()), v2)
					b.w("%s", v)
				}
			default:
				c.err(errorf("TODO %v", mode)) // -
			}
		}
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		rt, rmode = n.Type(), exprUintpr
		b.w("%s", c.expr(w, n.CastExpression, nil, exprUintpr))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		switch n.Type().Undecay().(type) {
		case *cc.FunctionType:
			if mode == exprCall {
				rt, rmode = n.Type(), mode
				b.w("%s", c.expr(w, n.CastExpression, nil, mode))
				break out
			}
		}

		switch mode {
		case exprDefault, exprLvalue:
			rt, rmode = n.Type(), exprDefault
			b.w("(*(*%s)(unsafe.%sPointer(%s)))", c.typ(n.CastExpression.Type().(*cc.PointerType).Elem()), tag(preserve), c.expr(w, n.CastExpression, nil, exprDefault))
		case exprVoid:
			rt, rmode = n.Type(), mode
			b.w("_ = (*(*%s)(unsafe.%sPointer(%s)))", c.typ(n.CastExpression.Type().(*cc.PointerType).Elem()), tag(preserve), c.expr(w, n.CastExpression, nil, exprDefault))
		case exprUintpr:
			rt, rmode = n.CastExpression.Type(), mode
			b.w("%s", c.expr(w, n.CastExpression, nil, exprDefault))
		default:
			c.err(errorf("TODO %v", mode))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		rt, rmode = n.Type(), exprDefault
		b.w("(+%s)", c.expr(w, n.CastExpression, nil, exprDefault))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		rt, rmode = n.Type(), exprDefault
		b.w("(-%s)", c.expr(w, n.CastExpression, nil, exprDefault))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		rt, rmode = n.Type(), exprDefault
		b.w("(^%s)", c.expr(w, n.CastExpression, nil, exprDefault))
	case cc.UnaryExpressionNot: // '!' CastExpression
		rt, rmode = n.Type(), exprBool
		b.w("(!%s)", c.expr(w, n.CastExpression, nil, exprBool))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		rt, rmode = n.Type(), exprDefault
		switch n.UnaryExpression.Type().Undecay().Kind() {
		case cc.Array:
			b.w("%s(unsafe.Sizeof(%s))", c.typ(n.Type()), c.expr(w, n.UnaryExpression, nil, exprIndex))
		default:
			b.w("%s(unsafe.Sizeof(%s))", c.typ(n.Type()), c.expr(w, n.UnaryExpression, nil, exprDefault))
		}
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		rt, rmode = n.Type(), exprDefault
		switch tn := n.TypeName.Type(); {
		case cc.IsScalarType(tn):
			b.w("%s(unsafe.Sizeof(%s(0)))", c.typ(n.Type()), c.typ(tn))
		default:
			b.w("%s(unsafe.Sizeof(%s{}))", c.typ(n.Type()), c.typ(tn.Undecay()))
		}
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
	return &b, rt, rmode
}

func (c *ctx) postfixExpression(w writer, n *cc.PostfixExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.PostfixExpressionIndex: // PostfixExpression '[' ExpressionList ']'
		switch x := n.PostfixExpression.Type().(type) {
		case *cc.PointerType:
			switch mode {
			case exprSelect, exprLvalue, exprDefault, exprIndex:
				switch y := x.Undecay().(type) {
				case *cc.ArrayType:
					rt, rmode = n.Type(), mode
					b.w("%s[%s]", c.expr(w, n.PostfixExpression, nil, exprIndex), c.expr(w, n.ExpressionList, nil, exprDefault))
				case *cc.PointerType:
					rt, rmode = n.Type(), mode
					var s string
					if v := y.Elem().Size(); v != 1 {
						s = fmt.Sprintf("*%v", v)
					}
					b.w("(*(*%s)(unsafe.%sAdd(unsafe.Pointer(%s), (%s)%s)))", c.typ(y.Elem()), tag(preserve), c.expr(w, n.PostfixExpression, nil, exprDefault), c.expr(w, n.ExpressionList, nil, exprDefault), s)
				default:
					c.err(errorf("TODO %T", x))
				}
			case exprUintpr:
				rt, rmode = n.Type(), mode
				s := ""
				if sz := x.Elem().Size(); sz != 1 {
					s = fmt.Sprintf("*%v", sz)
				}
				b.w("uintptr(unsafe.%sAdd(unsafe.%[1]sPointer(&%s), (%s%s)))", tag(preserve), c.pin(n, c.expr(w, n.PostfixExpression, nil, exprLvalue)), c.expr(w, n.ExpressionList, nil, exprDefault), s)
			default:
				c.err(errorf("TODO %v", mode))
			}
		case *cc.PredefinedType:
			switch {
			case x.VectorSize() < 0:
				c.err(errorf("TODO %v", x))
			default:
				c.err(errorf("TODO %v", x))
			}
		default:
			c.err(errorf("TODO %T", x))
		}
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		return c.postfixExpressionCall(w, n)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		switch mode {
		case exprLvalue, exprDefault, exprSelect:
			rt, rmode = n.Type(), mode
			b.w("(%s.", c.expr(w, n.PostfixExpression, nil, exprSelect))
			switch f := n.Field(); {
			case f.Parent() != nil:
				c.err(errorf("TODO %v", n.Case))
			default:
				b.w("%s%s)", tag(field), f.Name())
			}
		case exprUintpr:
			rt, rmode = n.Type(), mode
			b.w("uintptr(unsafe.Pointer(&%s.", c.pin(n, c.expr(w, n.PostfixExpression, nil, exprLvalue)))
			switch f := n.Field(); {
			case f.Parent() != nil:
				c.err(errorf("TODO %v", n.Case))
			default:
				b.w("%s%s))", tag(field), f.Name())
			}
		case exprIndex:
			switch x := n.Type().Undecay().(type) {
			case *cc.ArrayType:
				b.w("%s", c.expr(w, n, nil, exprDefault))
				rt, rmode = n.Type(), mode
			default:
				c.err(errorf("TODO %T", x))
			}
		default:
			c.err(errorf("TODO %v", mode))
		}
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		rt, rmode = n.Type(), exprDefault
		b.w("((*%s)(unsafe.%sPointer(%s)).", c.typ(n.PostfixExpression.Type().(*cc.PointerType).Elem()), tag(preserve), c.expr(w, n.PostfixExpression, nil, exprDefault))
		switch f := n.Field(); {
		case f.Parent() != nil:
			c.err(errorf("TODO %v", n.Case))
		default:
			b.w("%s%s)", tag(field), f.Name())
		}
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		rt, rmode = n.Type(), mode
		switch pe := n.PostfixExpression.Type(); {
		case pe.Kind() == cc.Ptr && pe.(*cc.PointerType).Elem().Size() != 1:
			sz := pe.(*cc.PointerType).Elem().Size()
			switch mode {
			case exprVoid:
				b.w("%s += %d", c.expr(w, n.PostfixExpression, nil, exprDefault), sz)
			case exprDefault:
				v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
				switch d := c.declaratorOf(n.PostfixExpression); {
				case d != nil:
					ds := c.expr(w, n.PostfixExpression, nil, exprDefault)
					w.w("\n%s := %s", v, ds)
					w.w("\n%s += %d", ds, sz)
					b.w("%s", v)
				default:
					v2 := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
					w.w("\n%s := %s", v2, c.expr(w, n.PostfixExpression, n.PostfixExpression.Type().Pointer(), exprUintpr))
					w.w("\n%s := (*(*%s)(unsafe.Pointer(%s)))", v, c.typ(n.PostfixExpression.Type()), v2)
					w.w("\n(*(*%s)(unsafe.Pointer(%s))) += %d", c.typ(n.PostfixExpression.Type()), v2, sz)
					b.w("%s", v)
				}
			default:
				c.err(errorf("TODO %v", mode)) // -
			}
		default:
			switch mode {
			case exprVoid:
				b.w("%s++", c.expr(w, n.PostfixExpression, nil, exprDefault))
			case exprDefault:
				v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
				switch d := c.declaratorOf(n.PostfixExpression); {
				case d != nil:
					ds := c.expr(w, n.PostfixExpression, nil, exprDefault)
					w.w("\n%s := %s", v, ds)
					w.w("\n%s++", ds)
					b.w("%s", v)
				default:
					v2 := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
					w.w("\n%s := %s", v2, c.expr(w, n.PostfixExpression, n.PostfixExpression.Type().Pointer(), exprUintpr))
					w.w("\n%s := (*(*%s)(unsafe.Pointer(%s)))", v, c.typ(n.PostfixExpression.Type()), v2)
					w.w("\n(*(*%s)(unsafe.Pointer(%s)))++", c.typ(n.PostfixExpression.Type()), v2)
					b.w("%s", v)
				}
			default:
				c.err(errorf("TODO %v", mode)) // -
			}
		}
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		switch pe := n.PostfixExpression.Type(); {
		case pe.Kind() == cc.Ptr && pe.(*cc.PointerType).Elem().Size() != 1:
			sz := pe.(*cc.PointerType).Elem().Size()
			switch mode {
			case exprVoid:
				b.w("%s = %d", c.expr(w, n.PostfixExpression, nil, exprDefault), sz)
			case exprDefault:
				v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
				switch d := c.declaratorOf(n.PostfixExpression); {
				case d != nil:
					ds := c.expr(w, n.PostfixExpression, nil, exprDefault)
					w.w("\n%s := %s", v, ds)
					w.w("\n%s -= %d", ds, sz)
					b.w("%s", v)
				default:
					v2 := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
					w.w("\n%s := %s", v2, c.expr(w, n.PostfixExpression, n.PostfixExpression.Type().Pointer(), exprUintpr))
					w.w("\n%s := (*(*%s)(unsafe.Pointer(%s)))", v, c.typ(n.PostfixExpression.Type()), v2)
					w.w("\n(*(*%s)(unsafe.Pointer(%s))) -= %d", c.typ(n.PostfixExpression.Type()), v2, sz)
					b.w("%s", v)
				}
			default:
				c.err(errorf("TODO %v", mode)) // -
			}
		default:
			switch mode {
			case exprVoid:
				rt, rmode = n.Type(), exprVoid
				b.w("%s--", c.expr(w, n.PostfixExpression, nil, exprDefault))
			case exprDefault:
				rt, rmode = n.Type(), exprDefault
				v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
				switch d := c.declaratorOf(n.PostfixExpression); {
				case d != nil:
					ds := c.expr(w, n.PostfixExpression, nil, exprDefault)
					w.w("\n%s := %s", v, ds)
					w.w("\n%s--", ds)
					b.w("%s", v)
				default:
					v2 := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
					w.w("\n%s := %s", v2, c.expr(w, n.PostfixExpression, n.PostfixExpression.Type().Pointer(), exprUintpr))
					w.w("\n%s := (*(*%s)(unsafe.Pointer(%s)))", v, c.typ(n.PostfixExpression.Type()), v2)
					w.w("\n(*(*%s)(unsafe.Pointer(%s)))--", c.typ(n.PostfixExpression.Type()), v2)
					b.w("%s", v)
				}
			default:
				c.err(errorf("TODO")) // -
			}
		}
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) declaratorOf(n cc.ExpressionNode) *cc.Declarator {
	for n != nil {
		switch x := n.(type) {
		case *cc.PrimaryExpression:
			switch x.Case {
			case cc.PrimaryExpressionIdent: // IDENTIFIER
				switch y := x.ResolvedTo().(type) {
				case *cc.Declarator:
					return y
				case *cc.Parameter:
					return y.Declarator
				default:
					c.err(errorf("TODO %T", y))
					return nil
				}
			case cc.PrimaryExpressionExpr: // '(' ExpressionList ')'
				n = x.ExpressionList
			default:
				return nil
			}
		case *cc.PostfixExpression:
			switch x.Case {
			case cc.PostfixExpressionPrimary: // PrimaryExpression
				n = x.PrimaryExpression
			default:
				return nil
			}
		case *cc.ExpressionList:
			if x == nil {
				return nil
			}

			for l := x; l != nil; l = l.ExpressionList {
				n = l.AssignmentExpression
			}
		case *cc.CastExpression:
			switch x.Case {
			case cc.CastExpressionUnary: // UnaryExpression
				n = x.UnaryExpression
			default:
				return nil
			}
		case *cc.UnaryExpression:
			switch x.Case {
			case
				cc.UnaryExpressionInc,
				cc.UnaryExpressionDec,
				cc.UnaryExpressionPostfix: // PostfixExpression

				n = x.PostfixExpression
			default:
				return nil
			}
		default:
			panic(todo("%T", n))
		}
	}
	return nil
}

func (c *ctx) postfixExpressionCall(w writer, n *cc.PostfixExpression) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	pt, ok := n.PostfixExpression.Type().(*cc.PointerType)
	if !ok {
		c.err(errorf("TODO %T", n.PostfixExpression.Type()))
		return
	}

	ft, ok := pt.Elem().(*cc.FunctionType)
	if !ok {
		c.err(errorf("TODO %T", pt.Elem()))
		return
	}

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
	var xargs []*buf
	for i, v := range args {
		mode := exprDefault
		var t cc.Type
		switch {
		case i < len(params):
			t = params[i].Type()
		default:
			switch t = v.Type(); {
			case cc.IsIntegerType(t):
				t = cc.IntegerPromotion(t)
			case t.Kind() == cc.Float:
				t = c.ast.Double
			}
		}
		switch v.Type().Undecay().Kind() {
		case cc.Array:
			if _, ok := v.Value().(cc.StringValue); !ok {
				mode = exprUintpr
			}
		case cc.Function:
			mode = exprUintpr
		}
		xargs = append(xargs, c.expr(w, v, t, mode))
	}
	b.w("%s(%stls", c.expr(w, n.PostfixExpression, nil, exprCall), tag(ccgo))
	switch {
	case ft.IsVariadic():
		for _, v := range xargs[:ft.MinArgs()] {
			b.w(", %s", v)
		}
		switch {
		case len(xargs) == ft.MinArgs():
			b.w(", 0")
		default:
			b.w(", %s%sVaList(%s", c.task.tlsQualifier, tag(preserve), bpOff(c.f.tlsAllocs+8))
			if n := len(xargs[ft.MinArgs():]); n > c.f.maxValist {
				c.f.maxValist = n
			}
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
	rt, rmode = ft.Result(), exprDefault
	if rt.Kind() == cc.Void {
		rmode = exprVoid
	}
	return &b, rt, rmode
}

func (c *ctx) assignmentExpression(w writer, n *cc.AssignmentExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		switch mode {
		case exprDefault:
			rt, rmode = n.Type(), exprDefault
			v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
			w.w("\n%s := %s", v, c.expr(w, n.AssignmentExpression, n.UnaryExpression.Type(), exprDefault))
			w.w("\n%s = %s", c.expr(w, n.UnaryExpression, nil, exprDefault), v)
			b.w("%s", v)
		case exprVoid:
			b.w("%s = %s", c.expr(w, n.UnaryExpression, nil, exprLvalue), c.expr(w, n.AssignmentExpression, n.UnaryExpression.Type(), exprDefault))
			rt, rmode = n.Type(), exprVoid
		default:
			c.err(errorf("TODO %v", mode))
		}
	case cc.AssignmentExpressionMul, // UnaryExpression "*=" AssignmentExpression
		cc.AssignmentExpressionDiv, // UnaryExpression "/=" AssignmentExpression
		cc.AssignmentExpressionMod, // UnaryExpression "%=" AssignmentExpression
		cc.AssignmentExpressionAdd, // UnaryExpression "+=" AssignmentExpression
		cc.AssignmentExpressionSub, // UnaryExpression "-=" AssignmentExpression
		cc.AssignmentExpressionLsh, // UnaryExpression "<<=" AssignmentExpression
		cc.AssignmentExpressionRsh, // UnaryExpression ">>=" AssignmentExpression
		cc.AssignmentExpressionAnd, // UnaryExpression "&=" AssignmentExpression
		cc.AssignmentExpressionXor, // UnaryExpression "^=" AssignmentExpression
		cc.AssignmentExpressionOr:  // UnaryExpression "|=" AssignmentExpression

		rt, rmode = n.Type(), mode
		op := n.Token.SrcStr()
		op = op[:len(op)-1]
		x, y := n.UnaryExpression.Type(), n.AssignmentExpression.Type()
		var mul, div string
		switch n.Case {
		case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
			switch {
			case x.Kind() == cc.Ptr && cc.IsIntegerType(y):
				if sz := x.(*cc.PointerType).Elem().Size(); sz != 1 {
					mul = fmt.Sprintf("*%d", sz)
				}
			case cc.IsIntegerType(x) && y.Kind() == cc.Ptr:
				c.err(errorf("TODO")) // -
			}
		case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
			switch {
			case x.Kind() == cc.Ptr && cc.IsIntegerType(y):
				if sz := x.(*cc.PointerType).Elem().Size(); sz != 1 {
					mul = fmt.Sprintf("*%d", sz)
				}
			case x.Kind() == cc.Ptr && y.Kind() == cc.Ptr:
				if sz := x.(*cc.PointerType).Elem().Size(); sz != 1 {
					div = fmt.Sprintf("/%d", sz)
				}
			}
		}
		ct := c.usualArithmeticConversions(x, y)
		switch mode {
		case exprVoid:
			switch d := c.declaratorOf(n.UnaryExpression); {
			case d != nil:
				b.w("\n%s = ", c.expr(w, n.UnaryExpression, nil, exprDefault))
				var b2 buf
				b2.w("(%s %s (%s%s))", c.expr(w, n.UnaryExpression, ct, exprDefault), op, c.expr(w, n.AssignmentExpression, ct, exprDefault), mul)
				b.w("%s", c.convert(n, w, &b2, ct, t, mode, mode))
			default:
				p := fmt.Sprintf("%sp%d", tag(ccgo), c.id())
				ut := n.UnaryExpression.Type()
				w.w("\n%s := %s", p, c.expr(w, n.UnaryExpression, ut.Pointer(), exprUintpr))
				var b2 buf
				p2 := newBufFromtring(fmt.Sprintf("(*(*%s)(unsafe.Pointer(%s)))", c.typ(ut), p))
				b2.w("((%s %s (%s%s))%s)", c.convert(n, w, p2, n.UnaryExpression.Type(), ct, exprDefault, exprDefault), op, c.expr(w, n.AssignmentExpression, ct, exprDefault), mul, div)
				b.w("*(*%s)(unsafe.Pointer(%s)) = %s", c.typ(ut), p, c.convert(n, w, &b2, ct, t, mode, mode))
			}
		case exprDefault:
			switch d := c.declaratorOf(n.UnaryExpression); {
			case d != nil:
				w.w("\n%s = ", c.expr(w, n.UnaryExpression, nil, exprDefault))
				var b2 buf
				b2.w("(%s %s (%s%s))", c.expr(w, n.UnaryExpression, ct, exprDefault), op, c.expr(w, n.AssignmentExpression, ct, exprDefault), mul)
				w.w("%s", c.convert(n, w, &b2, ct, t, mode, mode))
				b.w("%s", c.expr(w, n.UnaryExpression, nil, exprDefault))
			default:
				c.err(errorf("TODO"))
			}
		default:
			c.err(errorf("TODO %v", mode))
		}
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) expressionList(w writer, n *cc.ExpressionList, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	for ; n != nil; n = n.ExpressionList {
		switch {
		case n.ExpressionList == nil:
			return c.expr0(w, n.AssignmentExpression, t, mode)
		default:
			c.expr(w, n.AssignmentExpression, nil, exprVoid)
		}
	}
	c.err(errorf("TODO internal error", n))
	return r, rt, rmode
}

func (c *ctx) primaryExpression(w writer, n *cc.PrimaryExpression, t cc.Type, mode mode) (r *buf, rt cc.Type, rmode mode) {
	var b buf
out:
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		rt, rmode = n.Type(), mode
		switch x := n.ResolvedTo().(type) {
		case *cc.Declarator:
			b.n = x
			var info *declInfo
			if c.f != nil {
				info = c.f.declInfos.info(x)
			}
			switch {
			case info != nil && info.escapes():
				switch mode {
				case exprLvalue, exprSelect, exprIndex:
					b.w("(*(*%s)(unsafe.%sPointer(%s)))", c.typ(x.Type()), tag(preserve), bpOff(info.bpOff))
				case exprUintpr:
					b.w("%s", bpOff(info.bpOff))
				case exprDefault:
					switch n.Type().Undecay().(type) {
					case *cc.ArrayType:
						b.w("%s", bpOff(info.bpOff))
					default:
						b.w("(*(*%s)(unsafe.%sPointer(%s)))", c.typ(x.Type()), tag(preserve), bpOff(info.bpOff))
					}
				default:
					c.err(errorf("TODO %v %v:", mode, n.Position()))
				}
			default:
				switch mode {
				case exprDefault:
					switch x.Type().Kind() {
					case cc.Array:
						p := &buf{n: x}
						p.w("%s%s", c.declaratorTag(x), x.Name())
						b.w("uintptr(unsafe.Pointer(&%s))", c.pin(n, p))
					case cc.Function:
						v := fmt.Sprintf("%sf%d", tag(ccgo), c.id())
						switch {
						case c.f == nil:
							w.w("\nvar %s = %s%s", v, c.declaratorTag(x), x.Name())
						default:
							w.w("\n%s := %s%s", v, c.declaratorTag(x), x.Name())
						}
						b.w("(*(*uintptr)(unsafe.Pointer(&%s)))", v)
					default:
						b.w("%s%s", c.declaratorTag(x), x.Name())
					}
				case exprLvalue, exprSelect:
					b.w("%s%s", c.declaratorTag(x), x.Name())
				case exprCall:
					switch y := x.Type().(type) {
					case *cc.FunctionType:
						b.w("%s%s", c.declaratorTag(x), x.Name())
					case *cc.PointerType:
						switch z := y.Elem().(type) {
						case *cc.FunctionType:
							rmode = exprUintpr
							b.w("%s%s", c.declaratorTag(x), x.Name())
						default:
							c.err(errorf("TODO %T", z))
						}
					default:
						c.err(errorf("TODO %T", y))
					}
				case exprIndex:
					switch x.Type().Kind() {
					case cc.Array:
						b.w("%s%s", c.declaratorTag(x), x.Name())
					default:
						c.err(errorf("TODO %v", mode))
					}
				case exprUintpr:
					switch {
					case x.Type().Kind() == cc.Function:
						v := fmt.Sprintf("%sf%d", tag(ccgo), c.id())
						switch {
						case c.f == nil:
							w.w("\nvar %s = %s%s", v, c.declaratorTag(x), x.Name())
						default:
							w.w("\n%s := %s%s", v, c.declaratorTag(x), x.Name())
						}
						b.w("(*(*uintptr)(unsafe.Pointer(&%s)))", v) // Free pass from .pin
					default:
						p := &buf{n: x}
						p.w("%s%s", c.declaratorTag(x), x.Name())
						b.w("uintptr(unsafe.%sPointer(&%s))", tag(preserve), c.pin(n, p))
					}
				default:
					c.err(errorf("TODO %v", mode))
				}
			}
		case *cc.Enumerator:
			switch {
			case x.ResolvedIn().Parent == nil:
				rt, rmode = n.Type(), exprDefault
				b.w("%s%s", tag(enumConst), x.Token.Src())
			default:
				rt, rmode = n.Type(), exprDefault
				b.w("%v", n.Value())
			}
		case nil:
			switch mode {
			case exprCall:
				b.w("%s%s", tag(external), n.Token.Src())
				break out
			default:
				c.err(errorf("TODO %v: %v", n.Position(), mode))
				break out
			}
		default:
			c.err(errorf("TODO %T", x))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		rt, rmode = n.Type(), exprUntyped
		b.w("%d", n.Value())
	case cc.PrimaryExpressionFloat: // FLOATCONST
		rt, rmode = n.Type(), exprUntyped
		b.w("%v", n.Value())
	case cc.PrimaryExpressionChar: // CHARCONST
		rt, rmode = n.Type(), exprUntyped
		b.w("%d", n.Value())
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		c.err(errorf("TODO %v", n.Case))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		rt, rmode = n.Type(), exprUntyped
		b.w("%s", n.Value())
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		c.err(errorf("TODO %v", n.Case))
	case cc.PrimaryExpressionExpr: // '(' ExpressionList ')'
		return c.expr0(w, n.ExpressionList, nil, mode)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		c.err(errorf("TODO %v", n.Case))
	case cc.PrimaryExpressionGeneric: // GenericSelection
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return &b, rt, rmode
}

func (c *ctx) stringCharConst(b byte) string {
	switch {
	case b >= ' ' && b < 0x7f:
		return strconv.QuoteRuneToASCII(rune(b))
	case c.ast.ABI.SignedChar:
		return fmt.Sprint(int8(b))
	default:
		return fmt.Sprint(b)
	}
}
