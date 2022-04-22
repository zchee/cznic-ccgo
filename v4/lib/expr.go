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
	exprBool         // C arithmetic type, Go bool
	exprCall         // C func pointer, Go function value
	exprDefault      //
	exprIndex        // C pointer, Go array
	exprLvalue       //
	exprPointer      // C pointer, Go pointer
	exprSelect       // C struct, Go struct
	exprUintpr       // C pointer, Go uintptr
	exprUntyped      // C primary expr literal, Go typed literal
	exprVoid         // C void, no Go equivalent
)

func (c *ctx) expr(w writer, n cc.ExpressionNode, to cc.Type, toMode mode) []byte {
	if toMode == 0 {
		c.err(errorf("internal error"))
		return nil
	}

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

	if to == nil {
		to = n.Type()
	}
	r, from, fromMode := c.expr0(w, n, to, toMode)
	if assert && (from == nil || fromMode == 0) {
		c.err(errorf("TODO %T %v %v", n, from, fromMode))
	}

	return c.convert(n, w, r, from, to, fromMode, toMode)
}

func (c *ctx) convert(n cc.Node, w writer, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
	// defer func() { trc("%v: from %v: %v, to %v: %v %q -> %q", c.pos(n), from, fromMode, to, toMode, s, r) }()
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
		if fromMode == toMode {
			return s
		}

		return c.convertMode(n, w, s, from, to, fromMode, toMode)
	}

	if fromMode == toMode {
		return c.convertType(n, s, from, to, fromMode, toMode)
	}

	// 	if from == to {
	// 		if fromMode == expr && toMode == exprVoid ||
	// 			fromMode == toMode {
	// 			return s
	// 		}
	// 	}
	//
	if from != nil && from.Kind() == cc.Ptr {
		return c.convertFromPointer(n, s, from.(*cc.PointerType), to, fromMode, toMode)
	}

	// 	if to.Kind() == cc.Ptr {
	// 		return c.convertToPointer(n, s, from, to.(*cc.PointerType), fromMode, toMode)
	// 	}
	//
	// 	if toMode == exprBool {
	// 		return c.convertToBool(n, s, from, to, fromMode, toMode)
	// 	}
	//
	// 	if fromMode == exprBool {
	// 		return c.convertFromBool(n, s, from, to, fromMode, toMode)
	// 	}
	//
	// 	if cc.IsIntegerType(from) {
	// 		return c.convertFromInteger(n, s, from, to, fromMode, toMode)
	// 	}
	//
	// 	if cc.IsArithmeticType(from) {
	// 		return c.convertFromArithmetic(n, s, from, to, fromMode, toMode)
	// 	}
	//
	// 	if from.Kind() == cc.Struct && fromMode == expr && to.Kind() == cc.Struct && toMode == exprSelect {
	// 		return s
	// 	}
	//
	c.err(errorf("TODO %q %s %s -> %s %s", s, from, fromMode, to, toMode))
	return s //TODO
}

func (c *ctx) convertUntyped(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
	// defer func() { trc("%v: from %v: %v, to %v: %v %q -> %q", c.pos(n), from, fromMode, to, toMode, s, r) }()
	if toMode != exprDefault {
		c.err(errorf("TODO"))
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
				for len(s) != 0 && s[len(s)-1] == 0 {
					s = s[:len(s)-1]
				}
				b.w("%s{", c.typ(to))
				for i := 0; i < len(s) && int64(i) < max; i++ {
					b.w("%s, ", c.stringCharConst(s[i]))
				}
				b.w("}")
				return b.bytes()
			case cc.Ptr:
				to := to.(*cc.PointerType)
				if c.isCharType(to.Elem()) || to.Elem().Kind() == cc.Void {
					b.w("%q", s)
					return b.bytes()
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
				v, err := strconv.ParseInt(string(s), 10, 64)
				if err != nil {
					c.err(errorf("internal error: %v", err))
					break
				}

				val = uint64(v)
			default:
				c.err(errorf("TODO"))
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
		default:
			c.err(errorf("TODO"))
		}
		return b.bytes()
	case cc.IsArithmeticType(from):
		switch {
		case cc.IsArithmeticType(to):
			b.w("%s(%s)", c.typ(to), s)
		default:
			c.err(errorf("TODO"))
		}
		return b.bytes()
	}

	c.err(errorf("TODO %q %s %s -> %s %s", s, from, fromMode, to, toMode))
	return s //TODO
}

// type unchanged
func (c *ctx) convertMode(n cc.Node, w writer, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
	// defer func() { trc("%v: from %v: %v, to %v: %v %q -> %q", c.pos(n), from, fromMode, to, toMode, b, r) }()
	var b buf
	switch fromMode {
	case exprDefault:
		switch toMode {
		case exprLvalue:
			if assert {
				s := string(s)
				if !c.isIdent(s) {
					break
				}

				switch symKind(s) {
				case automatic:
					// ok
				default:
					c.err(errorf("TODO %v", symKind(s)))
				}
			}
			return s
		case exprCall:
			if assert {
				s := string(s)
				if !c.isIdent(s) {
					break
				}

				switch symKind(s) {
				case external:
					// ok
				default:
					c.err(errorf("TODO %v", symKind(s)))
				}
			}
			return s
		case exprVoid:
			return s
		case exprBool:
			b.w("(%s != 0)", s)
			return b.bytes()
		}
	case exprUintpr:
		switch toMode {
		case exprDefault:
			return s
		case exprCall:
			v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
			ft := from.(*cc.PointerType).Elem().(*cc.FunctionType)
			w.w("\nvar %s func%s", v, c.signature(ft, false, false))
			w.w("\n*(*uintptr)(unsafe.%sPointer(&%s)) = %s", tag(preserve), v, s)
			return []byte(v)
		}
	case exprBool:
		switch toMode {
		case exprDefault:
			switch {
			case cc.IsIntegerType(to):
				b.w("%s%sBool%s(%s)", c.task.tlsQualifier, tag(preserve), c.typeSuffix(to), s)
				return b.bytes()
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
func (c *ctx) convertType(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
	// defer func() { trc("%v: from %v: %v, to %v: %v %q -> %q", c.pos(n), from, fromMode, to, toMode, s, r) }()
	var b buf
	if from.Kind() == cc.Ptr && to.Kind() == cc.Ptr {
		return s
	}

	if cc.IsArithmeticType(from) && cc.IsArithmeticType(to) {
		b.w("%s(%s)", c.typ(to), s)
		return b.bytes()
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

// func (c *ctx) convertFromArithmetic(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
// 	var b buf
// 	if cc.IsArithmeticType(to) {
// 		b.w("%s(%s)", c.typ(to), s)
// 		return b.bytes()
// 	}
//
// 	c.err(errorf("TODO %q %s %s, %s -> %s %s, %s", s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
// 	return s
// }
//
// func (c *ctx) convertFromInteger(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
// 	var b buf
// 	if cc.IsArithmeticType(to) {
// 		b.w("%s(%s)", c.typ(to), s)
// 		return b.bytes()
// 	}
//
// 	c.err(errorf("TODO %q %s %s, %s -> %s %s, %s", s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
// 	return s
// }
//
// func (c *ctx) convertFromBool(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
// 	var b buf
// 	if toMode == expr && to.Kind() == cc.Int && to.Size() == 4 {
// 		b.w("%sBool32(%s)", c.task.tlsQualifier, s)
// 		return b.bytes()
// 	}
//
// 	c.err(errorf("TODO %q %s %s, %s -> %s %s, %s", s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
// 	return s //TODO
// }
//
// func (c *ctx) convertToBool(n cc.Node, s []byte, from, to cc.Type, fromMode, toMode mode) (r []byte) {
// 	var b buf
// 	b.w("(%s != 0)", s)
// 	return b.bytes()
// }
//
// func (c *ctx) convertToPointer(n cc.Node, s []byte, from cc.Type, to *cc.PointerType, fromMode, toMode mode) (r []byte) {
// 	var b buf
// 	if from.Kind() == cc.Struct && fromMode == expr && to.Elem().Kind() == cc.Struct && toMode == exprUintpr {
// 		b.w("uintptr(unsafe.Pointer(&%s))", s)
// 		return b.bytes()
// 	}
//
// 	if cc.IsScalarType(from) && fromMode == expr && to.Elem().Kind() == from.Kind() && toMode == exprUintpr {
// 		b.w("uintptr(unsafe.Pointer(&%s))", s)
// 		return b.bytes()
// 	}
//
// 	if from.Kind() == cc.Function && fromMode == exprFunc && toMode == exprUintpr { //TODO get rid of the alloc
// 		b.w("(*(*uintptr)(unsafe.Pointer(&struct{f func%s}{%s})))", c.signature(from.(*cc.FunctionType), false, false), s)
// 		return b.bytes()
// 	}
//
// 	if cc.IsIntegerType(from) {
// 		b.w("%s(%s)", c.typ(to), s)
// 		return b.bytes()
// 	}
//
// 	c.err(errorf("TODO %q %s %s, %s -> %s %s, %s", s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
// 	return s //TODO
// }

func (c *ctx) convertFromPointer(n cc.Node, s []byte, from *cc.PointerType, to cc.Type, fromMode, toMode mode) (r []byte) {
	// 	var b buf
	if to.Kind() == cc.Ptr {
		if fromMode == exprUintpr && toMode == exprDefault {
			return s
		}

		// 		if fromMode == expr && toMode == expr {
		// 			return s
		// 		}
		//
		// 		if fromMode == exprUintpr && toMode == exprUintpr {
		// 			return s
		// 		}
		//
		// 		if fromMode == expr && toMode == exprUintpr {
		// 			b.w("uintptr(unsafe.Pointer(&%s))", s)
		// 			return b.bytes()
		// 		}
		//
		// 		if fromMode == expr && toMode == exprBool {
		// 			b.w("(%s != 0)", s)
		// 			return b.bytes()
		// 		}
	}

	// 	if to.Kind() == cc.Array {
	// 		if ufrom := from.Undecay(); ufrom.Kind() == cc.Array {
	// 			if fromMode == expr && toMode == exprIndex {
	// 				return s
	// 			}
	// 		}
	// 	}
	//
	// 	if from.Elem().Kind() == to.Kind() && fromMode == exprUintpr && toMode == expr {
	// 		b.w("(*(*%s)(unsafe.Pointer(%s)))", c.typ(to), s)
	// 		return b.bytes()
	// 	}
	//
	// 	if to.Kind() == cc.Ptr {
	// 		tpt := to.(*cc.PointerType)
	// 		if from.Elem().Kind() == tpt.Elem().Kind() && fromMode == expr && toMode == exprPointer {
	// 			b.w("(*%s)(unsafe.Pointer(%s))", c.typ(tpt.Elem()), s)
	// 			return b.bytes()
	// 		}
	// 	}
	//
	// 	if from.Elem().Kind() == cc.Array && fromMode == exprUintpr && to.Kind() == cc.Array && toMode == exprIndex {
	// 		b.w("(*%s)(unsafe.Pointer(%s))", c.typ(to), s)
	// 		return b.bytes()
	// 	}
	//
	// 	if fromMode == exprUintpr && to.Kind() == cc.Ptr && toMode == expr {
	// 		return s
	// 	}
	//
	// 	if fromMode == exprUintpr && toMode == exprSelect {
	// 		b.w("(*%s)(unsafe.Pointer(%s))", c.typ(to), s)
	// 		return b.bytes()
	// 	}
	//
	// 	if (fromMode == exprUintpr || fromMode == expr) && toMode == exprFunc {
	// 		b.w("(*(*func%s)(unsafe.Pointer(&(%s))))", c.signature(from.Elem().(*cc.FunctionType), false, false), s)
	// 		return b.bytes()
	// 	}
	//
	// 	if cc.IsIntegerType(to) {
	// 		b.w("%s(%s)", c.typ(to), s)
	// 		return b.bytes()
	// 	}

	c.err(errorf("TODO %q %s %s, %s -> %s %s, %s", s, from, from.Kind(), fromMode, to, to.Kind(), toMode))
	return s //TODO
}

func (c *ctx) expr0(w writer, n cc.ExpressionNode, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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

func (c *ctx) andExpression(w writer, n *cc.AndExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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
	return b.bytes(), rt, rmode
}

func (c *ctx) exclusiveOrExpression(w writer, n *cc.ExclusiveOrExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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
	return b.bytes(), rt, rmode
}

func (c *ctx) inclusiveOrExpression(w writer, n *cc.InclusiveOrExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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
	return b.bytes(), rt, rmode
}

func (c *ctx) shiftExpression(w writer, n *cc.ShiftExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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
	return b.bytes(), rt, rmode
}

func (c *ctx) logicalAndExpression(w writer, n *cc.LogicalAndExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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
	return b.bytes(), rt, rmode
}

func (c *ctx) logicalOrExpression(w writer, n *cc.LogicalOrExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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
	return b.bytes(), rt, rmode
}

func (c *ctx) conditionalExpression(w writer, n *cc.ConditionalExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
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
	return b.bytes(), rt, rmode
}

func (c *ctx) castExpression(w writer, n *cc.CastExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		c.err(errorf("TODO %v", n.Case))
		// return c.expr0(w, n.UnaryExpression, t, mode)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		rt, rmode = n.Type(), mode
		b.w("%s", c.expr(w, n.CastExpression, n.Type(), mode))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) multiplicativeExpression(w writer, n *cc.MultiplicativeExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	rt, rmode = n.Type(), mode
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
	return b.bytes(), rt, rmode
}

func (c *ctx) additiveExpression(w writer, n *cc.AdditiveExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	rt, rmode = n.Type(), mode
	var b buf
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		switch x, y := n.AdditiveExpression.Type(), n.MultiplicativeExpression.Type(); {
		case cc.IsArithmeticType(x) && cc.IsArithmeticType(y):
			b.w("(%s + %s)", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault))
		default:
			c.err(errorf("TODO %v + %v", x, y))
		}
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		switch x, y := n.AdditiveExpression.Type(), n.MultiplicativeExpression.Type(); {
		case cc.IsArithmeticType(x) && cc.IsArithmeticType(y):
			b.w("(%s - %s)", c.expr(w, n.AdditiveExpression, n.Type(), exprDefault), c.expr(w, n.MultiplicativeExpression, n.Type(), exprDefault))
		// case x.Kind() == cc.Ptr && y.Kind() == cc.Ptr:
		// 	b.w("(%s - %s)", c.expr(w, n.AdditiveExpression, n.Type(), expr), c.expr(w, n.MultiplicativeExpression, n.Type(), expr))
		// 	if v := x.(*cc.PointerType).Elem().Size(); v > 1 {
		// 		b.w("/%d", v)
		// 	}
		// case x.Kind() == cc.Ptr && cc.IsIntegerType(y):
		// 	b.w("(%s - (%s", c.expr(w, n.AdditiveExpression, n.Type(), expr), c.expr(w, n.MultiplicativeExpression, n.Type(), expr))
		// 	if v := x.(*cc.PointerType).Elem().Size(); v > 1 {
		// 		b.w("*%d)", v)
		// 	}
		// 	b.w(")")
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
	return b.bytes(), rt, rmode
}

func (c *ctx) relationExpression(w writer, n *cc.RelationalExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	if n.Case == cc.RelationalExpressionShift { // ShiftExpression
		c.err(errorf("TODO %v", n.Case))
		return b.bytes(), nil, exprBool
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
	return b.bytes(), rt, rmode
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

func (c *ctx) unaryExpression(w writer, n *cc.UnaryExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
out:
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		c.err(errorf("TODO %v", n.Case))
		// switch ue := n.UnaryExpression.Type(); {
		// case ue.Kind() == cc.Ptr && ue.(*cc.PointerType).Elem().Size() != 1:
		// 	sz := ue.(*cc.PointerType).Elem().Size()
		// 	switch mode {
		// 	case exprVoid:
		// 		b.w("%s += %d", c.expr(w, n.UnaryExpression, nil, expr), sz)
		// 	default:
		// 		c.err(errorf("TODO"))
		// 	}
		// default:
		// 	switch mode {
		// 	case exprVoid:
		// 		b.w("%s++", c.expr(w, n.UnaryExpression, nil, expr))
		// 		rmode = exprVoid
		// 	default:
		// 		c.err(errorf("TODO"))
		// 	}
		// }
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		c.err(errorf("TODO %v", n.Case))
		// switch ue := n.UnaryExpression.Type(); {
		// case ue.Kind() == cc.Ptr && ue.(*cc.PointerType).Elem().Size() != 1:
		// 	switch mode {
		// 	case exprVoid:
		// 		c.err(errorf("TODO"))
		// 	default:
		// 		c.err(errorf("TODO"))
		// 	}
		// default:
		// 	switch mode {
		// 	case exprVoid:
		// 		b.w("%s--", c.expr(w, n.UnaryExpression, nil, expr))
		// 		rmode = exprVoid
		// 	default:
		// 		v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
		// 		switch d := c.declaratorOf(n.UnaryExpression); {
		// 		case d != nil:
		// 			ds := c.expr(w, n.UnaryExpression, nil, expr)
		// 			w.w("\n%s--", ds)
		// 			w.w("\n%s := %s", v, ds)
		// 			b.w("%s", v)
		// 		default:
		// 			//TODO v2 := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
		// 			//TODO w.w("\n%s := %s", v2, c.expr(w, n.UnaryExpression, nil, exprUintpr))
		// 			//TODO w.w("\n(*(*%s)(unsafe.Pointer(%s)))--", c.typ(n.UnaryExpression.Type()), v2)
		// 			//TODO w.w("\n%s := (*(*%s)(unsafe.Pointer(%s)))", v, c.typ(n.UnaryExpression.Type()), v2)
		// 			//TODO b.w("%s", v)
		// 			w.w("panic(`TODO`)")
		// 			b.w("0")
		// 		}
		// 	}
		// }
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

		rt, rmode = n.Type(), exprDefault
		b.w("(*%s)", c.expr(w, n.CastExpression, nil, exprPointer))
	case cc.UnaryExpressionPlus: // '+' CastExpression
		rt, rmode = n.Type(), exprDefault
		b.w("(+%s)", c.expr(w, n.CastExpression, nil, exprDefault))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		rt, rmode = n.Type(), exprDefault
		b.w("(-%s)", c.expr(w, n.CastExpression, nil, exprDefault))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		c.err(errorf("TODO %v", n.Case))
		// b.w("(^%s)", c.expr(w, n.CastExpression, nil, expr))
	case cc.UnaryExpressionNot: // '!' CastExpression
		rt, rmode = n.Type(), exprBool
		b.w("(!%s)", c.expr(w, n.CastExpression, nil, exprBool))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		rt, rmode = n.Type(), exprDefault
		b.w("unsafe.Sizeof(%s)", c.expr(w, n.UnaryExpression, nil, exprDefault))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		rt, rmode = n.Type(), exprDefault
		switch x := n.TypeName.Type().(type) {
		case *cc.PredefinedType:
			switch x.Kind() {
			case cc.Void:
				c.err(errorf("TODO %v", x.Kind()))
			default:
				b.w("unsafe.Sizeof(%s(0))", c.typ(x))
			}
		default:
			c.err(errorf("TODO %T", x))
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
			// var a1 buf
			// b1 := c.expr(&a1, n.ExpressionList, nil, expr)
			// switch {
			// case a1.len() != 0:
			// 	w.w("\n%s", a1.bytes())
			// 	fallthrough
			// default:
			// 	switch x := n.PostfixExpression.Type().Undecay().(type) {
			// 	case *cc.ArrayType:
			// 		switch mode {
			// 		case exprUintpr:
			// 			rmode = exprUintpr
			// 			var s string
			// 			if v := x.Elem().Size(); v != 1 {
			// 				s = fmt.Sprintf("*%v", v)
			// 			}
			// 			b.w("(uintptr(unsafe.%sAdd(unsafe.Pointer(%s), (%s)%s)))", tag(preserve), c.expr(w, n.PostfixExpression, nil, exprUintpr), b1, s)
			// 			rt = x.Elem()
			// 		default:
			// 			b.w("%s[%s]", c.expr(w, n.PostfixExpression, n.PostfixExpression.Type().Undecay(), exprIndex), b1)
			// 		}
			// 	case *cc.PointerType:
			// 		var s string
			// 		if v := x.Elem().Size(); v != 1 {
			// 			s = fmt.Sprintf("*%v", v)
			// 		}
			// 		b.w("(*(*%s)(unsafe.%sAdd(unsafe.Pointer(%s), (%s)%s)))", c.typ(x.Elem()), tag(preserve), c.expr(w, n.PostfixExpression, n.PostfixExpression.Type().Undecay(), expr), b1, s)
			// 	default:
			// 		c.err(errorf("internal error %T", x))
			// 	}
			// }
			switch mode {
			case exprSelect, exprLvalue, exprDefault, exprIndex:
				switch x := n.PostfixExpression.Type().Undecay().(type) {
				case *cc.ArrayType:
					rt, rmode = n.Type(), mode
					b.w("%s[%s]", c.expr(w, n.PostfixExpression, nil, exprIndex), c.expr(w, n.ExpressionList, nil, exprDefault))
				case *cc.PointerType:
					rt, rmode = n.Type(), mode
					var s string
					if v := x.Elem().Size(); v != 1 {
						s = fmt.Sprintf("*%v", v)
					}
					b.w("(*(*%s)(unsafe.%sAdd(unsafe.Pointer(%s), (%s)%s)))", c.typ(x.Elem()), tag(preserve), c.expr(w, n.PostfixExpression, nil, exprDefault), c.expr(w, n.ExpressionList, nil, exprDefault), s)
				default:
					c.err(errorf("TODO %T", x))
				}
			case exprUintpr:
				rt, rmode = n.Type(), mode
				s := ""
				switch x := n.PostfixExpression.Type().(type) {
				case *cc.PointerType:
					if sz := x.Elem().Size(); sz != 1 {
						s = fmt.Sprintf("*%v", sz)
					}
				default:
					c.err(errorf("TODO %T", x))
				}
				b.w("uintptr(unsafe.%sAdd(unsafe.%[1]sPointer(&%s), (%s%s)))", tag(preserve), c.expr(w, n.PostfixExpression, nil, exprLvalue), c.expr(w, n.ExpressionList, nil, exprDefault), s)
			default:
				c.err(errorf("TODO %v", mode))
			}
		}
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		return c.postfixExpressionCall(w, n)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		switch mode {
		case exprLvalue, exprDefault:
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
			b.w("uintptr(unsafe.Pointer(&%s.", c.expr(w, n.PostfixExpression, nil, exprLvalue))
			switch f := n.Field(); {
			case f.Parent() != nil:
				c.err(errorf("TODO %v", n.Case))
			default:
				b.w("%s%s))", tag(field), f.Name())
			}
		default:
			c.err(errorf("TODO %v", mode))
		}
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		rt, rmode = n.Type(), mode
		b.w("(%s.", c.expr(w, n.PostfixExpression, nil, exprPointer))
		switch f := n.Field(); {
		case f.Parent() != nil:
			c.err(errorf("TODO %v", n.Case))
		default:
			b.w("%s%s)", tag(field), f.Name())
		}
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		switch pe := n.PostfixExpression.Type(); {
		case pe.Kind() == cc.Ptr && pe.(*cc.PointerType).Elem().Size() != 1:
			c.err(errorf("TODO %v", n.Case))
		// 	sz := pe.(*cc.PointerType).Elem().Size()
		// 	switch mode {
		// 	case exprVoid:
		// 		b.w("%s += %d", c.expr(w, n.PostfixExpression, nil, expr), sz)
		// 	default:
		// 		c.err(errorf("TODO"))
		// 	}
		default:
			switch mode {
			case exprVoid:
				b.w("%s++", c.expr(w, n.PostfixExpression, nil, exprDefault))
				rt, rmode = n.Type(), exprVoid
			case exprPointer:
				rt, rmode = n.Type(), mode
				v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
				switch d := c.declaratorOf(n.PostfixExpression); {
				case d != nil:
					switch x := d.Type().(type) {
					case *cc.PointerType:
						ds := c.expr(w, n.PostfixExpression, nil, exprDefault)
						w.w("\n%s := %s", v, ds)
						w.w("\n%s++", ds)
						b.w("(*%s)(unsafe.Pointer(%s))", c.typ(x.Elem()), v)
					default:
						c.err(errorf("TODO %T", x))
					}
				default:
					c.err(errorf("TODO"))
				}
			case exprDefault:
				v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
				switch d := c.declaratorOf(n.PostfixExpression); {
				case d != nil:
					rt, rmode = n.Type(), mode
					ds := c.expr(w, n.PostfixExpression, nil, exprDefault)
					w.w("\n%s := %s", v, ds)
					w.w("\n%s++", ds)
					b.w("%s", v)
				default:
					c.err(errorf("TODO"))
					// v2 := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
					// w.w("\n%s := %s", v2, c.expr(w, n.PostfixExpression, nil, exprUintpr))
					// w.w("\n(*(*%s)(unsafe.Pointer(%s)))++", c.typ(n.PostfixExpression.Type()), v2)
					// w.w("\n%s := (*(*%s)(unsafe.Pointer(%s)))", v, c.typ(n.PostfixExpression.Type()), v2)
					// b.w("%s", v)
				}
			default:
				c.err(errorf("TODO %v", mode))
			}
		}
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		switch pe := n.PostfixExpression.Type(); {
		case pe.Kind() == cc.Ptr && pe.(*cc.PointerType).Elem().Size() != 1:
			switch mode {
			case exprVoid:
				c.err(errorf("TODO"))
			default:
				c.err(errorf("TODO"))
			}
		default:
			switch mode {
			case exprVoid:
				rt, rmode = n.Type(), exprVoid
				b.w("%s--", c.expr(w, n.PostfixExpression, nil, exprDefault))
			default:
				c.err(errorf("TODO"))
				// v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
				// switch d := c.declaratorOf(n.PostfixExpression); {
				// case d != nil:
				// 	ds := c.expr(w, n.PostfixExpression, nil, expr)
				// 	w.w("\n%s := %s", v, ds)
				// 	w.w("\n%s--", ds)
				// 	b.w("%s", v)
				// default:
				// 	c.err(errorf("TODO"))
				// }
			}
		}
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return b.bytes(), rt, rmode
}

func (c *ctx) declaratorOf(n cc.ExpressionNode) *cc.Declarator {
	for n != nil {
		switch x := n.(type) {
		case *cc.PrimaryExpression:
			switch x.Case {
			case cc.PrimaryExpressionIdent: // IDENTIFIER
				if y, ok := x.ResolvedTo().(*cc.Declarator); ok {
					return y
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
			case cc.UnaryExpressionPostfix: // PostfixExpression
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
	b.w("%s(%stls", c.expr(w, n.PostfixExpression, nil, exprCall), tag(ccgoAutomatic))
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
	return b.bytes(), rt, rmode
}

func (c *ctx) assignmentExpression(w writer, n *cc.AssignmentExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		c.err(errorf("TODO %v", n.Case))
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		switch mode {
		case exprDefault /*, exprBool */ :
			v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
			w.w("\n%s := %s", v, c.expr(w, n.AssignmentExpression, n.UnaryExpression.Type(), exprDefault))
			w.w("\n%s = %s", c.expr(w, n.UnaryExpression, nil, exprDefault), v)
			b.w("%s", v)
			rt, rmode = n.Type(), exprDefault
		case exprVoid:
			// 	rhs := expr
			// 	if n.UnaryExpression.Type().Kind() == cc.Ptr && n.AssignmentExpression.Type().Undecay().Kind() == cc.Array {
			// 		rhs = exprUintpr
			// 	}
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

		op := n.Token.SrcStr()
		op = op[:len(op)-1]
		switch mode {
		case exprVoid:
			ct := c.usualArithmeticConversions(n.UnaryExpression.Type(), n.AssignmentExpression.Type())
			rt, rmode = n.Type(), mode
			switch d := c.declaratorOf(n.UnaryExpression); {
			case d != nil:
				b.w("\n%s = ", c.expr(w, n.UnaryExpression, nil, exprDefault))
				var b2 buf
				b2.w("(%s %s %s)", c.expr(w, n.UnaryExpression, ct, exprDefault), op, c.expr(w, n.AssignmentExpression, ct, exprDefault))
				b.w("%s", c.convert(n, w, b2.bytes(), ct, t, mode, mode))
			default:
				c.err(errorf("TODO"))
			}
		default:
			c.err(errorf("TODO %v", n.Case))
		}
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
	return r, rt, rmode
}

func (c *ctx) primaryExpression(w writer, n *cc.PrimaryExpression, t cc.Type, mode mode) (r []byte, rt cc.Type, rmode mode) {
	var b buf
out:
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		rt, rmode = n.Type(), mode
		switch x := n.ResolvedTo().(type) {
		case *cc.Declarator:
			// 	if x.StorageDuration() == cc.Automatic {
			// 		if info := c.f.declInfos.info(x); info.escapes() {
			// 			b.w("(%s)", bpOff(info.bpOff))
			// 			rt = x.Type().Pointer()
			// 			rmode = exprUintpr
			// 			break
			// 		}
			// 	}

			// 	if x.Type().Kind() == cc.Function {
			// 		switch mode {
			// 		case expr:
			// 			v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
			// 			w.w("\n%s := %s%s", v, c.declaratorTag(x), x.Name())
			// 			b.w("(*(*uintptr)(unsafe.Pointer(&%s)))", v)
			// 			break out
			// 		default:
			// 			rmode = exprFunc
			// 		}
			// 	}

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
					switch y := n.Type().Undecay().(type) {
					case *cc.ArrayType:
						b.w("%s", bpOff(info.bpOff))
					default:
						c.err(errorf("TODO %T %v:", y, n.Position()))
					}
				default:
					c.err(errorf("TODO %v %v:", mode, n.Position()))
				}
			default:
				if x.Type().Kind() == cc.Function {
					switch mode {
					case exprDefault:
						rt, rmode = n.Type(), mode
						v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
						w.w("\n%s := %s%s", v, c.declaratorTag(x), x.Name())
						b.w("(*(*uintptr)(unsafe.Pointer(&%s)))", v)
						break out
					}
				}

				switch mode {
				case exprBool:
					rmode = exprDefault
					b.w("%s%s", c.declaratorTag(x), x.Name())
				case exprDefault:
					b.w("%s%s", c.declaratorTag(x), x.Name())
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
				case exprPointer:
					b.w("((*%s)(unsafe.%sPointer(%s%s)))", c.typ(x.Type().(*cc.PointerType).Elem()), tag(preserve), c.declaratorTag(x), x.Name())
				case exprUintpr:
					if x.StorageDuration() == cc.Automatic {
						c.err(errorf("%v: internal error", n.Position()))
					}
					switch {
					case x.Type().Kind() == cc.Function:
						v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
						switch {
						case c.f == nil:
							w.w("\nvar %s = %s%s", v, c.declaratorTag(x), x.Name())
						default:
							w.w("\n%s := %s%s", v, c.declaratorTag(x), x.Name())
						}
						b.w("(*(*uintptr)(unsafe.Pointer(&%s)))", v)
					default:
						b.w("uintptr(unsafe.%sPointer(&%s%s))", tag(preserve), c.declaratorTag(x), x.Name())
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
				c.err(errorf("TODO %v", mode))
				// return c.intConst(x.Value(), n.Type(), t)
			}
		// case nil:
		// 	if n.Type().Kind() == cc.Ptr && n.Type().(*cc.PointerType).Elem().Kind() == cc.Function {
		// 		switch mode {
		// 		case expr:
		// 			v := fmt.Sprintf("%sv%d", tag(ccgoAutomatic), c.id())
		// 			w.w("\n%s := %s%s", v, tag(external), n.Token.Src())
		// 			b.w("(*(*uintptr)(unsafe.Pointer(&%s)))", v)
		// 			break out
		// 		default:
		// 			rmode = exprFunc
		// 		}
		// 	}

		// 	b.w("%s%s", tag(external), n.Token.Src())
		default:
			c.err(errorf("TODO %T", x))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		rt, rmode = n.Type(), exprUntyped
		b.w("%d", n.Value())
	case cc.PrimaryExpressionFloat: // FLOATCONST
		rt, rmode = n.Type(), exprUntyped
		b.w("%v", n.Value())
		// rt = t
		// switch x := n.Value().(type) {
		// case cc.Float64Value:
		// 	switch {
		// 	case t.Kind() == cc.Double:
		// 		s := fmt.Sprint(x)
		// 		v, err := strconv.ParseFloat(s, int(t.Size()*8))
		// 		if err != nil || v != float64(x) {
		// 			c.err(errorf("TODO %v != %v %v", x, v, err))
		// 			break
		// 		}

		// 		b.w("%s", s)
		// 	case t.Kind() == cc.Float:
		// 		s := fmt.Sprint(float32(x))
		// 		v, err := strconv.ParseFloat(s, int(t.Size()*8))
		// 		if err != nil || float32(v) != float32(x) {
		// 			c.err(errorf("TODO %v != %v %v", x, v, err))
		// 			break
		// 		}

		// 		b.w("%s", s)
		// 	case cc.IsIntegerType(t):
		// 		b.w("%s(%v)", c.typ(t), x)
		// 	default:
		// 		c.err(errorf("TODO %v", t.Kind()))
		// 	}
		// default:
		// 	c.err(errorf("TODO %T", x))
		// }
	case cc.PrimaryExpressionChar: // CHARCONST
		rt, rmode = n.Type(), exprUntyped
		b.w("%d", n.Value())
		// rt = t
		// switch x := n.Value().(type) {
		// case cc.Int64Value:
		// 	switch {
		// 	case cc.IsSignedInteger(t):
		// 		if t.Size() < 8 {
		// 			m := uint64(1)<<(t.Size()*8) - 1
		// 			switch {
		// 			case x < 0:
		// 				x |= ^cc.Int64Value(m)
		// 			default:
		// 				x &= cc.Int64Value(m)
		// 			}
		// 		}
		// 		switch {
		// 		case x >= ' ' && x < 0x7f:
		// 			b.w("int%d(%s)", 8*t.Size(), (strconv.QuoteRuneToASCII(rune(x))))
		// 		default:
		// 			b.w("int%d(%d)", 8*t.Size(), x)
		// 		}
		// 	case cc.IsIntegerType(t):
		// 		if t.Size() < 8 {
		// 			m := uint64(1)<<(t.Size()*8) - 1
		// 			x &= cc.Int64Value(m)
		// 		}
		// 		switch {
		// 		case x >= ' ' && x < 0x7f:
		// 			b.w("uint%d(%s)", 8*t.Size(), (strconv.QuoteRuneToASCII(rune(x))))
		// 		default:
		// 			b.w("uint%d(%d)", 8*t.Size(), x)
		// 		}
		// 	default:
		// 		switch {
		// 		case x >= ' ' && x < 0x7f:
		// 			b.w("%s(%s)", c.typ(t), (strconv.QuoteRuneToASCII(rune(x))))
		// 		default:
		// 			b.w("%s(%d)", c.typ(t), x)
		// 		}
		// 	}
		// default:
		// 	c.err(errorf("TODO %T", x))
		// }
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		c.err(errorf("TODO %v", n.Case))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		rt, rmode = n.Type(), exprUntyped
		b.w("%s", n.Value())
		// rt, rmode = t, exprDefault
		// switch x := t.(type) {
		// case *cc.ArrayType:
		// case *cc.PointerType:
		// 	b.w("%q", n.Value())
		// default:
		// 	c.err(errorf("TODO %T", x))
		// }
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
	return b.bytes(), rt, rmode
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

// func (c *ctx) intConst(v cc.Value, from, to cc.Type) (r []byte, rt cc.Type, rmode mode) {
// 	var b buf
// 	if !cc.IsIntegerType(to) && to.Kind() != cc.Ptr {
// 		b.w("%d", v)
// 		return b.bytes(), from, rmode
// 	}
//
// 	switch x := v.(type) {
// 	case cc.Int64Value:
// 		switch {
// 		case cc.IsSignedInteger(to):
// 			if to.Size() < 8 {
// 				m := uint64(1)<<(to.Size()*8) - 1
// 				switch {
// 				case x < 0:
// 					x |= ^cc.Int64Value(m)
// 				default:
// 					x &= cc.Int64Value(m)
// 				}
// 			}
// 			b.w("int%d(%d)", 8*to.Size(), x)
// 		default:
// 			if to.Size() < 8 {
// 				m := uint64(1)<<(to.Size()*8) - 1
// 				x &= cc.Int64Value(m)
// 			}
// 			switch {
// 			case to.Kind() == cc.Ptr:
// 				b.w("uintptr(%d)", uint64(x))
// 			default:
// 				b.w("uint%d(%d)", 8*to.Size(), uint64(x))
// 			}
// 		}
// 	case cc.UInt64Value:
// 		switch {
// 		case cc.IsSignedInteger(to):
// 			c.err(errorf("TODO"))
// 		default:
// 			if to.Size() < 8 {
// 				m := uint64(1)<<(to.Size()*8) - 1
// 				x &= cc.UInt64Value(m)
// 			}
// 			switch {
// 			case to.Kind() == cc.Ptr:
// 				b.w("uintptr(%d)", uint64(x))
// 			default:
// 				b.w("uint%d(%d)", 8*to.Size(), uint64(x))
// 			}
// 		}
// 	default:
// 		c.err(errorf("TODO %T", x))
// 	}
// 	return b.bytes(), to, rmode
// }
