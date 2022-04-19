// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"sort"

	"modernc.org/cc/v4"
)

func (c *ctx) initializerOuter(w writer, n *cc.Initializer, t cc.Type) (r []byte) {
	return c.initializer(w, c.initalizerFlatten(n, nil), t, 0)
}

func (c *ctx) initalizerFlatten(n *cc.Initializer, a []*cc.Initializer) (r []*cc.Initializer) {
	r = a
	switch n.Case {
	case cc.InitializerExpr: // AssignmentExpression
		return append(r, n)
	case cc.InitializerInitList: // '{' InitializerList ',' '}'
		for l := n.InitializerList; l != nil; l = l.InitializerList {
			r = append(r, c.initalizerFlatten(l.Initializer, nil)...)
		}
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	return r
}

func (c *ctx) initializer(w writer, a []*cc.Initializer, t cc.Type, off0 int64) (r []byte) {
	if len(a) == 0 {
		c.err(errorf("TODO"))
		return nil
	}

	if cc.IsScalarType(t) {
		if len(a) != 1 {
			c.err(errorf("TODO"))
			return nil
		}

		if a[0].Offset()-off0 != 0 {
			c.err(errorf("TODO"))
			return nil
		}

		return c.expr(w, a[0].AssignmentExpression, t, expr)
	}

	switch x := t.(type) {
	case *cc.ArrayType:
		if len(a) == 1 && a[0].Type().Kind() == cc.Array && a[0].Value() != cc.Unknown {
			return c.expr(w, a[0].AssignmentExpression, t, expr)
		}

		return c.initializerArray(w, a, x, off0)
	// case *cc.PredefinedType:
	// 	if len(a) != 1 {
	// 		c.err(errorf("TODO"))
	// 		return nil
	// 	}

	// 	if a[0].Offset()-off0 != 0 {
	// 		c.err(errorf("TODO"))
	// 		return nil
	// 	}

	// 	return c.expr(w, a[0].AssignmentExpression, t, expr)
	default:
		//trc("%v: in type %v, in expr type %v, t %v", a[0].Position(), a[0].Type(), a[0].AssignmentExpression.Type(), t)
		c.err(errorf("TODO %T", x))
		return nil
	}
}

func (c *ctx) initializerArray(w writer, a []*cc.Initializer, t *cc.ArrayType, off0 int64) (r []byte) {
	var b buf
	b.w("%s{", c.typ(t))
	if len(a) != 0 {
		et := t.Elem()
		esz := et.Size()
		m := map[int64][]*cc.Initializer{}
		for _, v := range a {
			off := v.Offset() - off0
			off -= off % esz
			m[off] = append(m[off], v)
		}
		var offs []int64
		for k := range m {
			offs = append(offs, k)
		}
		sort.Slice(offs, func(i, j int) bool { return offs[i] < offs[j] })
		var off int64
		keys := false
		for _, v := range offs {
			if v != off {
				keys = true
				break
			}

			off += esz
		}
		for _, off := range offs {
			if keys {
				b.w("%d: ", off/esz)
			}

			ins := m[off]
			sort.Slice(ins, func(i, j int) bool { return ins[i].Offset() < ins[j].Offset() })
			b.w("%s, ", c.initializer(w, ins, et, off))
		}
	}
	b.w("}")
	return b.bytes()
}
