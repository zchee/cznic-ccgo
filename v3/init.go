// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"fmt"
	"strings"

	"modernc.org/cc/v3"
)

// 6.7.8 Initialization
func (p *project) initializer(f *function, n *cc.Initializer, t cc.Type, tld *tld, fld cc.Field) {
	// 11: The initializer for a scalar shall be a single expression, optionally
	// enclosed in braces. The initial value of the object is that of the
	// expression (after conversion); the same type constraints and conversions as
	// for simple assignment apply, taking the type of the scalar to be the
	// unqualified version of its declared type.
	if t.IsScalarType() {
		switch n.Case {
		case cc.InitializerExpr: // AssignmentExpression
			p.w("%s", tidyComment("", n.AssignmentExpression))
			switch {
			case tld != nil && t.Kind() == cc.Ptr && n.AssignmentExpression.Operand.Value() == nil:
				tld.patches = append(tld.patches, initPatch{t, n, fld})
				p.w(" 0 ")
			default:
				p.assignmentExpression(f, n.AssignmentExpression, t, exprValue, fOutermost)
			}
		case cc.InitializerInitList: // '{' InitializerList ',' '}'
			panic(todo("", n.Position(), n.Case))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
		return
	}

	k := t.Kind()

	// 13: The initializer for a structure or union object that has automatic
	// storage duration shall be either an initializer list as described below, or
	// a single expression that has compatible structure or union type. In the
	// latter case, the initial value of the object, including unnamed members, is
	// that of the expression.
	if k == cc.Struct || k == cc.Union {
		switch n.Case {
		case cc.InitializerExpr: // AssignmentExpression
			p.w("%s", tidyComment("", n.AssignmentExpression))
			p.assignmentExpression(f, n.AssignmentExpression, t, exprValue, fOutermost)
			return
		case cc.InitializerInitList: // '{' InitializerList ',' '}'
			// ok
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}

	// 14: An array of character type may be initialized by a character string
	// literal, optionally enclosed in braces. Successive characters of the
	// character string literal (including the terminating null character if there
	// is room or if the array is of unknown size) initialize the elements of the
	// array.
	if k == cc.Array && isCharType(t.Elem()) {
		var v cc.Value
		switch n.Case {
		case cc.InitializerExpr: // AssignmentExpression
			v = n.AssignmentExpression.Operand.Value()
		case cc.InitializerInitList: // '{' InitializerList ',' '}'
			list := n.InitializerList.List()
			if len(list) != 1 {
				break
			}

			panic(todo("", n.Position(), n.Case))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}

		if x, ok := v.(cc.StringValue); ok {
			s := cc.StringID(x).String()
			m := uintptr(len(s)) + 1
			tn := t.Len()
			switch {
			case tn < m-1:
				panic(todo("", pos(n), t.Len(), m))
			case tn < m:
				p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(n, t), p.stringLiteralString(s))
			default: // tn >= m
				p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(n, t), p.stringLiteralString(s+strings.Repeat("\x00", int(tn-m))))
			}
			return
		}
	}

	// 15: An array with element type compatible with wchar_t may be initialized by
	// a wide string literal, optionally enclosed in braces. Successive wide
	// characters of the wide string literal (including the terminating null wide
	// character if there is room or if the array is of unknown size) initialize
	// the elements of the array.
	if k == cc.Array && p.isWCharType(t.Elem()) {
		var v cc.Value
		switch n.Case {
		case cc.InitializerExpr: // AssignmentExpression
			v = n.AssignmentExpression.Operand.Value()
		case cc.InitializerInitList: // '{' InitializerList ',' '}'
			list := n.InitializerList.List()
			if len(list) != 1 {
				break
			}

			panic(todo("", n.Position(), n.Case))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}

		if x, ok := v.(cc.WideStringValue); ok {
			s := []rune(cc.StringID(x).String())
			m := uintptr(len(s)) + 1
			tn := t.Len()
			switch {
			case tn < m-1:
				panic(todo("", pos(n)))
			case tn < m:
				p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(n, t), p.wideStringLiteral(x, 0))
			default: // tn >= m
				p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(n, t), p.wideStringLiteral(x, int(tn-m)))
			}
			return
		}
	}

	// 16: Otherwise, the initializer for an object that has aggregate or union
	// type shall be a brace-enclosed list of initializers for the elements or
	// named members.
	if n.Case != cc.InitializerInitList { // '{' InitializerList ',' '}'
		panic(todo("", pos(n)))
	}

	switch t.Kind() {
	case cc.Array:
		p.initializerListArray(f, n, t, tld)
	case cc.Struct:
		p.initializerListStruct(f, n, t, tld)
	case cc.Union:
		p.initializerListUnion(f, n, t, tld)
	default:
		panic(todo("", n.Position(), k, t))
	}
}

func (p *project) initializerListUnion(f *function, n0 *cc.Initializer, t cc.Type, tld *tld) {
	n := n0.InitializerList
	p.w("%s%s{", tidyComment("", &n0.Token), p.typ(n, t))
	idx := []int{0}
	for ; n != nil; n = n.InitializerList {
		if n.Designation != nil {
			panic(todo("", pos(n)))
		}

		if idx[0] != 0 {
			panic(todo("", pos(n)))
		}

		fld := t.FieldByIndex(idx)
		p.w("%s:", p.fieldName(n, fld.Name()))
		ft := fld.Type()
		var s string
		if ft.IsBitFieldType() {
			p.w("%sUint%dFrom%s(", p.task.crt, fld.BitFieldBlockWidth(), p.helperType(n, n.Initializer.Type()))
			s = ")"
		}

		switch {
		case isAggregateType(ft) && n.Initializer.Case != cc.InitializerInitList && ft.Kind() != n.Initializer.AssignmentExpression.Operand.Type().Kind():
			panic(todo("", n.Position(), t, ft))
		default:
			p.initializer(f, n.Initializer, ft, tld, fld)
		}
		p.w("%s,", s)
		idx[0]++
	}
	p.w("%s}", tidyComment("", &n0.Token3))
}

func (p *project) initializerListStruct(f *function, n0 *cc.Initializer, t cc.Type, tld *tld) {
	n := n0.InitializerList
	p.w("%s%s{", tidyComment("", &n0.Token), p.typ(n, t))
	idx := []int{0}
	var m map[uintptr][]string
	info := p.structLayout(t)
	nvalues := 0
	for list := n; list != nil; list = list.InitializerList {
		if list.Designation != nil {
			panic(todo("", list.Position(), t))
		}

		fld := t.FieldByIndex(idx)
		for fld.IsBitField() && fld.Name() == 0 {
			idx[0]++
			fld = t.FieldByIndex(idx)
		}
		nvalues++
		if fld.IsBitField() {
			panic(todo("", pos(n)))
			if m == nil {
				m = map[uintptr][]string{}
				for _, off := range info.offs {
					a := info.flds[off]
					if f := a[0]; f.IsBitField() && !f.BitFieldBlockFirst().IsBitField() {
						panic(todo("\n%s", dumpLayout(t)))
					}
				}
			}
			init := list.Initializer
			if init.Case != cc.InitializerExpr {
				panic(todo("", pos(n)))
			}

			switch x := init.AssignmentExpression.Operand.Value().(type) {
			case cc.Int64Value:
				off := fld.Offset()
				m[off] = append(m[off], fmt.Sprintf("%d<<%d&%#x", x, fld.BitFieldOffset(), fld.Mask()))
			case cc.Uint64Value:
				off := fld.Offset()
				m[off] = append(m[off], fmt.Sprintf("%d<<%d&%#x", x, fld.BitFieldOffset(), fld.Mask()))
			default:
				panic(todo("%T(%v) %v", x, x, init.AssignmentExpression.Operand.Type()))
			}
		}
		idx[0]++
	}
	idx[0] = 0
	keys := nvalues != t.NumField() || len(info.padBefore) != 0 || info.padAfter != 0
	for list := n; list != nil; list = list.InitializerList {
		if list.Designation != nil {
			panic(todo("", list.Position(), t))
		}

		fld := t.FieldByIndex(idx)
		for fld.IsBitField() && fld.Name() == 0 {
			idx[0]++
			fld = t.FieldByIndex(idx)
		}
		ft := fld.Type()
		comma := ","
		switch {
		case fld.IsBitField():
			panic(todo("", pos(n)))
			off := fld.Offset()
			if info.flds[off][0].Name() != fld.Name() {
				comma = ""
				break
			}

			if keys {
				p.w("%s: ", p.bitFieldName(list, fld.BitFieldBlockFirst()))
			}
			p.w("%s", strings.Join(m[off], "|"))
		case isAggregateType(ft) && list.Initializer.Case != cc.InitializerInitList && ft.Kind() != list.Initializer.AssignmentExpression.Operand.Type().Kind():
			panic(todo("", list.Position(), t, ft))
		default:
			if keys {
				p.w("%s: ", p.fieldName(list, fld.Name()))
			}
			p.initializer(f, list.Initializer, ft, tld, fld)
		}
		p.w("%s", comma)
		idx[0]++
	}
	p.w("%s}", tidyComment("", &n0.Token3))
}

func (p *project) initializerListArray(f *function, n0 *cc.Initializer, t cc.Type, tld *tld) {
	n := n0.InitializerList
	p.w("%s%s{", tidyComment("", &n0.Token), p.typ(n, t))
	et := t.Elem()
	for ; n != nil; n = n.InitializerList {
		if n.Designation != nil {
			panic(todo("", pos(n)))
		}

		switch {
		case isAggregateType(et) && n.Initializer.Case != cc.InitializerInitList && et.Kind() != n.Initializer.AssignmentExpression.Operand.Type().Kind():
			panic(todo("", pos(n)))
		default:
			p.initializer(f, n.Initializer, et, tld, nil)
		}
		p.w(",")
	}
	p.w("%s}", tidyComment("", &n0.Token3))
}

func isAggregateType(t cc.Type) bool {
	switch t.Kind() {
	case cc.Struct, cc.Union, cc.Array:
		return true
	}

	return false
}

func isCharType(t cc.Type) bool {
	switch t.Kind() {
	case cc.Char, cc.SChar, cc.UChar:
		return true
	}

	return false
}

func (p *project) isWCharType(t cc.Type) bool {
	return t.IsIntegerType() && t.Size() == p.ast.WideCharType.Size()
}
