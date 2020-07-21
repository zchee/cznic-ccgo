// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"strings"

	"modernc.org/cc/v3"
)

// // 6.7.8 Initialization
func (p *project) initializer(f *function, n *cc.Initializer, t cc.Type, tld *tld, fld cc.Field) {
	// 11: The initializer for a scalar shall be a single expression, optionally
	// enclosed in braces. The initial value of the object is that of the
	// expression (after conversion); the same type constraints and conversions as
	// for simple assignment apply, taking the type of the scalar to be the
	// unqualified version of its declared type.
	if t.IsScalarType() {
		if fld != nil && fld.IsBitField() {
			panic(todo("", pos(n), t, fld.Name(), fld.Type()))
		}

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
	if tld == nil && (k == cc.Struct || k == cc.Union) {
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
			list := p.initializerList(n.InitializerList)
			if len(list) != 1 {
				break
			}

			if e := n.InitializerList.Initializer.AssignmentExpression; e != nil {
				v = e.Operand.Value()
			}
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}

		if x, ok := v.(cc.StringValue); ok {
			s := cc.StringID(x).String()
			m := uintptr(len(s)) + 1
			tn := t.Len()
			switch {
			case tn < m-1:
				p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(n, t), p.stringLiteralString(s[:tn]))
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
			list := p.initializerList(n.InitializerList)
			if len(list) != 1 {
				break
			}

			if e := n.InitializerList.Initializer.AssignmentExpression; e != nil {
				v = e.Operand.Value()
			}
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

	if !isAggregateType(t) {
		panic(todo("", pos(n), t))
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
	switch seenBitfield, seenNonKeyableBitfield := p.checkInitializerBitFields(n0, t); {
	case !seenBitfield:
		first := true
		for list := n; list != nil; list = list.InitializerList {
			if !first {
				panic(todo(""))
			}

			fld := list.Initializer.Field
			p.w("%s: ", p.fieldName2(list, fld))
			p.initializer(f, list.Initializer, fld.Type(), tld, fld)
			p.w(",")
			first = false
		}
	case !seenNonKeyableBitfield:
		m := p.collectInitializerBitfields(n0)
		first := true
		for list := n; list != nil; list = list.InitializerList {
			if !first {
				panic(todo(""))
			}

			init := list.Initializer
			fld := init.Field
			switch {
			case fld.IsBitField():
				inits := m[init.Offset]
				if inits == nil {
					continue
				}

				p.w("%s: ", p.fieldName2(list, fld))
				for i, init := range inits {
					if i != 0 {
						p.w(" | ")
					}
					p.assignmentExpression(f, init.AssignmentExpression, p.bitFieldType(fld.BitFieldBlockWidth()), exprValue, fOutermost)
					p.w("&%#x<<%d", 1<<init.Field.BitFieldWidth()-1, init.Field.BitFieldOffset())
				}
				delete(m, init.Offset)
			default:
				p.w("%s: ", p.fieldName2(list, fld))
				p.initializer(f, list.Initializer, fld.Type(), tld, fld)
			}
			p.w(",")
			first = false
		}
	default:
		panic(todo("", pos(n), seenBitfield, seenNonKeyableBitfield, t))
	}
	p.w("%s}", tidyComment("", &n0.Token3))
}

func (p *project) initializerListStruct(f *function, n0 *cc.Initializer, t cc.Type, tld *tld) {
	n := n0.InitializerList
	p.w("%s%s{", tidyComment("", &n0.Token), p.typ(n, t))
	switch seenBitfield, seenNonKeyableBitfield := p.checkInitializerBitFields(n0, t); {
	case !seenBitfield:
		for list := n; list != nil; list = list.InitializerList {
			fld := list.Initializer.Field
			p.w("%s: ", p.fieldName2(list, fld))
			p.initializer(f, list.Initializer, fld.Type(), tld, fld)
			p.w(",")
		}
	case !seenNonKeyableBitfield:
		m := p.collectInitializerBitfields(n0)
		for list := n; list != nil; list = list.InitializerList {
			init := list.Initializer
			fld := init.Field
			switch {
			case fld.IsBitField():
				inits := m[init.Offset]
				if inits == nil {
					continue
				}

				p.w("%s: ", p.fieldName2(list, fld))
				for i, init := range inits {
					if i != 0 {
						p.w(" | ")
					}
					p.assignmentExpression(f, init.AssignmentExpression, p.bitFieldType(fld.BitFieldBlockWidth()), exprValue, fOutermost)
					p.w("&%#x<<%d", 1<<init.Field.BitFieldWidth()-1, init.Field.BitFieldOffset())
				}
				delete(m, init.Offset)
			default:
				p.w("%s: ", p.fieldName2(list, fld))
				p.initializer(f, list.Initializer, fld.Type(), tld, fld)
			}
			p.w(",")
		}
	default:
		panic(todo("", pos(n), seenBitfield, seenNonKeyableBitfield, t, dumpLayout(t, p.structLayout(t))))
	}
	p.w("%s}", tidyComment("", &n0.Token3))
}

func (p *project) bitFieldType(bits int) cc.Type {
	switch bits {
	case 8:
		return p.task.cfg.ABI.Type(cc.UChar)
	case 16:
		return p.task.cfg.ABI.Type(cc.UShort)
	case 32:
		return p.task.cfg.ABI.Type(cc.UInt)
	case 64:
		return p.task.cfg.ABI.Type(cc.ULongLong)
	default:
		panic(todo(""))
	}
}

func (p *project) collectInitializerBitfields(n *cc.Initializer) (r map[uintptr][]*cc.Initializer) {
	r = map[uintptr][]*cc.Initializer{}
	for _, v := range p.initializerList(n.InitializerList) {
		if f := v.Field; f != nil && f.IsBitField() {
			r[v.Offset] = append(r[v.Offset], v)
		}
	}
	return r
}

func (p *project) checkInitializerBitFields(n *cc.Initializer, t cc.Type) (seenBitfield, seenNonKeyableBitfield bool) {
	for _, v := range p.initializerList(n.InitializerList) {
		if f := v.Field; f != nil && f.IsBitField() && f.BitFieldWidth() != 0 {
			seenBitfield = true
			if !f.BitFieldBlockFirst().IsBitField() {
				seenNonKeyableBitfield = true
				return true, true
			}
		}
	}
	return seenBitfield, false
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

func (p *project) initializerList(n *cc.InitializerList) (r []*cc.Initializer) {
	for list := n; list != nil; list = list.InitializerList {
		r = append(r, list.Initializer)
	}
	return r
}
