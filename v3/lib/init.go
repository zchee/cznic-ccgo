// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//TODO 75_ TCC (cc/v3)

package ccgo // import "modernc.org/ccgo/v3/lib"

import (
	"sort"
	"strings"

	"modernc.org/cc/v3"
)

func isAggregateTypeOrUnion(t cc.Type) bool {
	switch t.Kind() {
	case cc.Struct, cc.Union, cc.Array:
		return true
	}

	return false
}

// 6.7.8 Initialization
func (p *project) initializer(f *function, n *cc.Initializer, t cc.Type, sc cc.StorageClass, tld *tld) {
	s := p.initializerFlatten(n)
	sort.Slice(s, func(i, j int) bool {
		a := s[i]
		b := s[j]
		if a.Offset < b.Offset {
			return true
		}

		if a.Offset > b.Offset {
			return false
		}

		if a.Field == nil || b.Field == nil || !a.Field.IsBitField() || !b.Field.IsBitField() {
			panic(todo("%v: internal error: %#x, %v: %#x", a.Position(), a.Offset, b.Position(), b.Offset))
		}

		return a.Field.BitFieldOffset() < b.Field.BitFieldOffset()
	})
	p.initializerInner(0, f, s, t, sc, tld, nil)
}

func (p *project) initializerInner(off uintptr, f *function, s []*cc.Initializer, t cc.Type, sc cc.StorageClass, tld *tld, fld cc.Field) {
	// 11: The initializer for a scalar shall be a single expression, optionally
	// enclosed in braces. The initial value of the object is that of the
	// expression (after conversion); the same type constraints and conversions as
	// for simple assignment apply, taking the type of the scalar to be the
	// unqualified version of its declared type.
	if t.IsScalarType() && len(s) == 1 {
		switch {
		case tld != nil && t.Kind() == cc.Ptr && s[0].AssignmentExpression.Operand.Value() == nil:
			tld.patches = append(tld.patches, initPatch{t, s[0], fld})
			p.w(" 0 ")
		default:
			p.assignmentExpression(f, s[0].AssignmentExpression, t, exprValue, fOutermost)
		}
		return
	}

	// 12: The rest of this subclause deals with initializers for objects that have
	// aggregate or union type.

	k := t.Kind()

	// 13: The initializer for a structure or union object that has automatic
	// storage duration shall be either an initializer list as described below, or
	// a single expression that has compatible structure or union type. In the
	// latter case, the initial value of the object, including unnamed members, is
	// that of the expression.
	if sc == cc.Automatic && len(s) == 1 {
		switch k {
		case cc.Struct, cc.Union:
			if compatibleStructOrUnion(t, s[0].AssignmentExpression.Operand.Type()) {
				p.w("%s", tidyComment("", s[0].AssignmentExpression))
				p.assignmentExpression(f, s[0].AssignmentExpression, t, exprValue, fOutermost)
				return
			}
		}
	}

	if k == cc.Array && len(s) == 1 {
		et := t.Elem()
		switch {
		case isCharType(et):
			// 14: An array of character type may be initialized by a character string
			// literal, optionally enclosed in braces. Successive characters of the
			// character string literal (including the terminating null character if there
			// is room or if the array is of unknown size) initialize the elements of the
			// array.
			if x, ok := s[0].AssignmentExpression.Operand.Value().(cc.StringValue); ok {
				str := cc.StringID(x).String()
				slen := uintptr(len(str)) + 1
				alen := t.Len()
				switch {
				case alen < slen-1:
					p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(s[0], t), p.stringLiteralString(str[:alen]))
				case alen < slen:
					p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(s[0], t), p.stringLiteralString(str))
				default: // alen >= slen
					p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(s[0], t), p.stringLiteralString(str+strings.Repeat("\x00", int(alen-slen))))
				}
				return
			}
		case p.isWCharType(et):
			// 15: An array with element type compatible with wchar_t may be initialized by
			// a wide string literal, optionally enclosed in braces. Successive wide
			// characters of the wide string literal (including the terminating null wide
			// character if there is room or if the array is of unknown size) initialize
			// the elements of the array.
			if x, ok := s[0].AssignmentExpression.Operand.Value().(cc.WideStringValue); ok {
				str := []rune(cc.StringID(x).String())
				slen := uintptr(len(str)) + 1
				alen := t.Len()
				switch {
				case alen < slen-1:
					panic(todo("", p.pos(s[0])))
				case alen < slen:
					p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(s[0], t), p.wideStringLiteral(x, 0))
				default: // alen >= slen
					p.w("*(*%s)(unsafe.Pointer(%s))", p.typ(s[0], t), p.wideStringLiteral(x, int(alen-slen)))
				}
				return
			}
		}
	}

	// 16: Otherwise, the initializer for an object that has aggregate or union
	// type shall be a brace-enclosed list of initializers for the elements or
	// named members.
	switch k {
	case cc.Array:
		p.initializerArray(off, f, s, t, sc, tld)
	case cc.Struct:
		p.initializerStruct(off, f, s, t, sc, tld)
	case cc.Union:
		p.initializerUnion(off, f, s, t, sc, tld)
	default:
		panic(todo("%v: internal error: %v %v", s[0].Position(), t, len(s)))
	}
}

func (p *project) initializerArray(off uintptr, f *function, s []*cc.Initializer, t cc.Type, sc cc.StorageClass, tld *tld) {
	if len(s) == 0 {
		p.w("%s{}", p.typ(nil, t))
		return
	}

	et := t.Elem()
	esz := et.Size()
	var nl string
	if len(s) > 1 {
		nl = "\n"
	}
	s0 := s[0]
	p.w("%s{%s", p.typ(s0, t), nl)
	mustIndex := !et.IsScalarType() || uintptr(len(s)) != t.Len()
	var parts []*cc.Initializer
	var isZero bool
	for len(s) != 0 {
		s, parts, isZero = p.initializerArrayElement(off, s, esz)
		if isZero {
			mustIndex = true
			continue
		}

		elemOff := parts[0].Offset - off
		if mustIndex {
			p.w("%d:", elemOff/esz)
		}

		p.initializerInner(off+elemOff, f, parts, et, sc, tld, nil)
		p.w(",%s", nl)
	}
	p.w("}")
}

func (p *project) initializerArrayElement(off uintptr, s []*cc.Initializer, elemSize uintptr) (r []*cc.Initializer, parts []*cc.Initializer, isZero bool) {
	r = s
	isZero = true
	valueOff := s[0].Offset - off
	elemOff := valueOff - valueOff%elemSize
	nextOff := elemOff + elemSize
	for len(s) != 0 {
		if v := s[0]; v.Offset-off < nextOff {
			s = s[1:]
			parts = append(parts, v)
			if !v.AssignmentExpression.Operand.IsZero() {
				isZero = false
			}
			continue
		}

		break
	}
	return r[len(parts):], parts, isZero
}

func (p *project) initializerStruct(off uintptr, f *function, s []*cc.Initializer, t cc.Type, sc cc.StorageClass, tld *tld) {
	if len(s) == 0 {
		p.w("%s{}", p.typ(nil, t))
		return
	}

	s0 := s[0]
	p.w("%s%s{", listCommentPrefix(s0, ""), p.typ(s[0], t))
	var parts []*cc.Initializer
	var isZero bool
	var fld cc.Field
	for len(s) != 0 {
		s, fld, parts, isZero = p.initializerStructField(off, s, t)
		if isZero {
			continue
		}

		p.w("%s%s:", p.initializerSep("\n", parts[0]), p.fieldName2(parts[0], fld))
		ft := fld.Type()
		switch {
		case fld.IsBitField():
			first := true
			for _, v := range parts {
				if v.AssignmentExpression.Operand.IsZero() {
					continue
				}

				if !first {
					p.w("|")
				}
				first = false
				bitFld := v.Field
				bft := p.bitFileType(bitFld.BitFieldBlockWidth())
				p.assignmentExpression(f, v.AssignmentExpression, bft, exprValue, fOutermost)
				p.w("&%#x", uint64(1)<<uint64(bitFld.BitFieldWidth())-1)
				if o := bitFld.BitFieldOffset(); o != 0 {
					p.w("<<%d", o)
				}
			}
		default:
			p.initializerInner(off+fld.Offset(), f, parts, ft, sc, tld, fld)
		}
		p.w(",")
	}
	p.w("%s}", listCommentSuffix(s0, ""))
}

func (p *project) initializerStructField(off uintptr, s []*cc.Initializer, t cc.Type) (r []*cc.Initializer, fld cc.Field, parts []*cc.Initializer, isZero bool) {
	r = s
	isZero = true
	valueOff := s[0].Offset
	nf := t.NumField()
	nextOff := t.Size() + off
	bits := false
	for i := []int{0}; i[0] < nf; i[0]++ {
		fld2 := t.FieldByIndex(i)
		if fld2.Name() == 0 {
			continue
		}

		if fld == nil {
			fld = fld2
		}
		if fld2.Offset()+off > valueOff {
			nextOff = fld2.Offset() + off
			break
		}

		if !fld2.IsBitField() {
			fld = fld2
			continue
		}

		fld = fld2.BitFieldBlockFirst()
	}
	for len(s) != 0 {
		if v := s[0]; v.Offset < nextOff {
			if v.Field != nil && v.Field.IsBitField() {
				bits = true
			}
			s = s[1:]
			parts = append(parts, v)
			if !v.AssignmentExpression.Operand.IsZero() {
				isZero = false
			}
			continue
		}

		break
	}
	if bits && fld.Name() == 0 {
		for _, v := range parts {
			if v.Field != nil && v.Field.Name() != 0 {
				fld = v.Field
				break
			}
		}
	}
	return r[len(parts):], fld, parts, isZero
}

func (p *project) initializerUnion(off uintptr, f *function, s []*cc.Initializer, t cc.Type, sc cc.StorageClass, tld *tld) {
	if len(s) == 0 {
		p.w("%s{}", p.typ(nil, t))
		return
	}

	s0 := s[0]
	p.w("%s%s{", listCommentPrefix(s0, ""), p.typ(s[0], t))
	var parts []*cc.Initializer
	var isZero bool
	var fld cc.Field
	s, fld, parts, isZero = p.initializerUnionField(off, s, t)
	if len(s) != 0 {
		panic(todo("%v: internal error: %v", s0.Position(), t))
	}
	if !isZero {
		ft := fld.Type()
		p.w("%s%s:", p.initializerSep("\n", parts[0]), p.fieldName2(parts[0], fld))
		switch {
		case fld.IsBitField():
			first := true
			for _, v := range parts {
				if v.AssignmentExpression.Operand.IsZero() {
					continue
				}

				if !first {
					p.w("|")
				}
				first = false
				bitFld := v.Field
				bft := p.bitFileType(bitFld.BitFieldBlockWidth())
				p.assignmentExpression(f, v.AssignmentExpression, bft, exprValue, fOutermost)
				p.w("&%#x", uint64(1)<<uint64(bitFld.BitFieldWidth())-1)
				if o := bitFld.BitFieldOffset(); o != 0 {
					p.w("<<%d", o)
				}
			}
		default:
			p.initializerInner(off+fld.Offset(), f, parts, ft, sc, tld, fld)
		}
	}
	p.w("%s}", listCommentSuffix(s0, ""))
}

func (p *project) initializerUnionField(off uintptr, s []*cc.Initializer, t cc.Type) (r []*cc.Initializer, fld cc.Field, parts []*cc.Initializer, isZero bool) {
	r = s
	isZero = true
	fld = t.FieldByIndex([]int{0})
	nextOff := off + fld.Type().Size()
	for len(s) != 0 {
		if v := s[0]; v.Offset < nextOff {
			s = s[1:]
			parts = append(parts, v)
			if !v.AssignmentExpression.Operand.IsZero() {
				isZero = false
			}
			continue
		}

		break
	}
	return r[len(parts):], fld, parts, isZero
}

func (p *project) initializerSep(dflt string, n *cc.Initializer) string {
	if n := n.Parent(); n != nil {
		switch n.Case {
		case cc.InitializerExpr: // AssignmentExpression
			return dflt //TODO
		case cc.InitializerInitList: // '{' InitializerList ',' '}'
			return tidyComment(dflt, &n.Token)
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}

	return dflt
}

func compatibleStructOrUnion(t1, t2 cc.Type) bool {
	switch t1.Kind() {
	case cc.Struct:
		if t2.Kind() != cc.Struct {
			return false
		}
	case cc.Union:
		if t2.Kind() != cc.Union {
			return false
		}
	default:
		return false
	}

	if tag := t1.Tag(); tag != 0 && t2.Tag() != tag {
		return false
	}

	nf := t1.NumField()
	if t2.NumField() != nf {
		return false
	}

	for i := []int{0}; i[0] < nf; i[0]++ {
		f1 := t1.FieldByIndex(i)
		f2 := t2.FieldByIndex(i)
		nm := f1.Name()
		if f2.Name() != nm {
			return false
		}

		ft1 := f1.Type()
		ft2 := f2.Type()
		if ft1.Size() != ft2.Size() ||
			f1.IsBitField() != f2.IsBitField() ||
			f1.BitFieldOffset() != f2.BitFieldOffset() ||
			f1.BitFieldWidth() != f2.BitFieldWidth() {
			return false
		}

		if !compatibleType(ft1, ft2) {
			return false
		}
	}
	return true
}

func compatibleType(t1, t2 cc.Type) bool {
	if t1.Kind() != t2.Kind() {
		return false
	}

	switch t1.Kind() {
	case cc.Array:
		if t1.Len() != t2.Len() || !compatibleType(t1.Elem(), t2.Elem()) {
			return false
		}
	case cc.Struct, cc.Union:
		if !compatibleStructOrUnion(t1, t2) {
			return false
		}
	}
	return true
}

func listCommentPrefix(n *cc.Initializer, dflt string) string {
	if parent := n.Parent(); parent != nil {
		if parent.Case == cc.InitializerInitList {
			return tidyComment(dflt, &parent.Token)
		}
	}
	return dflt
}

func listCommentSuffix(n *cc.Initializer, dflt string) string {
	if parent := n.Parent(); parent != nil {
		if parent.Case == cc.InitializerInitList {
			return tidyComment(dflt, &parent.Token3)
		}
	}
	return dflt
}

func (p *project) bitFileType(bits int) cc.Type {
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
		panic(todo("%v: internal error: %v", bits))
	}
}

func (p *project) isWCharType(t cc.Type) bool {
	return t.IsAliasType() && t.AliasDeclarator().Name() == idWcharT
}

func isCharType(t cc.Type) bool {
	switch t.Kind() {
	case cc.Char, cc.SChar, cc.UChar:
		return true
	}

	return false
}

func (p *project) initializerFlatten(n *cc.Initializer) (s []*cc.Initializer) {
	switch n.Case {
	case cc.InitializerExpr: // AssignmentExpression
		return append(s, n)
	case cc.InitializerInitList: // '{' InitializerList ',' '}'
		for list := n.InitializerList; list != nil; list = list.InitializerList {
			s = append(s, p.initializerFlatten(list.Initializer)...)
		}
		return s
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}
