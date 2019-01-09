// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v2"

import (
	"fmt"
	"io"
	"path/filepath"

	"modernc.org/cc/v2"
	"modernc.org/ir"
)

func (g *gen) defineQueued() {
	for g.queue.Front() != nil {
		x := g.queue.Front()
		g.queue.Remove(x)
		func() {
			defer func() {
				e := recover()
				var b []byte
				if logging {
					b = g.out0.Bytes()
				}
				if err := newOpt().do(g.out, &g.out0, testFn); e != nil || err != nil {
					todo("recover: %v, err: %v\nsrc:\n====%s\n----\n%s", e, err, b, debugStack0())
				}

				g.out0.Reset()
			}()

			switch y := x.Value.(type) {
			case *cc.Declarator:
				g.tld(y)
			case *cc.EnumType:
				g.defineEnumType(y)
			case *cc.NamedType:
				g.defineNamedType(y)
			case *cc.TaggedEnumType:
				g.defineTaggedEnumType(y)
			case *cc.TaggedStructType:
				g.defineTaggedStructType(y)
			case *cc.TaggedUnionType:
				g.defineTaggedUnionType(y)
			case
				*cc.ArrayType,
				*cc.FunctionType,
				*cc.PointerType,
				*cc.StructType,
				*cc.UnionType,
				cc.TypeKind:

				// nop
			default:
				todo("%T", y)
			}
		}()
	}
}

func (g *gen) defineNamedType(t *cc.NamedType) {
	if _, ok := g.producedNamedTypes[t.Name]; ok {
		return
	}

	g.producedNamedTypes[t.Name] = struct{}{}
	if t.Type == nil {
		todo("", t)
	}

	switch {
	case t.Name == idLS:
		g.w("\ntype N%s = %s", dict.S(t.Name), g.typ(t.Type))
	default:
		g.w("\ntype T%s = %s", dict.S(t.Name), g.typ(t.Type))
	}
	if t.Type.Kind() == cc.Ptr {
		g.w("// %s", g.typeComment(t))
	}
	g.w("\n")
	g.enqueue(t.Type)
	if !g.tweaks.StructChecks {
		return
	}

	switch x := t.Type.(type) {
	case *cc.StructType:
		g.w("\n\nfunc init() {")
		g.w("\nvar z %s", g.typ(x))
		fields := x.Fields
		for i, v := range g.model.Layout(x) {
			if v.Bits < 0 {
				continue
			}

			if v.Bits != 0 && v.Bitoff != 0 {
				continue
			}

			if v.Bits != 0 && v.Bitoff == 0 {
				g.w("\nif n := unsafe.Offsetof(z.F%d); n != %d { panic(n) }", v.Offset, v.Offset)
				g.w("\nif n := unsafe.Sizeof(z.F%d); n != %d { panic(n) }", v.Offset, g.model.Sizeof(v.PackedType))
				continue
			}

			if fields[i].Name == 0 {
				continue
			}

			g.w("\nif n := unsafe.Offsetof(z.F%s); n != %d { panic(n) }", dict.S(fields[i].Name), v.Offset)
			g.w("\nif n := unsafe.Sizeof(z.F%s); n != %d { panic(n) }", dict.S(fields[i].Name), v.Size)
		}
		g.w("\nif n := unsafe.Sizeof(z); n != %d { panic(n) }", g.model.Sizeof(t))
		g.w("\n}\n")
	}
}

func (g *gen) defineEnumType(t *cc.EnumType) {
	if t.Tag != 0 {
		g.defineTaggedEnumType(&cc.TaggedEnumType{Tag: t.Tag, Type: t})
	}
}

func (g *gen) defineTaggedEnumType(t *cc.TaggedEnumType) {
	if _, ok := g.producedEnumTags[t.Tag]; ok {
		return
	}

	g.producedEnumTags[t.Tag] = struct{}{}
	et, ok := t.Type.(*cc.EnumType)
	if !ok {
		return
	}

	tag := dict.S(t.Tag)
	g.w("\ntype E%s = %s\n", tag, g.typ(et.Enums[0].Operand.Type))
	g.w("\n// Values of E%s\nconst (", tag)
	for _, v := range et.Enums {
		nm := v.Token.Val
		if _, ok := g.enumConsts[nm]; ok {
			continue
		}

		g.enumConsts[nm] = struct{}{}
		val := v.Operand.Value.(*ir.Int64Value).Value
		g.w("\nC%s = %d", dict.S(v.Token.Val), val)
	}
	g.w("\n)\n")
}

func (g *gen) defineTaggedStructType(t *cc.TaggedStructType) {
	if _, ok := g.producedStructTags[t.Tag]; ok {
		return
	}

	switch {
	case t.Type == nil:
		g.opaqueStructTags[t.Tag] = struct{}{}
	default:
		g.producedStructTags[t.Tag] = struct{}{}
		g.w("\ntype S%s = %s\n", dict.S(t.Tag), g.typ(t.Type))
		if g.tweaks.StructChecks || isTesting {
			g.w("\n\nfunc init() { // S%s", dict.S(t.Tag))
			st := cc.UnderlyingType(t.Type).(*cc.StructType)
			fields := st.Fields
			for i, v := range g.model.Layout(st) {
				if v.Bits < 0 {
					continue
				}

				if v.Bits != 0 && v.Bitoff != 0 {
					continue
				}

				if v.Bits != 0 && v.Bitoff == 0 {
					g.w("\nif n := unsafe.Offsetof(S%s{}.F%d); n != %d { panic(n) }", dict.S(t.Tag), v.Offset, v.Offset)
					g.w("\nif n := unsafe.Sizeof(S%s{}.F%d); n != %d { panic(n) }", dict.S(t.Tag), v.Offset, g.model.Sizeof(v.PackedType))
					continue
				}

				if fields[i].Name == 0 {
					continue
				}

				g.w("\nif n := unsafe.Offsetof(S%s{}.F%s); n != %d { panic(n) }", dict.S(t.Tag), dict.S(fields[i].Name), v.Offset)
				g.w("\nif n := unsafe.Sizeof(S%s{}.F%s); n != %d { panic(n) }", dict.S(t.Tag), dict.S(fields[i].Name), v.Size)
			}
			g.w("\nif n := unsafe.Sizeof(S%s{}); n != %d { panic(n) }", dict.S(t.Tag), g.model.Sizeof(t))
			g.w("\n}\n")
		}
	}
}

func (g *gen) defineTaggedUnionType(t *cc.TaggedUnionType) {
	if _, ok := g.producedStructTags[t.Tag]; ok {
		return
	}

	g.producedStructTags[t.Tag] = struct{}{}
	g.w("\ntype U%s = %s\n", dict.S(t.Tag), g.typ(t.Type))
	if g.tweaks.StructChecks || isTesting {
		g.w("\n\nfunc init() { // U%s", dict.S(t.Tag))
		g.w("\nif n := unsafe.Sizeof(U%s{}); n != %d { panic(n) }", dict.S(t.Tag), g.model.Sizeof(t))
		g.w("\n}\n")
	}
}

func (g *gen) tld(n *cc.Declarator) {
	n = g.normalizeDeclarator(n)
	mn := g.mangleDeclarator(n)
	if _, ok := g.producedTLDs[mn]; ok {
		return
	}

	defer func() {
		if e := recover(); e != nil {
			panic(fmt.Errorf("%s\n%s", e, debugStack()))
		}

		var b []byte
		if logging {
			b = g.out0.Bytes()
		}
		if err := newOpt().do(g.out, io.MultiReader(&g.tldPreamble, &g.out0), testFn); err != nil {
			todo("%s\n====%s\n---\n", err, b)
		}

		g.tldPreamble.Reset()
		g.out0.Reset()
	}()

	t := cc.UnderlyingType(n.Type)
	if t.Kind() == cc.Function {
		g.functionDefinition(n)
		return
	}

	ds := n.DeclarationSpecifier
	g.linkInfo(n, ds.IsExtern())
	if ds.IsExtern() {
		return
	}

	if ds.IsStatic() {
		if n.Referenced == 0 && !n.AddressTaken {
			return
		}
	}

	g.producedTLDs[mn] = struct{}{}
	pos := g.position(n)
	pos.Filename, _ = filepath.Abs(pos.Filename)
	if !isTesting && !g.tweaks.FullTLDPaths {
		pos.Filename = filepath.Base(pos.Filename)
	}
	g.w("\n\n// %s %s, escapes: %v, %v", mn, g.typeComment(n.Type), g.escaped(n), pos)

	arr, esc, vla, param := g.isArray(n)
	switch {
	case
		!arr,
		arr && esc && !vla && !param:

		// nop
	default:
		todo("", g.position(n), arr, esc, vla, param)
		return
	}

	if g.isZeroInitializer(n.Initializer) {
		if g.escaped(n) {
			g.w("\nvar %s = Lb + %d", mn, g.model.Sizeof(n.Type))
			return
		}

		switch t.(type) {
		case *cc.PointerType:
			g.w("\nvar %s uintptr\n", mn)
		default:
			g.w("\nvar %s %s\n", mn, g.typ(n.Type))
		}
		return
	}

	if g.escaped(n) {
		g.escapedTLD(n)
		return
	}

	switch n.Initializer.Case {
	case cc.InitializerExpr: // Expr
		switch cc.UnderlyingType(n.Type).(type) {
		case *cc.PointerType:
			g.w("\nvar %s %s\n", mn, g.typ(n.Type))
			g.w("\nfunc init() { %s = ", mn)
			g.convert(n.Initializer.Expr, n.Type)
			g.w(" }\n")
		default:
			g.w("\nvar %s = ", mn)
			g.convert(n.Initializer.Expr, n.Type)
			g.w("\n")
		}
	default:
		todo("", g.position(n), n.Initializer.Case)
	}
}

func (g *gen) linkInfo(n *cc.Declarator, declarationOnly bool) {
	if n.Linkage != cc.LinkageExternal {
		return
	}

	mn := g.mangleDeclarator(n)
	switch {
	case declarationOnly:
		g.w("\n\n%se%s = %q", lConstPrefix, mn, g.typ(n.Type))
	default:
		g.w("\n\n%sd%s = %q", lConstPrefix, mn, g.typ(n.Type))
	}
	for _, v := range n.Attributes {
		if len(v) == 0 {
			continue
		}

		switch t := v[0]; t.Rune {
		case cc.IDENTIFIER:
			switch t.Val {
			case idAlias:
				if len(v) != 2 {
					todo("", g.position(n), cc.PrettyString(v))
				}

				switch t := v[1]; t.Rune {
				case cc.STRINGLITERAL:
					nm2 := dict.S(t.Val)
					nm2 = nm2[1 : len(nm2)-1]
					id2 := dict.ID(nm2)
					switch n2 := n.Scope.LookupIdent(id2).(type) {
					case *cc.Declarator:
						g.enqueue(n2)
						switch n2.Linkage {
						case cc.LinkageInternal:
							g.w("\n\n%sb%s = %q", lConstPrefix, mn, g.mangleDeclarator(n2))
						case cc.LinkageExternal:
							g.w("\n\n%sa%s = %q", lConstPrefix, mn, g.mangleDeclarator(n2))
						default:
							todo("%v: %q %v", g.position(n2), nm2, n2.Linkage)
						}
					default:
						todo("%v: %q %T", g.position(n), nm2, n2)
					}
				default:
					todo("", g.position(n), cc.PrettyString(v))
				}
			case idVisibility, idVisibility2:
				if len(v) != 2 {
					todo("", g.position(n), cc.PrettyString(v))
				}

				switch t := v[1]; t.Rune {
				case cc.STRINGLITERAL:
					g.w("\n\n%sv%s = %s", lConstPrefix, mn, dict.S(t.Val))
				default:
					todo("", g.position(n), cc.PrettyString(v))
				}
			case idWeak, idWeak2:
				if len(v) != 1 {
					todo("", g.position(n), cc.PrettyString(v))
				}
				g.w("\n\n%sw%s = %q", lConstPrefix, mn, "")
			case
				idAligned, //TODO? 990326-1.c
				idPure,
				idStdcall,
				idConst,
				idNoClone,
				idNoInline,
				idNoInline2,
				idNoReturn,
				idNoReturn2:

				if len(v) != 1 {
					todo("", g.position(n), cc.PrettyString(v))
				}
				// ignored
			default:
				todo("%v: %q,  %q %v", g.position(n), mn, dict.S(t.Val), cc.PrettyString(v))
			}
		default:
			todo("", g.position(n), cc.PrettyString(v))
		}
	}
}

func (g *gen) escapedTLD(n *cc.Declarator) {
	if g.isConstInitializer(n.Type, n.Initializer) {
		g.w("\nvar %s = Ld + %q\n", g.mangleDeclarator(n), g.allocDS(n.Type, n.Initializer))
		return
	}

	switch x := cc.UnderlyingType(n.Type).(type) {
	case *cc.ArrayType:
		if x.Item.Kind() == cc.Char && n.Initializer.Expr.Operand.Value != nil {
			g.w("\nvar %s = Ld + %d\n", g.mangleDeclarator(n), g.allocDS(n.Type, n.Initializer))
			return
		}
	}

	g.w("\nvar %s = Lb +%d // %v \n", g.mangleDeclarator(n), g.model.Sizeof(n.Type), n.Type)
	g.w("\n\nfunc init() { *(*%s)(unsafe.Pointer(%s)) = ", g.typ(n.Type), g.mangleDeclarator(n))
	g.literal(n.Type, n.Initializer)
	g.w("}")
}

func (g *gen) functionDefinition(n *cc.Declarator) {
	main := fixMain(n)
	g.nextLabel = 1
	pos := g.position(n)
	pos.Filename, _ = filepath.Abs(pos.Filename)
	if !isTesting && !g.tweaks.FullTLDPaths {
		pos.Filename = filepath.Base(pos.Filename)
	}
	g.linkInfo(n, n.FunctionDefinition == nil)
	if n.FunctionDefinition == nil {
		return
	}

	mn := g.mangleDeclarator(n)
	if _, ok := g.producedTLDs[mn]; ok {
		return
	}

	g.producedTLDs[mn] = struct{}{}
	g.w("\n\n// %s is defined at %v", mn, pos)
	g.w("\nfunc %s(tls %sTLS", mn, g.crtPrefix)
	names := n.ParameterNames()
	t := n.Type.(*cc.FunctionType)
	if len(names) != len(t.Params) {
		if len(names) != 0 {
			if !(len(names) == 1 && names[0] == 0) {
				todo("K&R C %v %v %v", g.position(n), names, t.Params)
			}
		}

		names = make([]int, len(t.Params))
	}
	params := n.Parameters
	var escParams []*cc.Declarator
	switch {
	case len(t.Params) == 1 && t.Params[0].Kind() == cc.Void:
		// nop
	default:
		for i, v := range t.Params {
			var param *cc.Declarator
			if i < len(params) {
				param = params[i]
			}
			nm := names[i]
			g.w(", ")
			switch {
			case param != nil && g.escaped(param):
				g.w("a%s %s", dict.S(nm), g.flattenParam(v))
				escParams = append(escParams, param)
			default:
				switch cc.UnderlyingType(v).(type) {
				case *cc.ArrayType:
					g.w("%s uintptr /* %v */ ", mangleIdent(nm, false), g.typ(v))
				default:
					g.w("%s %s ", mangleIdent(nm, false), g.flattenParam(v))
				}
				if isVaList(v) {
					continue
				}

				if v.Kind() == cc.Ptr {
					g.w("/* %s */", g.typeComment(v))
				}
			}
		}
		if t.Variadic {
			g.w(", %s...interface{}", ap)
		}
	}
	g.w(")")
	void := t.Result.Kind() == cc.Void
	//dbg("", g.position(n), string(dict.S(n.Name())), void)
	if !void {
		g.w("(r %s", g.flattenParam(t.Result))
		if t.Result.Kind() == cc.Ptr {
			g.w("/* %s */", g.typeComment(t.Result))
		}
		g.w(")")
	}
	vars := n.FunctionDefinition.LocalVariables()
	if n.Alloca {
		vars = append(append([]*cc.Declarator(nil), vars...), allocaDeclarator)
	}
	g.functionBody(n.FunctionDefinition.FunctionBody, vars, void, n.Parameters, escParams, main)
	g.w("\n")
}

func (g *gen) flattenParam(t cc.Type) string {
	if isVaList(t) {
		return g.typ(t)
	}

	return g.typ(cc.UnderlyingType(t))
}

func (g *gen) functionBody(n *cc.FunctionBody, vars []*cc.Declarator, void bool, params, escParams []*cc.Declarator, main bool) {
	if vars == nil {
		vars = []*cc.Declarator{}
	}
	g.compoundStmt(n.CompoundStmt, vars, nil, !void, nil, nil, params, escParams, main, false)
}

func (g *gen) mangleDeclarator(n *cc.Declarator) string {
	nm := n.Name()
	if n.Linkage == cc.LinkageExternal {
		return mangleIdent(nm, true)
	}

	if n.Linkage == cc.LinkageInternal {
		return fmt.Sprintf("x%s", dict.S(nm))
	}

	if num, ok := g.nums[n]; ok {
		switch {
		case n.DeclarationSpecifier.IsStatic():
			return fmt.Sprintf("x%d%s", num, dict.S(nm))
		default:
			return fmt.Sprintf("_%d%s", num, dict.S(nm))
		}
	}

	if n.IsField {
		return mangleIdent(nm, true)
	}

	return mangleIdent(nm, false)
}

func (g *gen) normalizeDeclarator(n *cc.Declarator) *cc.Declarator {
	if n == nil {
		return nil
	}

	if n.Definition != nil {
		return n.Definition
	}

	return n
}

func (g *gen) declaration(n *cc.Declaration, inline bool) {
	// DeclarationSpecifiers InitDeclaratorListOpt ';'
	g.initDeclaratorListOpt(n.InitDeclaratorListOpt, inline)
}

func (g *gen) initDeclaratorListOpt(n *cc.InitDeclaratorListOpt, inline bool) {
	if n == nil {
		return
	}

	g.initDeclaratorList(n.InitDeclaratorList, inline)
}

func (g *gen) initDeclaratorList(n *cc.InitDeclaratorList, inline bool) {
	first := true
	for ; n != nil; n = n.InitDeclaratorList {
		g.initDeclarator(n.InitDeclarator, inline && first)
		first = false
	}
}

func (g *gen) initDeclarator(n *cc.InitDeclarator, inline bool) {
	d := n.Declarator
	ds := d.DeclarationSpecifier
	if ds.IsExtern() {
		return
	}

	if ds.IsStatic() {
		g.enqueue(d)
		return
	}

	if x, ok := d.Type.(*cc.ArrayType); ok && x.IsVLA() {
		g.w("\n/*TODO initDeclarator: %s is VLA */", dict.S(d.Name()))
		return
	}

	if n.Case == cc.InitDeclaratorInit { // Declarator '=' Initializer
		g.initializer(d, inline)
	}
}

func errs(out, in error) error {
	if out == nil {
		out = in
	}
	return out
}
