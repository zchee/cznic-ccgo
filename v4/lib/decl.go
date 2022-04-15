// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"sort"
	"strings"

	"modernc.org/cc/v4"
)

type declInfo struct {
	addressTaken bool
	bpOff        int64
}

type declInfos map[*cc.Declarator]*declInfo

func (n *declInfos) info(d *cc.Declarator) (r *declInfo) {
	m := *n
	if m == nil {
		m = declInfos{}
		*n = m
	}
	if r = m[d]; r == nil {
		r = &declInfo{}
		m[d] = r
	}
	return r
}

type fnCtx struct {
	declInfos declInfos
	stack     []bool
	t         *cc.FunctionType
	tlsAllocs int64

	maxValist int

	takeAddress bool
}

func newFnCtx(t *cc.FunctionType, n *cc.FunctionDefinition) (r *fnCtx) {
	r = &fnCtx{t: t}
	visit(n, r)
	var a []*cc.Declarator
	for d, n := range r.declInfos {
		if n.addressTaken {
			a = append(a, d)
		}
	}
	sort.Slice(a, func(i, j int) bool {
		x := a[i].NameTok()
		y := a[j].NameTok()
		return x.Seq() < y.Seq()
	})
	for _, d := range a {
		info := r.declInfos[d]
		info.bpOff = roundup(r.tlsAllocs, int64(d.Type().Align()))
		r.tlsAllocs = info.bpOff + d.Type().Size()
	}
	return r
}

func (f *fnCtx) visit(n cc.Node, enter bool) visitor {
	switch {
	case enter:
		switch x := n.(type) {
		case *cc.PostfixExpression:
			switch x.Case {
			case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
				p, ok := x.PostfixExpression.Type().(*cc.PointerType)
				if !ok {
					break
				}

				ft, ok := p.Elem().(*cc.FunctionType)
				if !ok {
					break
				}

				if ft.MaxArgs() >= 0 {
					break
				}

				nargs := 0
				for l := x.ArgumentExpressionList; l != nil; l = l.ArgumentExpressionList {
					nargs++
				}
				if nargs < ft.MinArgs() {
					break
				}

				if nargs > ft.MaxArgs() && ft.MaxArgs() >= 0 {
					break
				}

				if v := nargs - ft.MinArgs(); v > f.maxValist {
					f.maxValist = v
				}
			}
		case *cc.UnaryExpression:
			switch x.Case {
			case cc.UnaryExpressionAddrof: // '&' CastExpression
				f.stack = append(f.stack, f.takeAddress)
				f.takeAddress = true
			}
		case *cc.PrimaryExpression:
			if !f.takeAddress {
				break
			}

			switch x.Case {
			case cc.PrimaryExpressionIdent: // IDENTIFIER
				switch x := x.ResolvedTo().(type) {
				case *cc.Declarator:
					if x.StorageDuration() == cc.Automatic {
						f.declInfos.info(x).addressTaken = true
					}
				}
			}
		}
	default:
		switch x := n.(type) {
		case *cc.UnaryExpression:
			switch x.Case {
			case cc.UnaryExpressionAddrof: // '&' CastExpression
				f.takeAddress = f.stack[len(f.stack)-1]
				f.stack = f.stack[:len(f.stack)-1]
			}
		}
	}
	return f
}

func (c *ctx) externalDeclaration(w writer, n *cc.ExternalDeclaration) {
	w.w("\n")
	switch n.Case {
	case cc.ExternalDeclarationFuncDef: // FunctionDefinition
		c.functionDefinition(w, n.FunctionDefinition)
	case cc.ExternalDeclarationDecl: // Declaration
		c.declaration(w, n.Declaration, true)
	case cc.ExternalDeclarationAsmStmt: // AsmStatement
		//TODO c.err(errorf("TODO %v", n.Case))
	case cc.ExternalDeclarationEmpty: // ';'
		//TODO c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) functionDefinition(w writer, n *cc.FunctionDefinition) {
	d := n.Declarator
	ft, ok := d.Type().(*cc.FunctionType)
	if !ok {
		c.err(errorf("%v: internal error %v", d.Position(), d.Type()))
		return
	}

	f0 := c.f
	c.f = newFnCtx(ft, n)
	defer func() { c.f = f0 }()
	isMain := d.Linkage() == cc.External && d.Name() == "main"
	w.w("\nfunc %s%s%s ", c.declaratorTag(d), d.Name(), c.signature(ft, true, isMain))
	c.compoundStatement(w, n.CompoundStatement, true)
	if isMain && c.task.tlsQualifier != "" { //TODO move to linker
		w.w("\n\nfunc main() { %s%sStart(%smain) }\n", c.task.tlsQualifier, tag(preserve), tag(external))
	}
}

func (c *ctx) signature(f *cc.FunctionType, names, isMain bool) string {
	var b strings.Builder
	fmt.Fprintf(&b, "(%stls *%s%sTLS", tag(ccgoAutomatic), c.task.tlsQualifier, tag(preserve))
	for _, v := range f.Parameters() {
		b.WriteString(", ")
		if names {
			nm := v.Name()
			if nm == "" {
				nm = "_"
			}
			fmt.Fprintf(&b, "%s%s ", tag(automatic), nm)
		}
		b.WriteString(c.typ(v.Type()))
	}
	switch {
	case isMain && len(f.Parameters()) == 0:
		fmt.Fprintf(&b, ", %sargc int32, %[1]sargv uintptr", tag(ccgoAutomatic))
	case isMain && len(f.Parameters()) == 1:
		fmt.Fprintf(&b, ", %sargv uintptr", tag(ccgoAutomatic))
	}
	b.WriteByte(')')
	if f.Result().Kind() != cc.Void {
		b.WriteString(c.typ(f.Result()))
	}
	return b.String()
}

func (c *ctx) declaration(w writer, n *cc.Declaration, external bool) {
	switch n.Case {
	case cc.DeclarationDecl: // DeclarationSpecifiers InitDeclaratorList AttributeSpecifierList ';'
		switch {
		case n.InitDeclaratorList == nil:
			if !external {
				break
			}

			switch x := n.DeclarationSpecifiers.Type().(type) {
			case *cc.EnumType:
				c.defineEnum(w, x)
			case *cc.StructType:
				c.defineStruct(w, x)
			case *cc.UnionType:
				c.defineUnion(w, x)
			}
		default:
			for l := n.InitDeclaratorList; l != nil; l = l.InitDeclaratorList {
				c.initDeclarator(w, l.InitDeclarator, external)
			}
		}
	case cc.DeclarationAssert: // StaticAssertDeclaration
		c.err(errorf("TODO %v", n.Case))
	case cc.DeclarationAuto: // "__auto_type" Declarator '=' Initializer ';'
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) initDeclarator(w writer, n *cc.InitDeclarator, external bool) {
	d := n.Declarator
	if d.Type().Kind() == cc.Function || d.IsExtern() && d.Type().IsIncomplete() {
		return
	}

	if n.Asm != nil {
		w.w("\n//TODO %T %v (%v: )\n// %s // %v:", n, n.Case, origin(1), cc.NodeSource(d), c.pos(n))
		c.err(errorf("TODO asm %v", n.Case))
		return //TODO-
	}

	nm := d.Name()
	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator Asm
		switch {
		case d.IsTypename():
			if external && c.typenames.add(nm) {
				w.w("\ntype %s%s = %s", tag(typename), nm, c.typedef(d.Type()))
			}
		default:
			if d.IsExtern() {
				return
			}

			c.defineEnumStructUnion(w, d.Type())
			w.w("\nvar %s%s %s", c.declaratorTag(d), nm, c.typ(d.Type()))
		}
	case cc.InitDeclaratorInit: // Declarator Asm '=' Initializer
		c.defineEnumStructUnion(w, d.Type())
		switch {
		case d.Linkage() == cc.Internal:
			w.w("\nvar %s%s = %s", c.declaratorTag(d), nm, c.initializer(w, n.Initializer, d.Type()))
		case d.IsStatic():
			w.w("\nvar %s%s = %s", c.declaratorTag(d), nm, c.initializer(w, n.Initializer, d.Type()))
		default:
			w.w("\n%s%s := %s", c.declaratorTag(d), nm, c.initializer(w, n.Initializer, d.Type()))
		}

	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	w.w(" // %v:", c.pos(d))
}
