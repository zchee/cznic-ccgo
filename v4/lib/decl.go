// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"strings"

	"modernc.org/cc/v4"
)

type fctx struct {
	maxValist int
	t         *cc.FunctionType
	tlsAllocs int
}

func newFctx(t *cc.FunctionType, n *cc.FunctionDefinition) (r *fctx) {
	r = &fctx{t: t}
	visit(n, r.visitor)
	return r
}

func (c *fctx) visitor(n cc.Node) bool {
	switch x := n.(type) {
	case *cc.PostfixExpression:
		switch x.Case {
		case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
			ft := x.PostfixExpression.Type().(*cc.PointerType).Elem().(*cc.FunctionType)
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

			if v := nargs - ft.MinArgs(); v > c.maxValist {
				c.maxValist = v
			}
		}
	}
	return true
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
	c.f = newFctx(ft, n)
	defer func() { c.f = f0 }()
	isMain := d.Linkage() == cc.External && d.Name() == "main"
	w.w("\nfunc %s%s%s ", c.declaratorTag(d), d.Name(), c.signature(ft, true, isMain))
	c.compoundStatement(w, n.CompoundStatement, true)
	if isMain && c.task.tlsQualifier != "" { //TODO move to linker
		w.w("\n\nfunc main() { %s%sStart(%smain) }\n", c.task.tlsQualifier, tag(preserve), tag(external))
	}
}

func (c *ctx) compoundStatement(w writer, n *cc.CompoundStatement, fnBlock bool) {
	w.w(" { // %v:", c.pos(n))
	if fnBlock && c.f.tlsAllocs+c.f.maxValist != 0 {
		w.w("\n%sbp := %[1]stls.Alloc(%d)", tag(ccgoAutomatic), c.f.tlsAllocs+8*(c.f.maxValist+1))
	}
	for l := n.BlockItemList; l != nil; l = l.BlockItemList {
		c.blockItem(w, l.BlockItem)
	}
	w.w("\n}")
}

func (c *ctx) blockItem(w writer, n *cc.BlockItem) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		c.declaration(w, n.Declaration, false)
	case cc.BlockItemLabel: // LabelDeclaration
		c.err(errorf("TODO %v", n.Case))
	case cc.BlockItemStmt: // Statement
		c.statement(w, n.Statement)
	case cc.BlockItemFuncDef: // DeclarationSpecifiers Declarator CompoundStatement
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
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
