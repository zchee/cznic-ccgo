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

	read  int
	sd    cc.StorageDuration
	write int
}

func (n *declInfo) escapes() bool { return n.sd == cc.Automatic && n.addressTaken }

type declInfos map[*cc.Declarator]*declInfo

func (n *declInfos) info(d *cc.Declarator) (r *declInfo) {
	m := *n
	if m == nil {
		m = declInfos{}
		*n = m
	}
	if r = m[d]; r == nil {
		r = &declInfo{sd: d.StorageDuration()}
		m[d] = r
	}
	return r
}

func (n *declInfos) read(d *cc.Declarator)        { n.info(d).read++ }
func (n *declInfos) takeAddress(d *cc.Declarator) { n.info(d).addressTaken = true }
func (n *declInfos) write(d *cc.Declarator)       { n.info(d).write++ }

type fnCtx struct {
	c         *ctx
	declInfos declInfos
	stack     []bool
	t         *cc.FunctionType
	tlsAllocs int64

	maxValist int
	nextID    int
	read      int
	write     int

	takeAddress bool
}

func (c *ctx) newFnCtx(t *cc.FunctionType, n *cc.FunctionDefinition) (r *fnCtx) {
	r = &fnCtx{c: c, t: t, read: 1}
	walk(n, r)
	var a []*cc.Declarator
	for d, n := range r.declInfos {
		if n.escapes() {
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

func (f *fnCtx) id() int { f.nextID++; return f.nextID }

func (f *fnCtx) visit(n cc.Node, enter bool) visitor {
	switch {
	case enter:
		switch x := n.(type) {
		case *cc.AssignmentExpression:
			switch x.Case {
			case cc.AssignmentExpressionCond: // ConditionalExpression
				walk(x.ConditionalExpression, f)
			default:
				f.write++
				f.read--
				walk(x.UnaryExpression, f)
				f.read++
				f.write--
				walk(x.AssignmentExpression, f)
			}
			return nil
		case *cc.Declaration:
			switch x.Case {
			case cc.DeclarationAuto:
				f.declInfos.write(x.Declarator)
				walk(x.Initializer, f)
			default:
				walk(x.InitDeclaratorList, f)
			}
			return nil
		case *cc.InitDeclarator:
			switch x.Case {
			case cc.InitDeclaratorInit:
				f.declInfos.write(x.Declarator)
				walk(x.Initializer, f)
			}
			return nil
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

				if v := nargs - ft.MinArgs(); ft.IsVariadic() && v > f.maxValist {
					f.maxValist = v
				}
			case cc.PostfixExpressionInc, cc.PostfixExpressionDec:
				f.write++
				f.read++
				walk(x.PostfixExpression, f)
				f.read--
				f.write--
				return nil
			}
		case *cc.PrimaryExpression:
			switch x.Case {
			case cc.PrimaryExpressionIdent: // IDENTIFIER
				switch y := x.ResolvedTo().(type) {
				case *cc.Declarator:
					if f.takeAddress {
						f.declInfos.takeAddress(y)
					}
					if f.read != 0 {
						f.declInfos.read(y)
					}
					if f.write != 0 {
						f.declInfos.write(y)
					}
				}
			}
		case *cc.UnaryExpression:
			switch x.Case {
			case cc.UnaryExpressionAddrof: // '&' CastExpression
				f.stack = append(f.stack, f.takeAddress)
				f.takeAddress = true
			case cc.UnaryExpressionInc, cc.UnaryExpressionDec:
				f.write++
				f.read++
				walk(x.UnaryExpression, f)
				f.read--
				f.write--
				return nil
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
	c.f = c.newFnCtx(ft, n)
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
	switch {
	case names:
		fmt.Fprintf(&b, "(%stls *%s%sTLS", tag(ccgoAutomatic), c.task.tlsQualifier, tag(preserve))
	default:
		fmt.Fprintf(&b, "(*%s%sTLS", c.task.tlsQualifier, tag(preserve))
	}
	if f.MaxArgs() != 0 {
		for _, v := range f.Parameters() {
			b.WriteString(", ")
			if names {
				switch nm := v.Name(); {
				case nm == "":
					fmt.Fprintf(&b, "%sp ", tag(ccgoAutomatic))
				default:
					fmt.Fprintf(&b, "%s%s ", tag(automatic), nm)
				}
			}
			b.WriteString(c.typ(v.Type()))
		}
	}
	switch {
	case isMain && len(f.Parameters()) == 0:
		fmt.Fprintf(&b, ", %sargc int32, %[1]sargv uintptr", tag(ccgoAutomatic))
	case isMain && len(f.Parameters()) == 1:
		fmt.Fprintf(&b, ", %sargv uintptr", tag(ccgoAutomatic))
	case f.IsVariadic():
		switch {
		case names:
			fmt.Fprintf(&b, ", %sva uintptr", tag(ccgoAutomatic))
		default:
			fmt.Fprintf(&b, ", uintptr")
		}
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

	var info *declInfo
	if c.f != nil {
		info = c.f.declInfos.info(d)
	}
	nm := d.Name()
	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator Asm
		switch {
		case d.IsTypename():
			if external && c.typenames.add(nm) {
				w.w("\ntype %s%s = %s", tag(typename), nm, c.typedef(d.Type()))
			}
			if !external {
				return
			}
		default:
			if d.IsExtern() {
				return
			}

			c.defineEnumStructUnion(w, d.Type())
			s := ""
			if info != nil && info.escapes() {
				s = "// "
			}
			w.w("\n%svar %s%s %s", s, c.declaratorTag(d), nm, c.typ(d.Type()))
		}
	case cc.InitDeclaratorInit: // Declarator Asm '=' Initializer
		c.defineEnumStructUnion(w, d.Type())
		switch {
		case d.Linkage() == cc.Internal:
			w.w("\nvar %s%s = %s", c.declaratorTag(d), nm, c.initializerOuter(w, n.Initializer, d.Type()))
		case d.IsStatic():
			w.w("\nvar %s%s = %s", c.declaratorTag(d), nm, c.initializerOuter(w, n.Initializer, d.Type()))
		default:
			switch {
			case info != nil && info.escapes():
				w.w("\n*(*%s)(unsafe.Pointer(%s)) = %s", c.typ(d.Type()), bpOff(info.bpOff), c.initializerOuter(w, n.Initializer, d.Type()))
			default:
				switch {
				case d.LexicalScope().Parent == nil:
					w.w("\nvar %s%s = %s", c.declaratorTag(d), nm, c.initializerOuter(w, n.Initializer, d.Type()))
				default:
					w.w("\n%s%s := %s", c.declaratorTag(d), nm, c.initializerOuter(w, n.Initializer, d.Type()))
				}
			}
		}

	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
	w.w(" // %v:", c.pos(d))
	if info != nil {
		w.w(" read: %d, write: %d, escapes %v", info.read, info.write, info.escapes()) //TODO-
		if d.StorageDuration() == cc.Automatic && info.read == 0 && !info.escapes() {
			w.w("\n_ = %s%s", c.declaratorTag(d), nm)
		}
	}
}
