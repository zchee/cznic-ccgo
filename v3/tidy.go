// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

//TODO s/break;fallthrough//

import (
	"go/ast"
	"go/token"
)

type tidy struct {
	g    *gen
	fset *token.FileSet
	used map[string]struct{}
}

func newTidy(g *gen, fset *token.FileSet) *tidy { return &tidy{g: g, fset: fset} }

func (t *tidy) pos(n ast.Node) token.Position {
	if n == nil {
		return token.Position{}
	}

	return t.fset.Position(n.Pos())
}

func (t *tidy) file(n *ast.File) {
	for i := range n.Decls {
		t.decl(&n.Decls[i])
	}
}

func (t *tidy) decl(n *ast.Decl) {
	switch x := (*n).(type) {
	case *ast.FuncDecl:
		t.used = map[string]struct{}{}
		t.blockStmt(x.Body, true)
		t.used = nil
	case *ast.GenDecl:
		for i := range x.Specs {
			t.spec(&x.Specs[i])
		}
	default:
		todo("%v: %T", t.pos(x), x)
	}
}

func (t *tidy) spec(n *ast.Spec) {
	switch x := (*n).(type) {
	case *ast.TypeSpec:
		// nop
	case *ast.ValueSpec:
		trackUsed := x.Names[0].Name != "_"
		for i := range x.Values {
			t.expr(&x.Values[i], trackUsed, true)
		}
	default:
		todo("%v: %T", t.pos(x), x)
	}
}

func (t *tidy) blockStmt(n *ast.BlockStmt, outermost bool) {
	if n == nil {
		return
	}

	t.body(&n.List)
	if !outermost {
		return
	}

	w := 0
	for _, v := range n.List {
		switch x := v.(type) {
		case *ast.AssignStmt:
			if x2, ok := x.Lhs[0].(*ast.Ident); ok && x2.Name == "_" {
				if _, used := t.used[x.Rhs[0].(*ast.Ident).Name]; used {
					continue
				}
			}
		case *ast.DeclStmt:
			switch x2 := x.Decl.(type) {
			case *ast.GenDecl:
				w := 0
				for _, v := range x2.Specs {
					if x3, ok := v.(*ast.ValueSpec); ok && x3.Names[0].Name == "_" {
						if _, used := t.used[x3.Values[0].(*ast.Ident).Name]; used {
							continue
						}
					}

					x2.Specs[w] = v
					w++
				}
				x2.Specs = x2.Specs[:w]
			}
		}
		n.List[w] = v
		w++
	}
	n.List = n.List[:w]
}

func (t *tidy) body(l0 *[]ast.Stmt) {
	l := *l0
	w := 0
	for i := range l {
		t.stmt(&l[i])
		switch x := l[i].(type) {
		case *ast.EmptyStmt:
			// nop
		default:
			l[w] = x
			w++
		}
	}
	*l0 = l[:w]
}

func (t *tidy) stmt(n *ast.Stmt) {
	switch x := (*n).(type) {
	case nil:
		// nop
	case *ast.AssignStmt:
		for i := range x.Lhs {
			t.expr(&x.Lhs[i], false, true)
		}
		trackUsed := true
		if x, ok := x.Lhs[0].(*ast.Ident); ok && x.Name == "_" {
			trackUsed = false
		}
		for i := range x.Rhs {
			t.expr(&x.Rhs[i], trackUsed, true)
		}
	case *ast.BlockStmt:
		t.blockStmt(x, false)
	case *ast.BranchStmt:
		// nop
	case *ast.CaseClause:
		for i := range x.List {
			t.expr(&x.List[i], true, true)
		}
		t.body(&x.Body)
	case *ast.DeclStmt:
		t.decl(&x.Decl)
	case *ast.DeferStmt:
		t.call(x.Call, true)
	case *ast.EmptyStmt:
		// nop
	case *ast.ExprStmt:
		t.expr(&x.X, false, true)
	case *ast.ForStmt:
		t.stmt(&x.Init)
		t.expr(&x.Cond, true, true)
		t.stmt(&x.Post)
		t.blockStmt(x.Body, false)
	case *ast.IfStmt:
		t.stmt(&x.Init)
		t.expr(&x.Cond, true, true)
		t.blockStmt(x.Body, false)
		t.stmt(&x.Else)
		switch xe := x.Else.(type) {
		case *ast.BlockStmt:
			if len(xe.List) == 0 {
				x.Else = nil
				break
			}

			if len(xe.List) == 1 {
				if x0, ok := xe.List[0].(*ast.IfStmt); ok {
					x.Else = x0
				}
			}
		case *ast.EmptyStmt:
			x.Else = nil
		}
		if len(x.Body.List) == 0 && x.Else != nil {
			// Turn
			//	if cond {} else { stmtList }
			// into
			//	if !cond { stmtList }
			switch y := x.Else.(type) {
			case *ast.BlockStmt:
				x.Cond = t.not(x.Cond)
				x.Body.List = y.List
				x.Else = nil
			case *ast.IfStmt:
				//TODO if cond {} else if cond2 { ... } -> if !cond && cond2 { ... }
			default:
				todo("%T", y)
			}
		}
	case *ast.IncDecStmt:
		t.expr(&x.X, true, true)
	case *ast.LabeledStmt:
		t.stmt(&x.Stmt)
	case *ast.RangeStmt:
		t.expr(&x.Key, false, true)
		t.expr(&x.Value, false, true)
		t.expr(&x.X, true, true)
		t.blockStmt(x.Body, false)
	case *ast.ReturnStmt:
		for i := range x.Results {
			t.expr(&x.Results[i], true, true)
		}
	case *ast.SwitchStmt:
		t.stmt(&x.Init)
		t.expr(&x.Tag, true, true)
		t.blockStmt(x.Body, false)
	default:
		todo("%v: %T", t.pos(x), x)
	}
}

func (t *tidy) expr(n *ast.Expr, trackUsed, outermost bool) {
	switch x := (*n).(type) {
	case *ast.ArrayType:
		t.expr(&x.Len, false, true)
		t.expr(&x.Elt, false, true)
	case *ast.BasicLit:
		// nop
	case *ast.BinaryExpr:
		t.expr(&x.X, true, false)
		t.expr(&x.Y, true, false)
		switch x.Op {
		case token.SHL, token.SHR:
			switch rhs := x.Y.(type) {
			case *ast.BasicLit:
				if rhs.Value == "0" {
					*n = x.X
					return
				}
			}
		}
		switch rhs := x.Y.(type) {
		case *ast.BasicLit:
			switch x.Op {
			case token.ADD, token.SUB:
				if rhs.Value == "0" {
					*n = x.X
					return
				}
			case token.MUL, token.QUO:
				if rhs.Value == "1" {
					*n = x.X
					return
				}
			}
		}
		switch lhs := x.X.(type) {
		case *ast.BasicLit:
			switch x.Op {
			case token.ADD, token.SUB:
				if lhs.Value == "0" {
					*n = x.Y
					return
				}
			case token.MUL, token.QUO:
				if lhs.Value == "1" {
					*n = x.Y
					return
				}
			}
		case *ast.CallExpr:
			switch x2 := lhs.Fun.(type) {
			case *ast.SelectorExpr:
				y, ok := x2.X.(*ast.Ident)
				if !ok || y.Name != t.g.task.crt[:len(t.g.task.crt)-1] || x2.Sel.Name != "Bool32" {
					break
				}

				switch x.Op {
				case token.EQL:
					switch rhs := x.Y.(type) {
					case *ast.BasicLit:
						if rhs.Value == "0" {
							*n = t.not(lhs.Args[0])
							return
						}
					}
				case token.NEQ:
					switch rhs := x.Y.(type) {
					case *ast.BasicLit:
						if rhs.Value == "0" {
							*n = lhs.Args[0]
							return
						}
					}
				}
			}
		}
	case *ast.CallExpr:
		t.call(x, outermost)
	case *ast.FuncLit:
		t.body(&x.Body.List)
	case *ast.Ident:
		if trackUsed && t.used != nil {
			t.used[x.Name] = struct{}{}
		}
	case *ast.IndexExpr:
		t.expr(&x.X, true, false)
		t.expr(&x.Index, true, true)
	case *ast.ParenExpr:
		if outermost {
			*n = x.X
			t.expr(&x.X, trackUsed, outermost)
			return
		}

		t.expr(&x.X, trackUsed, false)
		switch x2 := x.X.(type) {
		case *ast.BasicLit:
			*n = x2
		case *ast.CallExpr:
			*n = x2
		case *ast.Ident:
			*n = x2
		case *ast.ParenExpr:
			*n = x2.X
		case *ast.SelectorExpr:
			switch x2.X.(type) {
			case *ast.Ident:
				*n = x2
			}
		case *ast.StarExpr:
			*n = x2
		case *ast.UnaryExpr:
			switch x2.Op {
			case token.AND:
				switch x2.X.(type) {
				case
					*ast.Ident,
					*ast.SelectorExpr:

					*n = x2
				}
			}
		}
	case *ast.SelectorExpr:
		t.expr(&x.X, true, true)
	case *ast.StarExpr:
		t.expr(&x.X, trackUsed, true)
		switch x2 := x.X.(type) {
		case *ast.ParenExpr:
			switch x3 := x2.X.(type) {
			case *ast.UnaryExpr:
				switch x3.Op {
				case token.AND:
					*n = x3.X
				}
			}
		case *ast.UnaryExpr:
			switch x2.Op {
			case token.AND:
				*n = x2.X
			}
		}
	case
		*ast.FuncType,
		*ast.StructType:

		// nop
	case *ast.UnaryExpr:
		t.expr(&x.X, trackUsed, false)
		switch x.Op {
		case token.AND:
			switch x2 := x.X.(type) {
			case *ast.StarExpr:
				*n = x2.X
			}
		}
	case *ast.CompositeLit:
		for i := range x.Elts {
			t.expr(&x.Elts[i], true, true)
		}
	case *ast.KeyValueExpr:
		t.expr(&x.Key, true, true)
		t.expr(&x.Value, true, true)
	case *ast.InterfaceType:
		// nop
	case *ast.SliceExpr:
		t.expr(&x.X, trackUsed, false)
		t.expr(&x.Low, trackUsed, true)
		t.expr(&x.High, trackUsed, true)
		t.expr(&x.Max, trackUsed, true)
	case *ast.TypeAssertExpr:
		t.expr(&x.X, trackUsed, true)
	case nil:
		// nop
	default:
		todo("%v: %T", t.pos(x), x)
	}
}

func (t *tidy) not(n ast.Expr) ast.Expr {
	switch x := n.(type) {
	case *ast.BinaryExpr:
		switch x.Op {
		case
			token.LEQ,
			token.LSS,
			token.EQL,
			token.NEQ,
			token.GEQ,
			token.LAND,
			token.LOR,
			token.GTR:
			return &ast.UnaryExpr{Op: token.NOT, X: &ast.ParenExpr{X: x}}
		default:
			todo("%v: %v", t.pos(n), x.Op)
		}
	case *ast.ParenExpr:
		return &ast.UnaryExpr{Op: token.NOT, X: x.X}
	case *ast.UnaryExpr:
		switch x.Op {
		case token.NOT:
			return x.X
		default:
			todo("%v: %T", t.pos(n), x)
		}
	case *ast.Ident:
		switch x.Name {
		case "true":
			x.Name = "false"
			return x
		case "false":
			x.Name = "true"
			return x
		default:
			return &ast.UnaryExpr{Op: token.NOT, X: x}
		}
	default:
		todo("%v: %T", t.pos(n), x)
	}
	panic("unreachable")
}

func (t *tidy) call(n *ast.CallExpr, outermost bool) {
	t.expr(&n.Fun, true, outermost)
	for i := range n.Args {
		t.expr(&n.Args[i], true, true)
	}
}
