// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"bytes"
	"encoding/hex"
	"fmt"
	"go/scanner"
	"go/token"
	"math"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"modernc.org/cc/v3"
)

var (
	idMain  = cc.String("main")
	oTraceG bool
	oTraceW bool
	void    cc.Type = &voidType{}
)

type voidType struct {
	cc.Type
}

func (t *voidType) Kind() cc.Kind { return cc.Void }

type block struct {
	block  *cc.CompoundStatement
	decls  []*cc.Declaration // What to declare in this block.
	params []*cc.Parameter
	scope  scope

	topDecl bool // Declare locals at block start to avoid "jumps over declaration".
}

func newBlock(n *cc.CompoundStatement, decls []*cc.Declaration, params []*cc.Parameter, topDecl bool) *block {
	return &block{
		block:   n,
		decls:   decls,
		params:  params,
		scope:   newScope(),
		topDecl: topDecl,
	}
}

type local struct {
	name string
	off  uintptr // If isPinned: bp+off

	isPinned  bool // Prevent this local from being placed in Go movable stack.
	undeclare bool // Never read.
}

type switchState int

const (
	_                 switchState = iota // Not in switch.
	inSwitchFirst                        // Before seeing "case/default".
	inSwitchCase                         // Seen "case/default".
	inSwitchSeenBreak                    // In switch "case/default" and seen "break".
)

type context struct {
	block     *block
	blocks    map[*cc.CompoundStatement]*block
	bpName    string
	fndef     *cc.FunctionDefinition
	gen       *gen
	ignore    map[*cc.Declarator]bool // Pseudo declarators
	locals    map[*cc.Declarator]*local
	off       uintptr         // bp+off allocs
	params    []*cc.Parameter // May differ from what fndef says
	rt        cc.Type         // May differ from what fndef says
	scope     scope
	switchCtx switchState
	vaLists   map[*cc.PostfixExpression]uintptr

	mainSignatureForced bool
}

func newContext(g *gen, n *cc.FunctionDefinition) *context {
	d := n.Declarator
	t := d.Type()
	rt := t.Result()
	params := t.Parameters()
	var mainSignatureForced bool
	var ignore map[*cc.Declarator]bool
	if d.Name() == idMain && d.Linkage == cc.External {
		if rt.Kind() != cc.Int {
			rt = g.task.cfg.ABI.Type(cc.Int)
		}
		if len(params) != 2 {
			mainSignatureForced = true
			d1 := newDeclarator("argc")
			t1 := g.task.cfg.ABI.Type(cc.Int)
			d2 := newDeclarator("argv")
			t2 := g.task.cfg.ABI.Ptr(n, g.task.cfg.ABI.Type(cc.Void))
			params = []*cc.Parameter{
				cc.NewParameter(d1, t1),
				cc.NewParameter(d2, t2),
			}
			ignore = map[*cc.Declarator]bool{d1: true, d2: true}
		}
	}
	ctx := &context{
		blocks:              map[*cc.CompoundStatement]*block{},
		fndef:               n,
		gen:                 g,
		locals:              map[*cc.Declarator]*local{},
		mainSignatureForced: mainSignatureForced,
		params:              params,
		rt:                  rt,
		scope:               newScope(),
		vaLists:             map[*cc.PostfixExpression]uintptr{},
		ignore:              ignore,
	}
	ctx.scope.take("tls")
	ctx.layoutLocals(n.CompoundStatement, params)
	for _, v := range n.CompoundStatements() {
		ctx.renameLocals(v)
	}
	ctx.staticAllocs(n.CompoundStatement)
	return ctx
}

func newDeclarator(name string) *cc.Declarator {
	return &cc.Declarator{
		DirectDeclarator: &cc.DirectDeclarator{
			Case:  cc.DirectDeclaratorIdent,
			Token: cc.Token{Rune: cc.IDENTIFIER, Value: cc.String(name)},
		},
	}
}

func (ctx *context) staticAllocs(n *cc.CompoundStatement) {
	cc.Inspect(n, func(n cc.Node, entry bool) bool {
		if !entry {
			return true
		}

		x, ok := n.(*cc.PostfixExpression)
		if !ok || x.Case != cc.PostfixExpressionCall {
			return true
		}

		ft := x.PostfixExpression.Operand.Type()
		if !ft.IsVariadic() {
			return true
		}

		fixedParams := len(ft.Parameters())
		iArg := 0
		var need uintptr
		for list := x.ArgumentExpressionList; list != nil; list, iArg = list.ArgumentExpressionList, iArg+1 {
			if iArg < fixedParams {
				continue
			}

			t := list.AssignmentExpression.Operand.Type()
			if t.IsIntegerType() {
				need += 8
				continue
			}

			switch t.Kind() {
			case cc.Array:
				need += 8
			default:
				panic(todo("", t))
			}
		}
		if need != 0 {
			ctx.vaLists[x] = roundup(ctx.off, 8)
			ctx.off += need
		}
		return true
	})
}

func (ctx *context) layoutLocals(n *cc.CompoundStatement, params []*cc.Parameter) {
	block := newBlock(n, n.Declarations(), params, n.IsJumpTarget())
	ctx.blocks[n] = block
	for _, v := range n.Children() {
		ctx.layoutLocals(v, nil)
		if len(v.LabeledStatements()) != 0 {
			vb := ctx.blocks[v]
			block.decls = append(block.decls, vb.decls...)
			vb.decls = nil
		}
	}
}

func (ctx *context) renameLocals(n *cc.CompoundStatement) {
	block := ctx.blocks[n]
	type item struct {
		ds *cc.DeclarationSpecifiers
		d  *cc.Declarator
	}
	var work []item
	for _, v := range block.params {
		work = append(work, item{nil, v.Declarator()})
	}
	for _, decl := range block.decls {
		ds := decl.DeclarationSpecifiers
		for list := decl.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
			work = append(work, item{ds, list.InitDeclarator.Declarator})
		}
	}
	for _, item := range work {
		d := item.d
		if !ctx.ignore[d] && d.IsStatic() {
			continue
		}

		local := &local{undeclare: d.Read == 0}
		ctx.locals[d] = local
		if d.AddressTaken {
			local.isPinned = true
			local.off = roundup(ctx.off, uintptr(d.Type().Align()))
			ctx.off = local.off + d.Type().Size()
		}
		local.name = block.scope.take(d.Name().String())
	}
}

func roundup(n, to uintptr) uintptr {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}

type tld struct {
	name string // Can differ from the original one.

	hasInitilaizer bool
}

type taggedStruct struct {
	ctyp  cc.Type
	gotyp string
	name  string

	emitted bool
}

func (s *taggedStruct) emit(g *gen, ds *cc.DeclarationSpecifiers) {
	if s == nil || s.emitted {
		return
	}

	s.emitted = true
	g.w("%stype %s = %s;\n\n", comment("\n", ds), s.name, s.gotyp)
}

type gen struct {
	bssName  string
	bssNameP string
	bssSize  uintptr // BSS segment
	buf      bytes.Buffer
	ds       bytes.Buffer // Data segment
	errors   scanner.ErrorList
	imports  map[string]*imported // C name: import info
	scope    scope
	structs  map[cc.StringID]*taggedStruct // key: C tag
	task     *task
	tlds     map[*cc.Declarator]*tld
	ts       bytes.Buffer // Text segment
	tsName   string
	tsNameP  string
	tsOffs   map[string]uintptr

	isMain bool
}

func newGen(t *task) (*gen, error) {
	g := &gen{
		imports: map[string]*imported{},
		scope:   newScope(),
		tsOffs:  map[string]uintptr{},
		task:    t,
		tlds:    map[*cc.Declarator]*tld{},
	}
	if err := g.layout(); err != nil {
		return nil, err
	}

	for _, v := range t.imports {
		var err error
		if v.name, v.exports, err = t.capi(v.path); err != nil {
			return nil, err
		}

		v.qualifier = g.scope.take(v.name) + "."
		for k := range v.exports {
			if g.imports[k] == nil {
				g.imports[k] = v
			}
		}
	}
	g.bssName = g.scope.take("bss")
	g.bssNameP = g.scope.take("bss")
	g.tsNameP = g.scope.take("ts")
	g.tsName = g.scope.take("ts")
	return g, nil
}

func (g *gen) err(n cc.Node, s string, args ...interface{}) {
	g.errors.Add(token.Position(n.Position()), fmt.Sprintf(s, args...))
}

func (g *gen) Err() error {
	if len(g.errors) == 0 {
		return nil
	}

	var lpos token.Position
	w := 0
	for _, v := range g.errors {
		if lpos.Filename != "" {
			if v.Pos.Filename == lpos.Filename && v.Pos.Line == lpos.Line {
				continue
			}
		}

		g.errors[w] = v
		w++
		lpos = v.Pos
	}
	g.errors = g.errors[:w]
	sort.Slice(g.errors, func(i, j int) bool {
		a := g.errors[i]
		b := g.errors[j]
		if !a.Pos.IsValid() && b.Pos.IsValid() {
			return true
		}

		if a.Pos.IsValid() && !b.Pos.IsValid() {
			return false
		}

		if a.Pos.Filename < b.Pos.Filename {
			return true
		}

		if a.Pos.Filename > b.Pos.Filename {
			return false
		}

		if a.Pos.Line < b.Pos.Line {
			return true
		}

		if a.Pos.Line > b.Pos.Line {
			return false
		}

		return a.Pos.Column < b.Pos.Column
	})
	a := make([]string, 0, len(g.errors))
	for _, v := range g.errors {
		a = append(a, v.Error())
	}
	return fmt.Errorf("%s", strings.Join(a, "\n"))
}

func (g *gen) layout() error {
	if err := g.layoutTLDs(); err != nil {
		return err
	}

	if err := g.layoutStructs(); err != nil {
		return err
	}

	return g.layoutStaticLocals()
}

func (g *gen) layoutStructs() error {
	m := map[cc.StringID]*taggedStruct{}
	for _, v := range g.task.asts {
		for tag, v := range v.StructTypes {
			s := m[tag]
			if s == nil {
				m[tag] = &taggedStruct{name: g.scope.take("S" + tag.String()), ctyp: v}
				continue
			}

			if v.String() != s.ctyp.String() { // Conflict, cannot use named struct/union types.
				return nil
			}
		}
	}

	g.structs = m
	for _, v := range m {
		v.gotyp = g.structType(v.ctyp)
	}
	return nil
}

func (g *gen) structType(t cc.Type) string {
	switch t.Kind() {
	case cc.Struct, cc.Union:
		tag := t.Tag()
		if tag != 0 && g.structs != nil {
			s := g.structs[tag]
			if s.gotyp == "" {
				s.gotyp = g.structLiteral(t)
			}
			return s.gotyp
		}

		return g.structLiteral(t)
	default:
		panic(todo("internal error: %v", t.Kind()))
	}
}

func (g *gen) structLiteral(t cc.Type) string {
	nf := t.NumField()
	idx := []int{0}
	var b strings.Builder
	switch t.Kind() {
	case cc.Struct:
		b.WriteString("struct {")
		for idx[0] = 0; idx[0] < nf; idx[0]++ {
			f := t.FieldByIndex(idx)
			fmt.Fprintf(&b, "F%s %s;", f.Name(), g.typ(f.Type()))
			if f.Padding() != 0 {
				fmt.Fprintf(&b, "_ [%d]byte;", f.Padding())
			}
		}
		b.WriteByte('}')
	case cc.Union:
		b.WriteString("struct {")
		al := uintptr(t.Align())
		sz := t.Size()
		if al > sz {
			panic(todo(""))
		}

		switch al {
		case 1:
			fmt.Fprintf(&b, "U [%d]byte;", sz)
		case 2:
			fmt.Fprintf(&b, "_ [0]int16;")
			fmt.Fprintf(&b, "U [%d]byte;", sz)
		case 4:
			fmt.Fprintf(&b, "_ [0]int32;")
			fmt.Fprintf(&b, "U [%d]byte;", sz)
		case 8:
			fmt.Fprintf(&b, "_ [0]int64;")
			fmt.Fprintf(&b, "U [%d]byte;", sz)
		default:
			panic(todo("", al))
		}
		b.WriteByte('}')
	default:
		panic(todo("internal error: %v", t.Kind()))
	}
	return b.String()
}

func (g *gen) layoutStaticLocals() error {
	for _, v := range g.task.asts {
		for list := v.TranslationUnit; list != nil; list = list.TranslationUnit {
			decl := list.ExternalDeclaration
			switch decl.Case {
			case cc.ExternalDeclarationFuncDef: // FunctionDefinition
				// ok
			default:
				continue
			}

			cc.Inspect(decl.FunctionDefinition.CompoundStatement, func(n cc.Node, entry bool) bool {
				switch x := n.(type) {
				case *cc.Declarator:
					if !entry || !x.IsStatic() || x.Read == 0 || x.IsParameter {
						break
					}

					panic(todo("", x.Position(), x.Name(), x.Read, x.Write))
				}
				return true
			})
		}
	}
	return nil
}

func (g *gen) layoutTLDs() error {
	var a []*cc.Declarator
	externs := map[cc.StringID]*tld{}
	for _, v := range g.task.asts {
		a = a[:0]
		for k := range v.TLD {
			a = append(a, k)
		}
		sort.Slice(a, func(i, j int) bool {
			return a[i].NameTok().Seq() < a[j].NameTok().Seq()
		})
		for _, d := range a {
			isMain := false
			if d.Name() == idMain && d.Linkage == cc.External {
				isMain = true
				g.isMain = true
			}
			if !isMain && (d.Read == 0 && d.Write == 0 || d.IsExtern()) {
				continue
			}

			nm := d.Name()
			switch d.Linkage {
			case cc.External:
				if _, ok := externs[nm]; !ok { // First definition.
					name := fmt.Sprintf("X%s", d.Name())
					g.scope.take(name)
					tld := &tld{name: name, hasInitilaizer: d.HasInitializer()}
					externs[nm] = tld
					g.tlds[d] = tld
					break
				}

				panic(todo(""))
			case cc.Internal:
				name := nm.String()
				if token.IsExported(name) {
					name = "s" + name
				}
				g.tlds[d] = &tld{name: g.scope.take(name), hasInitilaizer: d.HasInitializer()}
			default:
				panic(todo("", d.Linkage))
			}
		}
	}
	return nil
}

func (g *gen) o(s string, args ...interface{}) {
	if oTraceG {
		fmt.Printf(s, args...)
	}
	fmt.Fprintf(g.task.out, s, args...)
}

func (g *gen) w(s string, args ...interface{}) {
	if oTraceW {
		fmt.Printf(s, args...)
	}
	fmt.Fprintf(&g.buf, s, args...)
}

func (g *gen) main() error {
	g.o(`// Code generated by '%s %s', DO NOT EDIT.
	
package %s
	
`,
		filepath.Base(g.task.args[0]),
		strings.Join(g.task.args[1:], " "),
		g.task.pkgName,
	)
	for _, v := range g.task.asts {
		g.oneAST(v)
	}
	sort.Slice(g.task.imports, func(i, j int) bool { return g.task.imports[i].path < g.task.imports[j].path })
	g.o(`import (
	"reflect"
	"unsafe"
`)
	first := true
	for _, v := range g.task.imports {
		if v.used {
			if first {
				g.o("\n")
				first = false
			}
			g.o("\t%q\n", v.path)
		}
	}
	g.o(`)
	
var _ unsafe.Pointer
var _ reflect.Kind
`)
	if g.isMain {
		g.o(`
func main() { %sStart(Xmain) }`, g.task.crt)
	}
	g.flushStructs()
	g.flushBSS()
	g.flushTS()
	if _, err := g.buf.WriteTo(g.task.out); err != nil {
		return err
	}

	return g.Err()
}

func (g *gen) flushBSS() {
	if g.bssSize == 0 {
		return
	}

	n := roundup(g.bssSize, 8)
	g.w("var %s [%d]uint64\n", g.bssNameP, n/8)
	g.w("var %s = uintptr(unsafe.Pointer(&%s[0]))\n\n", g.bssName, g.bssNameP)
}

func (g *gen) flushStructs() {
	var a []*taggedStruct
	for _, v := range g.structs {
		if !v.emitted {
			a = append(a, v)
		}
	}
	sort.Slice(a, func(i, j int) bool { return a[i].name < a[j].name })
	for _, v := range a {
		v.emit(g, nil)
	}
}

func (g *gen) flushTS() {
	b := g.ts.Bytes()
	if len(b) == 0 {
		return
	}

	s := strings.TrimSpace(hex.Dump(b))
	a := strings.Split(s, "\n")
	g.w("//  %s\n", strings.Join(a, "\n//  "))
	g.w("var %s = %q\n", g.tsName, b)
	g.w("var %s = (*reflect.StringHeader)(unsafe.Pointer(&%s)).Data\n", g.tsNameP, g.tsName)
}

func (g *gen) oneAST(ast *cc.AST) {
	for list := ast.TranslationUnit; list != nil; list = list.TranslationUnit {
		g.externalDeclaration(list.ExternalDeclaration)
	}
}

func (g *gen) externalDeclaration(n *cc.ExternalDeclaration) {
	switch n.Case {
	case cc.ExternalDeclarationFuncDef: // FunctionDefinition
		g.functionDefinition(n.FunctionDefinition)
	case cc.ExternalDeclarationDecl: // Declaration
		g.declaration(nil, n.Declaration)
	case cc.ExternalDeclarationAsm: // AsmFunctionDefinition
		panic(todo("", n.Position()))
	case cc.ExternalDeclarationAsmStmt: // AsmStatement
		panic(todo("", n.Position()))
	case cc.ExternalDeclarationEmpty: // ';'
		panic(todo("", n.Position()))
	case cc.ExternalDeclarationPragma: // PragmaSTDC
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) functionDefinition(n *cc.FunctionDefinition) {
	// DeclarationSpecifiers Declarator DeclarationList CompoundStatement
	d := n.Declarator
	tld := g.tlds[d]
	if tld == nil {
		return
	}

	ctx := newContext(g, n)
	g.functionDefinitionSignature(ctx, tld)
	g.w(" ")
	if need := ctx.off; need != 0 {
		ctx.bpName = ctx.scope.take("bp")
		g.w("{\n%s := tls.Alloc(%d)\n", ctx.bpName, need)
		g.w("defer tls.Free(%d)\n", need)
	}
	g.compoundStatement(ctx, n.CompoundStatement, false)
	g.w("\n\n")
}

func (g *gen) compoundStatement(ctx *context, n *cc.CompoundStatement, forceNoBraces bool) {
	// '{' BlockItemList '}'
	brace := (!n.IsJumpTarget() || n.Parent() == nil) && !forceNoBraces
	if brace && (n.Parent() != nil || ctx.off == 0) {
		g.w("{")
	}
	ctx.block = ctx.blocks[n]
	if ctx.block.topDecl {
		panic(todo(""))
	}
	var r *cc.JumpStatement
	for list := n.BlockItemList; list != nil; list = list.BlockItemList {
		r = g.blockItem(ctx, list.BlockItem)
	}
	if n.Parent() == nil && r == nil && ctx.rt.Kind() != cc.Void {
		g.w("\nreturn ")
		g.w("0%s", convert(g.task.cfg.ABI.Type(cc.Int), ctx.rt))
	}
	if brace {
		g.w("}")
	}
}

func (g *gen) blockItem(ctx *context, n *cc.BlockItem) (r *cc.JumpStatement) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		g.declaration(ctx, n.Declaration)
	case cc.BlockItemStmt: // Statement
		r = g.statement(ctx, n.Statement, false)
		g.w(";")
	case cc.BlockItemLabel: // LabelDeclaration
		panic(todo("", n.Position()))
		g.w(";")
	case cc.BlockItemFuncDef: // DeclarationSpecifiers Declarator CompoundStatement
		panic(todo("", n.Position()))
		g.w(";")
	case cc.BlockItemPragma: // PragmaSTDC
		panic(todo("", n.Position()))
		g.w(";")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (g *gen) statement(ctx *context, n *cc.Statement, forceCompoundStmtBrace bool) (r *cc.JumpStatement) {
	if ctx.switchCtx == inSwitchSeenBreak && n.Case != cc.StatementLabeled {
		return nil
	}

	if forceCompoundStmtBrace {
		g.w(" {")
	}
	switch n.Case {
	case cc.StatementLabeled: // LabeledStatement
		g.labeledStatement(ctx, n.LabeledStatement)
	case cc.StatementCompound: // CompoundStatement
		g.compoundStatement(ctx, n.CompoundStatement, forceCompoundStmtBrace)
	case cc.StatementExpr: // ExpressionStatement
		g.expressionStatement(ctx, n.ExpressionStatement)
	case cc.StatementSelection: // SelectionStatement
		g.selectionStatement(ctx, n.SelectionStatement)
	case cc.StatementIteration: // IterationStatement
		g.iterationStatement(ctx, n.IterationStatement)
	case cc.StatementJump: // JumpStatement
		r = g.jumpStatement(ctx, n.JumpStatement)
	case cc.StatementAsm: // AsmStatement
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if forceCompoundStmtBrace {
		g.w("}")
	}
	return r
}

func (g *gen) labeledStatement(ctx *context, n *cc.LabeledStatement) {
	switch {
	case ctx.block.block.IsJumpTarget():
		panic(todo(""))
	default:
		switch n.Case {
		case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
			panic(todo("", n.Position(), n.Case))
		case cc.LabeledStatementCaseLabel: // "case" ConstantExpression ':' Statement
			switch ctx.switchCtx {
			case inSwitchFirst:
				ctx.switchCtx = inSwitchCase
			case inSwitchCase:
				g.w("\nfallthrough;")
			case inSwitchSeenBreak:
				ctx.switchCtx = inSwitchCase
			default:
				panic(todo("", n.Position(), ctx.switchCtx))
			}
			g.w("%scase ", comment("\n", n))
			g.constantExpression(ctx, n.ConstantExpression, n.ConstantExpression.Operand.Type(), false, true)
			g.w(":")
			g.statement(ctx, n.Statement, false)
		case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
			panic(todo("", n.Position(), n.Case))
		case cc.LabeledStatementDefault: // "default" ':' Statement
			switch ctx.switchCtx {
			case inSwitchFirst:
				ctx.switchCtx = inSwitchCase
			case inSwitchCase:
				g.w("\nfallthrough;")
			case inSwitchSeenBreak:
				ctx.switchCtx = inSwitchCase
			default:
				panic(todo("", n.Position(), ctx.switchCtx))
			}
			g.w("%sdefault:", comment("\n", n))
			g.statement(ctx, n.Statement, false)
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (g *gen) constantExpression(ctx *context, n *cc.ConstantExpression, t cc.Type, asBool, outermost bool) {
	// ConditionalExpression
	g.conditionalExpression(ctx, n.ConditionalExpression, t, asBool, outermost)
}

func (g *gen) selectionStatement(ctx *context, n *cc.SelectionStatement) {
	switch {
	case ctx.block.block.IsJumpTarget():
		panic(todo(""))
	default:
		g.w("%s", comment("\n", n))
		switch n.Case {
		case cc.SelectionStatementIf: // "if" '(' Expression ')' Statement
			g.w("if ")
			g.expression(ctx, n.Expression, n.Expression.Operand.Type(), true, true)
			g.statement(ctx, n.Statement, true)
		case cc.SelectionStatementIfElse: // "if" '(' Expression ')' Statement "else" Statement
			g.w("if ")
			g.expression(ctx, n.Expression, n.Expression.Operand.Type(), true, true)
			g.statement(ctx, n.Statement, true)
			g.w(" else ")
			g.statement(ctx, n.Statement2, true)
		case cc.SelectionStatementSwitch: // "switch" '(' Expression ')' Statement
			sv := ctx.switchCtx
			ctx.switchCtx = inSwitchFirst
			g.w("switch ")
			g.expression(ctx, n.Expression, n.Expression.Operand.Type(), false, true)
			g.statement(ctx, n.Statement, true)
			ctx.switchCtx = sv
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (g *gen) iterationStatement(ctx *context, n *cc.IterationStatement) {
	sv := ctx.switchCtx
	ctx.switchCtx = 0

	defer func() { ctx.switchCtx = sv }()

	switch {
	case ctx.block.block.IsJumpTarget():
		panic(todo(""))
	default:
		g.w("%s", comment("\n", n))
		switch n.Case {
		case cc.IterationStatementWhile: // "while" '(' Expression ')' Statement
			g.w("for ")
			g.expression(ctx, n.Expression, n.Expression.Operand.Type(), true, true)
			g.statement(ctx, n.Statement, true)
		case cc.IterationStatementDo: // "do" Statement "while" '(' Expression ')' ';'
			v := ctx.scope.take("ok")
			g.w("for %v := true; %[1]v; %[1]v = ", v)
			g.expression(ctx, n.Expression, n.Expression.Operand.Type(), true, true)
			g.statement(ctx, n.Statement, true)
		case cc.IterationStatementFor: // "for" '(' Expression ';' Expression ';' Expression ')' Statement
			g.w("for ")
			g.expression(ctx, n.Expression, void, false, true)
			g.w("; ")
			g.expression(ctx, n.Expression2, n.Expression2.Operand.Type(), true, true)
			g.w("; ")
			g.expression(ctx, n.Expression3, void, false, true)
			g.statement(ctx, n.Statement, true)
		case cc.IterationStatementForDecl: // "for" '(' Declaration Expression ';' Expression ')' Statement
			panic(todo("", n.Position()))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (g *gen) jumpStatement(ctx *context, n *cc.JumpStatement) (r *cc.JumpStatement) {
	g.w("%s", comment("\n", n))
	switch n.Case {
	case cc.JumpStatementGoto: // "goto" IDENTIFIER ';'
		panic(todo("", n.Position()))
	case cc.JumpStatementGotoExpr: // "goto" '*' Expression ';'
		panic(todo("", n.Position()))
	case cc.JumpStatementContinue: // "continue" ';'
		panic(todo("", n.Position()))
	case cc.JumpStatementBreak: // "break" ';'
		if _, ok := n.Context().(*cc.SelectionStatement); ok {
			switch ctx.switchCtx {
			case inSwitchCase:
				ctx.switchCtx = inSwitchSeenBreak
				return
			default:
				panic(todo("", n.Position(), ctx.switchCtx))
			}
		}
		g.w("break")
	case cc.JumpStatementReturn: // "return" Expression ';'
		r = n
		g.w("return ")
		g.expression(ctx, n.Expression, ctx.rt, false, true)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (g *gen) expressionStatement(ctx *context, n *cc.ExpressionStatement) {
	g.w("%s", comment("\n", n))
	// Expression AttributeSpecifierList ';'
	g.expression(ctx, n.Expression, void, false, true)
}

func (g *gen) expression(ctx *context, n *cc.Expression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		g.assignmentExpression(ctx, n.AssignmentExpression, t, asBool, outermost)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) assignmentExpression(ctx *context, n *cc.AssignmentExpression, t cc.Type, asBool, outermost bool) {
	switch {
	case t.Kind() == cc.Void:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			g.conditionalExpression(ctx, n.ConditionalExpression, t, asBool, outermost)
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			g.unaryExpression(ctx, n.UnaryExpression, n.UnaryExpression.Operand.Type(), false, outermost)
			g.w(" = ")
			g.assignmentExpression(ctx, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), false, true)
		case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
			panic(todo("", n.Position()))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	default:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			g.conditionalExpression(ctx, n.ConditionalExpression, t, asBool, outermost)
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			if d := n.UnaryExpression.Declarator(); d != nil {
				if local := ctx.locals[d]; local != nil {
					if local.isPinned {
						panic(todo("", n.Position()))
					}

					s := convert(d.Type(), n.Operand.Type())
					g.w("%sAssign%s(&%s, ", g.task.crt, g.helperType(d.Type()), local.name)
					g.assignmentExpression(ctx, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), false, true)
					g.w(")%s", s)
					break
				}

				panic(todo("", n.Position()))
			}

			panic(todo("", n.Position()))
		case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
			panic(todo("", n.Position()))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (g *gen) helperType(t cc.Type) string {
	s := g.typ(t)
	return strings.ToUpper(s[:1]) + s[1:]
}

func convert(from, to cc.Type) string {
	if from.IsIntegerType() && from.Kind() == to.Kind() {
		return ""
	}

	switch from.Kind() {
	case cc.Function, cc.Struct, cc.Union, cc.Array, cc.Ptr:
		if from.Kind() == to.Kind() {
			return ""
		}
	}

	panic(todo("%q -> %q", from, to))
}

func (g *gen) conditionalExpression(ctx *context, n *cc.ConditionalExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		g.logicalOrExpression(ctx, n.LogicalOrExpression, t, asBool, outermost)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) logicalOrExpression(ctx *context, n *cc.LogicalOrExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		g.logicalAndExpression(ctx, n.LogicalAndExpression, t, asBool, outermost)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		if !outermost {
			g.w("(")
			defer g.w(")")
		}
		if !asBool {
			defer g.w("%s", convertOp(n.Operand, t))

			g.w("%sBool32(", g.task.crt)

			defer g.w(")")
		}
		g.logicalOrExpression(ctx, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), true, false)
		g.w(" || ")
		g.logicalAndExpression(ctx, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), true, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) logicalAndExpression(ctx *context, n *cc.LogicalAndExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		g.inclusiveOrExpression(ctx, n.InclusiveOrExpression, t, asBool, outermost)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		if !outermost {
			g.w("(")
			defer g.w(")")
		}
		if !asBool {
			defer g.w("%s", convertOp(n.Operand, t))

			g.w("%sBool32(", g.task.crt)

			defer g.w(")")
		}
		g.logicalAndExpression(ctx, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), true, false)
		g.w(" && ")
		g.inclusiveOrExpression(ctx, n.InclusiveOrExpression, n.InclusiveOrExpression.Operand.Type(), true, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) inclusiveOrExpression(ctx *context, n *cc.InclusiveOrExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		g.exclusiveOrExpression(ctx, n.ExclusiveOrExpression, t, asBool, outermost)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		if !outermost {
			g.w("(")
			defer g.w(")")
		}
		switch {
		case asBool:
			g.w("(")
			defer g.w(") != 0")
		default:
			defer g.w("%s", convertOp(n.Operand, t))
		}
		g.inclusiveOrExpression(ctx, n.InclusiveOrExpression, n.Promote(), false, false)
		g.w(" | ")
		g.exclusiveOrExpression(ctx, n.ExclusiveOrExpression, n.Promote(), false, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) exclusiveOrExpression(ctx *context, n *cc.ExclusiveOrExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		g.andExpression(ctx, n.AndExpression, t, asBool, outermost)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		if !outermost {
			g.w("(")
			defer g.w(")")
		}
		switch {
		case asBool:
			g.w("(")
			defer g.w(") != 0")
		default:
			defer g.w("%s", convertOp(n.Operand, t))
		}
		g.exclusiveOrExpression(ctx, n.ExclusiveOrExpression, n.Promote(), false, false)
		g.w(" ^ ")
		g.andExpression(ctx, n.AndExpression, n.Promote(), false, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) andExpression(ctx *context, n *cc.AndExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		g.equalityExpression(ctx, n.EqualityExpression, t, asBool, outermost)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		if !outermost {
			g.w("(")
			defer g.w(")")
		}
		switch {
		case asBool:
			g.w("(")
			defer g.w(") != 0")
		default:
			defer g.w("%s", convertOp(n.Operand, t))
		}
		g.andExpression(ctx, n.AndExpression, n.Promote(), false, false)
		g.w(" & ")
		g.equalityExpression(ctx, n.EqualityExpression, n.Promote(), false, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) equalityExpression(ctx *context, n *cc.EqualityExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		g.relationalExpression(ctx, n.RelationalExpression, t, asBool, outermost)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		g.binaryEqualityExpression(ctx, n, " == ", t, asBool)
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		g.binaryEqualityExpression(ctx, n, " != ", t, asBool)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryEqualityExpression(ctx *context, n *cc.EqualityExpression, oper string, t cc.Type, asBool bool) {
	if !asBool {
		defer g.w("%s", convertOp(n.Operand, t))

		g.w("%sBool32(", g.task.crt)

		defer g.w(")")
	}
	g.equalityExpression(ctx, n.EqualityExpression, n.Promote(), false, false)
	g.w("%s", oper)
	g.relationalExpression(ctx, n.RelationalExpression, n.Promote(), false, false)
}

func (g *gen) relationalExpression(ctx *context, n *cc.RelationalExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		g.shiftExpression(ctx, n.ShiftExpression, t, asBool, outermost)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		g.binaryRelationalExpression(ctx, n, " < ", t, asBool)
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		g.binaryRelationalExpression(ctx, n, " > ", t, asBool)
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		g.binaryRelationalExpression(ctx, n, " <= ", t, asBool)
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		g.binaryRelationalExpression(ctx, n, " >= ", t, asBool)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryRelationalExpression(ctx *context, n *cc.RelationalExpression, oper string, t cc.Type, asBool bool) {
	if !asBool {
		defer g.w("%s", convertOp(n.Operand, t))

		g.w("%sBool32(", g.task.crt)

		defer g.w(")")
	}
	g.relationalExpression(ctx, n.RelationalExpression, n.Promote(), false, false)
	g.w("%s", oper)
	g.shiftExpression(ctx, n.ShiftExpression, n.Promote(), false, false)
}

func convertOp(op cc.Operand, to cc.Type) string {
	from := op.Type()
	if from.IsIntegerType() && from.Kind() == to.Kind() {
		return ""
	}

	panic(todo("%v %q -> %q", op.Value(), op.Type(), to))
}

func (g *gen) shiftExpression(ctx *context, n *cc.ShiftExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		g.additiveExpression(ctx, n.AdditiveExpression, t, asBool, outermost)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		g.binaryShiftExpression(ctx, n, "<<", t, asBool, outermost)
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		g.binaryShiftExpression(ctx, n, ">>", t, asBool, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryShiftExpression(ctx *context, n *cc.ShiftExpression, oper string, t cc.Type, asBool, outermost bool) {
	if !outermost {
		g.w("(")
		defer g.w(")")
	}
	switch {
	case asBool:
		g.w("(")
		defer g.w(") != 0")
	default:
		defer g.w("%s", convertOp(n.Operand, t))
	}
	g.shiftExpression(ctx, n.ShiftExpression, n.Operand.Type(), false, false)
	g.w("%s", oper)
	g.additiveExpression(ctx, n.AdditiveExpression, n.Promote(), false, false)
}

func (g *gen) additiveExpression(ctx *context, n *cc.AdditiveExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		g.multiplicativeExpression(ctx, n.MultiplicativeExpression, t, asBool, outermost)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		g.binaryAdditiveExpression(ctx, n, "+", t, asBool, outermost)
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		g.binaryAdditiveExpression(ctx, n, "-", t, asBool, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryAdditiveExpression(ctx *context, n *cc.AdditiveExpression, oper string, t cc.Type, asBool, outermost bool) {
	if !outermost {
		g.w("(")
		defer g.w(")")
	}
	switch {
	case asBool:
		g.w("(")
		defer g.w(") != 0")
	default:
		defer g.w("%s", convertOp(n.Operand, t))
	}
	switch {
	case n.AdditiveExpression.Operand.Type().IsArithmeticType() && n.MultiplicativeExpression.Operand.Type().IsArithmeticType():
		g.additiveExpression(ctx, n.AdditiveExpression, n.Promote(), false, false)
		g.w("%s", oper)
		g.multiplicativeExpression(ctx, n.MultiplicativeExpression, n.Promote(), false, false)
	default:
		panic(todo(""))
	}
}

func (g *gen) multiplicativeExpression(ctx *context, n *cc.MultiplicativeExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		g.castExpression(ctx, n.CastExpression, t, asBool, outermost)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		g.binaryMultiplicativeExpression(ctx, n, "*", t, asBool, outermost)
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		g.binaryMultiplicativeExpression(ctx, n, "/", t, asBool, outermost)
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		g.binaryMultiplicativeExpression(ctx, n, "%", t, asBool, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryMultiplicativeExpression(ctx *context, n *cc.MultiplicativeExpression, oper string, t cc.Type, asBool, outermost bool) {
	if !outermost {
		g.w("(")
		defer g.w(")")
	}
	switch {
	case asBool:
		g.w("(")
		defer g.w(") != 0")
	default:
		defer g.w("%s", convertOp(n.Operand, t))
	}
	g.multiplicativeExpression(ctx, n.MultiplicativeExpression, n.Promote(), false, false)
	g.w("%s", oper)
	g.castExpression(ctx, n.CastExpression, n.Promote(), asBool, false)
}

func (g *gen) castExpression(ctx *context, n *cc.CastExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		g.unaryExpression(ctx, n.UnaryExpression, t, asBool, outermost)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) unaryExpression(ctx *context, n *cc.UnaryExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		g.postfixExpression(ctx, n.PostfixExpression, t, asBool, outermost)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		g.castExpressionAddrOf(ctx, n.CastExpression)
	case cc.UnaryExpressionDeref: // '*' CastExpression
		s := convert(n.CastExpression.Operand.Type().Elem(), t)
		g.w("*(*%s)(unsafe.Pointer(", g.typ(n.Operand.Type()))
		g.castExpression(ctx, n.CastExpression, n.CastExpression.Operand.Type(), false, true)
		g.w("))%s", s)
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		g.w("-")
		g.castExpression(ctx, n.CastExpression, t, false, outermost)
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo("", n.Position()))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo("", n.Position()))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) castExpressionAddrOf(ctx *context, n *cc.CastExpression) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		g.unaryExpressionAddrOf(ctx, n.UnaryExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) unaryExpressionAddrOf(ctx *context, n *cc.UnaryExpression) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		g.postfixExpressionAddrOf(ctx, n.PostfixExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) postfixExpressionAddrOf(ctx *context, n *cc.PostfixExpression) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		g.primaryExpressionAddrOf(ctx, n.PrimaryExpression)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		if d := n.PostfixExpression.Declarator(); d != nil {
			t := d.Type()
			var off uintptr
			f, ok := t.FieldByName(n.Token2.Value)
			if !ok {
				g.err(&n.Token2, "uknown field: %s", n.Token2.Value)
			} else {
				off = f.Offset()
			}

			g.declaratorAddrOf(ctx, d)
			g.w("%s /* &%s.%s */", nonZeroUintptr(off), d.Name(), n.Token2.Value)
			return
		}

		panic(todo("", n.Position()))
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		panic(todo("", n.Position()))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) primaryExpression(ctx *context, n *cc.PrimaryExpression, t cc.Type, asBool, outermost bool) {
	if asBool {
		if !outermost {
			g.w("(")
			defer g.w(")")
		}

		defer g.w(" != 0")
	}
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			g.declarator(ctx, d, t)
		default:
			panic(todo("", n.Position()))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		g.intConst(n.Token.Src.String(), n.Operand, t)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionChar: // CHARCONST
		g.charConst(n.Token.Src.String(), n.Operand, t)
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		g.w("%s", g.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		if !outermost {
			g.w("(")
			defer g.w(")")
		}
		g.expression(ctx, n.Expression, t, false, true)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) primaryExpressionAddrOf(ctx *context, n *cc.PrimaryExpression) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		g.declaratorAddrOf(ctx, n.Declarator())
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		g.expressionAddrOf(ctx, n.Expression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) expressionAddrOf(ctx *context, n *cc.Expression) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		g.assignmentExpressionAddrOf(ctx, n.AssignmentExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) assignmentExpressionAddrOf(ctx *context, n *cc.AssignmentExpression) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		g.conditionalExpressionAddrOf(ctx, n.ConditionalExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) conditionalExpressionAddrOf(ctx *context, n *cc.ConditionalExpression) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		g.logicalOrExpressionAddrOf(ctx, n.LogicalOrExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) logicalOrExpressionAddrOf(ctx *context, n *cc.LogicalOrExpression) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		g.logicalAndExpressionAddrOf(ctx, n.LogicalAndExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) logicalAndExpressionAddrOf(ctx *context, n *cc.LogicalAndExpression) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		g.inclusiveOrExpressionAddrOf(ctx, n.InclusiveOrExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) inclusiveOrExpressionAddrOf(ctx *context, n *cc.InclusiveOrExpression) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		g.exclusiveOrExpressionAddOf(ctx, n.ExclusiveOrExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) exclusiveOrExpressionAddOf(ctx *context, n *cc.ExclusiveOrExpression) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		g.andExpressionAddrOf(ctx, n.AndExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) andExpressionAddrOf(ctx *context, n *cc.AndExpression) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		g.equalityExpressionAddrOf(ctx, n.EqualityExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) equalityExpressionAddrOf(ctx *context, n *cc.EqualityExpression) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		g.relationalExpressionAddrOf(ctx, n.RelationalExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) relationalExpressionAddrOf(ctx *context, n *cc.RelationalExpression) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		g.shiftExpressionAddrOf(ctx, n.ShiftExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) shiftExpressionAddrOf(ctx *context, n *cc.ShiftExpression) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		g.additiveExpressionAddrOf(ctx, n.AdditiveExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) additiveExpressionAddrOf(ctx *context, n *cc.AdditiveExpression) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		g.multiplicativeExpressionAddrOf(ctx, n.MultiplicativeExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) multiplicativeExpressionAddrOf(ctx *context, n *cc.MultiplicativeExpression) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		g.castExpressionAddrOf(ctx, n.CastExpression)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) declarator(ctx *context, d *cc.Declarator, t cc.Type) {
	if local := ctx.locals[d]; local != nil {
		if local.isPinned {
			g.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", g.typ(d.Type()), ctx.bpName, nonZeroUintptr(local.off), local.name)
			return
		}

		g.w("%s", local.name)
		return
	}

	tld := g.tlds[d]
	if d.Type().Kind() == cc.Function {
		switch {
		case tld != nil:
			g.w("%s", tld.name)
		default:
			nm := d.Name()
			imp := g.imports[nm.String()]
			g.w("%sX%s", imp.qualifier, nm)
		}
		return
	}
	panic(todo("", d.Position()))
}

func (g *gen) declaratorAddrOf(ctx *context, d *cc.Declarator) {
	if local := ctx.locals[d]; local != nil {
		if local.isPinned {
			g.w("%s%s/* &%s */", ctx.bpName, nonZeroUintptr(local.off), local.name)
			return
		}

		panic(todo("", d.Position()))
		return
	}

	tld := g.tlds[d]
	g.w("%s", tld.name)
}

func (g *gen) declaratorForSelect(ctx *context, d *cc.Declarator) {
	if local := ctx.locals[d]; local != nil {
		if local.isPinned {
			panic(todo("", d.Position()))
		}

		g.w("%s", local.name)
		return
	}

	tld := g.tlds[d]
	g.w("(*%s)(unsafe.Pointer(%s))", g.typ(d.Type()), tld.name)
}

func (g *gen) declaratorForPSelect(ctx *context, d *cc.Declarator) {
	if local := ctx.locals[d]; local != nil {
		if local.isPinned {
			panic(todo("", d.Position()))
			return
		}

		g.w("(*%s)(unsafe.Pointer(%s))", g.typ(d.Type().Elem()), local.name)
		return
	}

	// tld := g.tlds[d]
	panic(todo("", d.Position()))
}

func (g *gen) postfixExpression(ctx *context, n *cc.PostfixExpression, t cc.Type, asBool, outermost bool) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		g.primaryExpression(ctx, n.PrimaryExpression, t, asBool, outermost)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		g.postfixExpression(ctx, n.PostfixExpression, n.PostfixExpression.Operand.Type(), false, false)
		g.w("[")
		g.expression(ctx, n.Expression, n.Expression.Operand.Type(), false, true)
		g.w("]")
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		g.postfixExpression(ctx, n.PostfixExpression, n.PostfixExpression.Operand.Type(), false, false)
		g.argumentExpressionList(ctx, n.PostfixExpression, n.ArgumentExpressionList, ctx.vaLists[n])
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		g.postfixExpressionForSelect(ctx, n.PostfixExpression, n.PostfixExpression.Operand.Type())
		g.w(".F%s", n.Token2.Value)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		g.postfixExpressionForPSelect(ctx, n.PostfixExpression, n.PostfixExpression.Operand.Type())
		g.w(".F%s", n.Token2.Value)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		g.incDecPostfixExpression(ctx, n, true, t)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		g.incDecPostfixExpression(ctx, n, false, t)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) postfixExpressionForPSelect(ctx *context, n *cc.PostfixExpression, t cc.Type) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		g.primaryExpressionForPSelect(ctx, n.PrimaryExpression, t)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		panic(todo("", n.Position()))
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		panic(todo("", n.Position()))
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo("", n.Position()))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo("", n.Position()))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) primaryExpressionForPSelect(ctx *context, n *cc.PrimaryExpression, t cc.Type) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			g.declaratorForPSelect(ctx, d)
		default:
			panic(todo("", n.Position()))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) postfixExpressionForSelect(ctx *context, n *cc.PostfixExpression, t cc.Type) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		g.primaryExpressionForSelect(ctx, n.PrimaryExpression, t)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		g.postfixExpression(ctx, n, t, false, false)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		panic(todo("", n.Position()))
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		panic(todo("", n.Position()))
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo("", n.Position()))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo("", n.Position()))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) primaryExpressionForSelect(ctx *context, n *cc.PrimaryExpression, t cc.Type) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			g.declaratorForSelect(ctx, d)
		default:
			panic(todo("", n.Position()))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) incDecPostfixExpression(ctx *context, n *cc.PostfixExpression, up bool, t cc.Type) {
	switch {
	case t.Kind() == cc.Void:
		switch d := n.PostfixExpression.Declarator(); {
		case d != nil:
			oper := "--"
			if up {
				oper = "++"
			}
			dt := d.Type()
			if dt.IsIntegerType() {
				g.declarator(ctx, d, d.Type())
				g.w("%s", oper)
				return
			}

			if dt.IsArithmeticType() {
				panic(todo(""))
			}

			panic(todo("", n.Position(), t.Kind()))
		default:
			panic(todo("", n.Position(), t.Kind()))
		}
	default:
		panic(todo("", n.Position(), t.Kind()))
	}
}

func (g *gen) argumentExpressionList(ctx *context, pe *cc.PostfixExpression, n *cc.ArgumentExpressionList, bpOff uintptr) {
	g.w("(tls")
	ft := pe.Operand.Type()
	isVariadic := ft.IsVariadic()
	params := ft.Parameters()
	var args []*cc.AssignmentExpression
	for ; n != nil; n = n.ArgumentExpressionList {
		args = append(args, n.AssignmentExpression)
	}
	if len(args) < len(params) {
		panic(todo(""))
	}

	va := true
	if len(args) > len(params) && !isVariadic {
		g.err(pe, "too many arguments")
		va = false
	}

	paren := ""
	for i, arg := range args {
		g.w(", ")
		switch {
		case i < len(params):
			g.assignmentExpression(ctx, arg, arg.Promote(), false, true)
		case va && i == len(params):
			g.w("%sVaList(%s%s, ", g.task.crt, ctx.bpName, nonZeroUintptr(bpOff))
			paren = ")"
			fallthrough
		default:
			g.assignmentExpression(ctx, arg, arg.Promote(), false, true)
		}
	}
	if isVariadic && len(args) == len(params) {
		g.w(", 0")
	}
	g.w("%s)", paren)
}

func nonZeroUintptr(n uintptr) string {
	if n == 0 {
		return ""
	}

	return fmt.Sprintf("%+d", n)
}

func (g *gen) stringLiteral(v cc.Value) string {
	switch x := v.(type) {
	case cc.StringValue:
		s := cc.StringID(x).String()
		off, ok := g.tsOffs[s]
		if !ok {
			off = uintptr(g.ts.Len())
			g.ts.WriteString(s)
			g.ts.WriteByte(0)
			g.tsOffs[s] = off
		}
		return fmt.Sprintf("%s%s", g.tsNameP, nonZeroUintptr(off))
	default:
		panic(todo("%T", x))
	}
}

func (g *gen) charConst(src string, op cc.Operand, to cc.Type) {
	if !to.IsIntegerType() {
		panic(todo(""))
	}

	r, mb, _, err := strconv.UnquoteChar(src[1:len(src)-1], '\'')
	rValid := !mb && err == nil
	var on uint64
	switch x := op.Value().(type) {
	case cc.Int64Value:
		if x < 0 {
			panic(todo(""))
		}

		on = uint64(x)
	default:
		panic(todo("%T(%v)", x, x))
	}
	var mask uint64
	switch {
	case to.IsSignedType():
		switch to.Size() {
		case 1:
			mask = math.MaxInt8
		case 2:
			mask = math.MaxInt16
		case 4:
			mask = math.MaxInt32
		case 8:
			mask = math.MaxInt64
		default:
			panic(todo("", op.Type().Size()))
		}
	default:
		switch to.Size() {
		case 1:
			mask = math.MaxUint8
		case 2:
			mask = math.MaxUint16
		case 4:
			mask = math.MaxUint32
		case 8:
			mask = math.MaxUint64
		default:
			panic(todo("", op.Type().Size()))
		}
	}
	if rValid && uint64(r)&mask == on { // Prefer original form
		g.w("%s", src)
		return
	}

	panic(todo("", mask))
}

func (g *gen) intConst(src string, op cc.Operand, to cc.Type) {
	if !to.IsIntegerType() {
		panic(todo(""))
	}

	src = strings.TrimRight(src, "luLU")
	sn, err := strconv.ParseUint(src, 0, 64)
	snValid := err == nil
	var on uint64
	switch x := op.Value().(type) {
	case cc.Int64Value:
		if x < 0 {
			panic(todo(""))
		}

		on = uint64(x)
	default:
		panic(todo("%T(%v)", x, x))
	}
	var mask uint64
	switch {
	case to.IsSignedType():
		switch to.Size() {
		case 1:
			mask = math.MaxInt8
		case 2:
			mask = math.MaxInt16
		case 4:
			mask = math.MaxInt32
		case 8:
			mask = math.MaxInt64
		default:
			panic(todo("", op.Type().Size()))
		}
	default:
		switch to.Size() {
		case 1:
			mask = math.MaxUint8
		case 2:
			mask = math.MaxUint16
		case 4:
			mask = math.MaxUint32
		case 8:
			mask = math.MaxUint64
		default:
			panic(todo("", op.Type().Size()))
		}
	}

	if snValid && sn&mask == on { // Prefer original form
		g.w("%s", src)
		return
	}

	panic(todo("", mask))
}

func (g *gen) functionDefinitionSignature(ctx *context, tld *tld) {
	switch {
	case ctx.mainSignatureForced:
		g.w("%sfunc %s(tls *%sTLS, _ int32, _ uintptr) int32", comment("\n", ctx.fndef), tld.name, g.task.crt)
	default:
		g.w("%s", comment("\n", ctx.fndef))
		g.functionSignature(ctx, ctx.fndef.Declarator.Type(), tld.name)
	}
}

func (g *gen) functionSignature(ctx *context, t cc.Type, nm string) {
	g.w("func")
	if nm != "" {
		g.w(" %s", nm)
	}
	g.w("(tls *%sTLS", g.task.crt)
	for _, v := range t.Parameters() {
		nm := "_"
		if d := v.Declarator(); d != nil {
			if local := ctx.locals[d]; local != nil {
				nm = local.name
			}
		}
		g.w(", %s %s", nm, g.typ(v.Type()))
	}
	g.w(")")
	if rt := t.Result(); rt != nil && rt.Kind() != cc.Void {
		g.w(" %s", g.typ(rt))
	}
}

// Return first non empty token separator within n or dflt otherwise.
func comment(dflt string, n cc.Node) string {
	if s := tokenSeparator(n); s != "" {
		return s
	}

	return dflt
}

// Return the preceding white space, including any comments, of the first token
// of n.
func tokenSeparator(n cc.Node) (r string) {
	var tok cc.Token
	cc.Inspect(n, func(n cc.Node, _ bool) bool {
		if x, ok := n.(*cc.Token); ok {
			if a, b := tok.Seq(), x.Seq(); a == 0 || a > x.Seq() && b != 0 {
				tok = *x
			}
		}
		return true
	})
	return tok.Sep.String()
}

func (g *gen) declaration(ctx *context, n *cc.Declaration) {
	if n.InitDeclaratorList == nil {
		cc.Inspect(n.DeclarationSpecifiers, func(m cc.Node, entry bool) bool {
			x, ok := m.(*cc.StructOrUnionSpecifier)
			if !ok {
				return true
			}

			if tag := x.Token.Value; tag != 0 {
				g.structs[tag].emit(g, n.DeclarationSpecifiers)
			}
			return false
		})
	}

	// DeclarationSpecifiers InitDeclaratorList ';'
	first := true
	for list := n.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
		g.initDeclarator(ctx, n.DeclarationSpecifiers, list.InitDeclarator, first)
		first = false
	}
}

func (g *gen) initDeclarator(ctx *context, ds *cc.DeclarationSpecifiers, n *cc.InitDeclarator, first bool) {
	if ctx == nil {
		g.tld(ctx, ds, n)
		return
	}

	d := n.Declarator
	local := ctx.locals[d]
	if local == nil { // Dead declaration.
		return
	}

	s := "\n"
	if first {
		s = comment(s, ds)
	}
	if local.isPinned {
		g.w("%s// var %s %s at %s%s;", s, local.name, g.typ(d.Type()), ctx.bpName, nonZeroUintptr(local.off))
		return
	}

	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		if ctx.block.topDecl {
			return
		}

		g.w("%svar %s %s;", s, local.name, g.typ(d.Type()))
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		switch {
		case ctx.block.topDecl:
			panic(todo(""))
		default:
			var paren string
			g.w("%s%s := ", s, local.name)
			switch d.Type().Kind() {
			case cc.Ptr:
				// ok
			default:
				g.w("%s(", g.typ(d.Type()))
				paren = ")"
			}
			g.initializer(ctx, n.Initializer, d.Type())
			g.w("%s;", paren)
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if local.undeclare {
		g.w("_ = %s;", local.name)
	}
}

func (g *gen) typ(t cc.Type) string {
	var b strings.Builder
	if t.IsIntegerType() {
		if !t.IsSignedType() {
			b.WriteByte('u')
		}
		fmt.Fprintf(&b, "int%d", 8*t.Size())
		return b.String()
	}

	switch t.Kind() {
	case cc.Ptr:
		return "uintptr"
	case cc.Array:
		n := t.Len()
		if n == 0 {
			panic(todo(""))
		}

		fmt.Fprintf(&b, "[%d]%s", n, g.typ(t.Elem()))
		return b.String()
	case cc.Struct, cc.Union:
		if tag := t.Tag(); tag != 0 {
			if s := g.structs[tag]; s != nil {
				return s.name
			}
		}

		return g.structType(t)
	}

	panic(todo("", t.Kind(), t))
}

func (g *gen) tld(ctx *context, ds *cc.DeclarationSpecifiers, n *cc.InitDeclarator) {
	d := n.Declarator
	tld := g.tlds[d] // Dead declaration.
	if tld == nil {
		return
	}

	s := comment("\n", ds)
	t := d.Type()
	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		g.w("%svar %s = %s%s; // %v: %s\n", s, tld.name, g.bssName, nonZeroUintptr(g.allocBSS(t)), pos(n), g.typ(t))
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

// Return n's position but with baseName(path)
func pos(n cc.Node) (r token.Position) {
	r = token.Position(n.Position())
	if r.IsValid() {
		r.Filename = filepath.Base(r.Filename)
	}
	return r
}

func (g *gen) allocBSS(t cc.Type) uintptr {
	r := roundup(g.bssSize, uintptr(t.Align()))
	g.bssSize = r + t.Size()
	return r
}
