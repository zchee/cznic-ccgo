// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"bytes"
	"encoding/hex"
	"fmt"
	"go/format"
	"go/parser"
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

	isPinned bool // Prevent this local from being placed in Go movable stack.
}

type switchState int

const (
	_                 switchState = iota // Not in switch.
	inSwitchFirst                        // Before seeing "case" or "default".
	inSwitchCase                         // Or "default".
	inSwitchSeenBreak                    // In switch "case"/"default" and seen "break".
)

type context struct {
	block     *block
	blocks    map[*cc.CompoundStatement]*block
	fndef     *cc.FunctionDefinition
	gen       *gen
	ignore    map[*cc.Declarator]bool
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
			ctx.vaLists[x] = ctx.off
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
		if d.Read == 0 || !ctx.ignore[d] && d.IsStatic() {
			continue
		}

		local := &local{}
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

func (s *taggedStruct) emit(g *gen, doc string) {
	if s == nil || s.emitted {
		return
	}

	s.emitted = true
	g.w("%stype %s = %s\n\n", doc, s.name, s.gotyp)
}

type gen struct {
	buf     bytes.Buffer         // wbuf flush goes here
	ds      data                 // Data segment
	imports map[string]*imported // C name: import info
	scope   scope
	strings map[string]int                // string value: ts off
	structs map[cc.StringID]*taggedStruct // key: C tag
	task    *task
	tlds    map[*cc.Declarator]*tld
	ts      data // Text segment
	tsName  string
	tsNameP string
	wbuf    bytes.Buffer // g.w

	isMain bool
}

func newGen(t *task) (*gen, error) {
	g := &gen{
		ds:      newData(),
		imports: map[string]*imported{},
		scope:   newScope(),
		strings: map[string]int{},
		task:    t,
		tlds:    map[*cc.Declarator]*tld{},
		ts:      newData(),
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
	g.tsNameP = g.scope.take("ts")
	g.tsName = g.scope.take("ts")
	return g, nil
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
	fmt.Fprintf(&g.wbuf, s, args...)
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
	g.flushTS()
	g.flush(false)
	g.buf.WriteTo(g.task.out)
	return nil
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
		v.emit(g, "")
	}
}

func (g *gen) flush(opt bool) {
	buf := bytes.NewBufferString("package p\n")
	buf.Write(g.wbuf.Bytes())
	g.wbuf.Reset()
	fset := token.NewFileSet()
	ast, err := parser.ParseFile(fset, "", buf.Bytes(), parser.ParseComments)
	if err != nil {
		panic(todo("\n%s\n----\n%s\n----", buf.Bytes(), err))
	}

	if opt {
		newTidy(g, fset).file(ast)
	}
	fmt.Fprintf(&g.buf, "\n\n")
	format.Node(&g.buf, fset, ast.Decls)
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
	// 	case cc.ExternalDeclarationAsm: // AsmFunctionDefinition
	// 		panic(todo("", n.Position()))
	// 	case cc.ExternalDeclarationAsmStmt: // AsmStatement
	// 		panic(todo("", n.Position()))
	// 	case cc.ExternalDeclarationEmpty: // ';'
	// 		panic(todo("", n.Position()))
	// 	case cc.ExternalDeclarationPragma: // PragmaSTDC
	// 		panic(todo("", n.Position()))
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
		ctx.scope.take("bp")
		g.w("{\n")
		g.w("bp := tls.Alloc(%d)\n", need)
		g.w("defer tls.Free(%d)\n", need)
	}
	g.compoundStatement(ctx, n.CompoundStatement, false)
	g.flush(true)
}

func (g *gen) compoundStatement(ctx *context, n *cc.CompoundStatement, forceNoBraces bool) {
	// '{' BlockItemList '}'
	brace := (!n.IsJumpTarget() || n.Parent() == nil) && !forceNoBraces
	if brace && (n.Parent() != nil || ctx.off == 0) {
		g.w("{\n")
	}
	ctx.block = ctx.blocks[n]
	if ctx.block.topDecl {
		panic(todo(""))
	}
	var r *cc.JumpStatement
	for list := n.BlockItemList; list != nil; list = list.BlockItemList {
		r = g.blockItem(ctx, list.BlockItem, "\n")
	}
	if n.Parent() == nil && r == nil && ctx.rt.Kind() != cc.Void {
		g.w("return ")
		g.w("0%s", convert(g.task.cfg.ABI.Type(cc.Int), ctx.rt))
	}
	if brace {
		g.w("}\n")
	}
}

func (g *gen) blockItem(ctx *context, n *cc.BlockItem, trailer string) (r *cc.JumpStatement) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		g.declaration(ctx, n.Declaration)
	case cc.BlockItemStmt: // Statement
		r = g.statement(ctx, n.Statement, trailer, false)
	case cc.BlockItemLabel: // LabelDeclaration
		panic(todo("", n.Position()))
	case cc.BlockItemFuncDef: // DeclarationSpecifiers Declarator CompoundStatement
		panic(todo("", n.Position()))
	case cc.BlockItemPragma: // PragmaSTDC
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (g *gen) statement(ctx *context, n *cc.Statement, trailer string, forceCompoundStmtBrace bool) (r *cc.JumpStatement) {
	if ctx.switchCtx == inSwitchSeenBreak && n.Case != cc.StatementLabeled {
		return nil
	}

	if forceCompoundStmtBrace {
		g.w(" {\n")
	}
	switch n.Case {
	case cc.StatementLabeled: // LabeledStatement
		g.labeledStatement(ctx, n.LabeledStatement)
	case cc.StatementCompound: // CompoundStatement
		g.compoundStatement(ctx, n.CompoundStatement, forceCompoundStmtBrace)
	case cc.StatementExpr: // ExpressionStatement
		g.expressionStatement(ctx, n.ExpressionStatement)
		g.w("\n")
	case cc.StatementSelection: // SelectionStatement
		g.selectionStatement(ctx, n.SelectionStatement)
	case cc.StatementIteration: // IterationStatement
		g.iterationStatement(ctx, n.IterationStatement)
	case cc.StatementJump: // JumpStatement
		r = g.jumpStatement(ctx, n.JumpStatement, trailer)
	case cc.StatementAsm: // AsmStatement
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if forceCompoundStmtBrace {
		g.w("}\n")
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
				g.w("fallthrough\n")
			case inSwitchSeenBreak:
				ctx.switchCtx = inSwitchCase
			default:
				panic(todo("", n.Position(), ctx.switchCtx))
			}
			g.w("case ")
			g.constantExpression(ctx, n.ConstantExpression, n.ConstantExpression.Operand.Type())
			g.w(":\n")
			g.statement(ctx, n.Statement, "", false)
		case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
			panic(todo("", n.Position(), n.Case))
		case cc.LabeledStatementDefault: // "default" ':' Statement
			switch ctx.switchCtx {
			case inSwitchFirst:
				ctx.switchCtx = inSwitchCase
			case inSwitchCase:
				g.w("fallthrough\n")
			case inSwitchSeenBreak:
				ctx.switchCtx = inSwitchCase
			default:
				panic(todo("", n.Position(), ctx.switchCtx))
			}
			g.w("default:\n")
			g.statement(ctx, n.Statement, "", false)
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (g *gen) constantExpression(ctx *context, n *cc.ConstantExpression, t cc.Type) {
	// ConditionalExpression
	g.conditionalExpression(ctx, n.ConditionalExpression, t)
}

func (g *gen) selectionStatement(ctx *context, n *cc.SelectionStatement) {
	switch {
	case ctx.block.block.IsJumpTarget():
		panic(todo(""))
	default:
		switch n.Case {
		case cc.SelectionStatementIf: // "if" '(' Expression ')' Statement
			panic(todo("", n.Position(), n.Case))
		case cc.SelectionStatementIfElse: // "if" '(' Expression ')' Statement "else" Statement
			panic(todo("", n.Position(), n.Case))
		case cc.SelectionStatementSwitch: // "switch" '(' Expression ')' Statement
			sv := ctx.switchCtx
			ctx.switchCtx = inSwitchFirst
			g.w("switch ")
			g.expression(ctx, n.Expression, n.Expression.Operand.Type())
			g.statement(ctx, n.Statement, "q\n", true)
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
		switch n.Case {
		case cc.IterationStatementWhile: // "while" '(' Expression ')' Statement
			g.w("for ")
			g.expression(ctx, n.Expression, n.Expression.Operand.Type())
			g.w(" != 0 ")
			g.statement(ctx, n.Statement, "", true)
		case cc.IterationStatementDo: // "do" Statement "while" '(' Expression ')' ';'
			v := ctx.scope.take("ok")
			g.w("for %v := true; %[1]v; %[1]v = ", v)
			g.expression(ctx, n.Expression, n.Expression.Operand.Type())
			g.w(" != 0")
			g.statement(ctx, n.Statement, "", true)
		case cc.IterationStatementFor: // "for" '(' Expression ';' Expression ';' Expression ')' Statement
			g.w("for ")
			g.expression(ctx, n.Expression, void)
			g.w("; ")
			g.expression(ctx, n.Expression2, n.Expression2.Operand.Type())
			g.w(" != 0; ")
			g.expression(ctx, n.Expression3, void)
			g.statement(ctx, n.Statement, "", true)
		// case cc.IterationStatementForDecl: // "for" '(' Declaration Expression ';' Expression ')' Statement
		// 	panic(todo("", n.Position()))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (g *gen) jumpStatement(ctx *context, n *cc.JumpStatement, trailer string) (r *cc.JumpStatement) {
	switch n.Case {
	// case cc.JumpStatementGoto: // "goto" IDENTIFIER ';'
	// 	panic(todo("", n.Position()))
	// case cc.JumpStatementGotoExpr: // "goto" '*' Expression ';'
	// 	panic(todo("", n.Position()))
	// case cc.JumpStatementContinue: // "continue" ';'
	// 	panic(todo("", n.Position()))
	case cc.JumpStatementBreak: // "break" ';'
		switch ctx.switchCtx {
		case inSwitchCase:
			ctx.switchCtx = inSwitchSeenBreak
			return
		default:
			panic(todo("", n.Position(), ctx.switchCtx))
		}
		g.w("break%s", trailer) //TODO need .Context from cc
	case cc.JumpStatementReturn: // "return" Expression ';'
		r = n
		g.w("return ")
		g.expression(ctx, n.Expression, ctx.rt)
		g.w("%s", trailer)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (g *gen) expressionStatement(ctx *context, n *cc.ExpressionStatement) {
	// Expression AttributeSpecifierList ';'
	g.expression(ctx, n.Expression, void)
}

func (g *gen) expression(ctx *context, n *cc.Expression, t cc.Type) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		g.assignmentExpression(ctx, n.AssignmentExpression, t)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) assignmentExpression(ctx *context, n *cc.AssignmentExpression, t cc.Type) {
	switch {
	case t.Kind() == cc.Void:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			g.conditionalExpression(ctx, n.ConditionalExpression, t)
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			g.unaryExpression(ctx, n.UnaryExpression, n.UnaryExpression.Operand.Type())
			g.w(" = ")
			g.assignmentExpression(ctx, n.AssignmentExpression, n.UnaryExpression.Operand.Type())
		// case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	default:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			g.conditionalExpression(ctx, n.ConditionalExpression, t)
		// case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		// case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		// 	panic(todo("", n.Position()))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (g *gen) declarator(ctx *context, d *cc.Declarator, t cc.Type) {
	defer g.w("%s", convert(d.Type(), t))

	switch d.Linkage {
	case cc.None:
		switch local := ctx.locals[d]; {
		case local.isPinned:
			panic(todo(""))
		default:
			g.w("%s", local.name)
		}
	case cc.External:
		switch tld := g.tlds[d]; {
		case tld != nil:
			g.w("%s", tld.name)
		default:
			nm := d.Name()
			imp := g.imports[nm.String()]
			g.w("%sX%s", imp.qualifier, nm)
		}
	default:
		panic(todo("", d.Position(), d.Name(), d.Linkage))
	}

	//TODO- g.w("%s", g.mangleDeclarator(ctx, d))
}

func convert(from, to cc.Type) string {
	if from.IsIntegerType() && from.Kind() == to.Kind() {
		return ""
	}

	switch from.Kind() {
	case cc.Function, cc.Struct, cc.Union, cc.Array:
		if from.Kind() == to.Kind() {
			return ""
		}
	}

	panic(todo("%q -> %q", from, to))
}

func (g *gen) conditionalExpression(ctx *context, n *cc.ConditionalExpression, t cc.Type) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		g.logicalOrExpression(ctx, n.LogicalOrExpression, t)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) logicalOrExpression(ctx *context, n *cc.LogicalOrExpression, t cc.Type) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		g.logicalAndExpression(ctx, n.LogicalAndExpression, t)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) logicalAndExpression(ctx *context, n *cc.LogicalAndExpression, t cc.Type) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		g.inclusiveOrExpression(ctx, n.InclusiveOrExpression, t)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) inclusiveOrExpression(ctx *context, n *cc.InclusiveOrExpression, t cc.Type) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		g.exclusiveOrExpression(ctx, n.ExclusiveOrExpression, t)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) exclusiveOrExpression(ctx *context, n *cc.ExclusiveOrExpression, t cc.Type) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		g.andExpression(ctx, n.AndExpression, t)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) andExpression(ctx *context, n *cc.AndExpression, t cc.Type) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		g.equalityExpression(ctx, n.EqualityExpression, t)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) equalityExpression(ctx *context, n *cc.EqualityExpression, t cc.Type) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		g.relationalExpression(ctx, n.RelationalExpression, t)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo("", n.Position()))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) relationalExpression(ctx *context, n *cc.RelationalExpression, t cc.Type) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		g.shiftExpression(ctx, n.ShiftExpression, t)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		g.binaryRelationalExpression(ctx, n, " < ", t)
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		g.binaryRelationalExpression(ctx, n, " > ", t)
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		g.binaryRelationalExpression(ctx, n, " <= ", t)
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		g.binaryRelationalExpression(ctx, n, " >= ", t)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryRelationalExpression(ctx *context, n *cc.RelationalExpression, oper string, t cc.Type) {
	defer g.w("%s", convertOp(n.Operand, t))

	g.w("%sBool32(", g.task.crt)
	g.relationalExpression(ctx, n.RelationalExpression, n.Promote())
	g.w("%s", oper)
	g.shiftExpression(ctx, n.ShiftExpression, n.Promote())
	g.w(")")
}

func convertOp(op cc.Operand, to cc.Type) string {
	from := op.Type()
	if from.IsIntegerType() && from.Kind() == to.Kind() {
		return ""
	}

	panic(todo("%v %q -> %q", op.Value(), op.Type(), to))
}

func (g *gen) shiftExpression(ctx *context, n *cc.ShiftExpression, t cc.Type) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		g.additiveExpression(ctx, n.AdditiveExpression, t)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo("", n.Position()))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) additiveExpression(ctx *context, n *cc.AdditiveExpression, t cc.Type) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		g.multiplicativeExpression(ctx, n.MultiplicativeExpression, t)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		g.binaryAdditiveExpression(ctx, n, "+", t)
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		g.binaryAdditiveExpression(ctx, n, "-", t)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryAdditiveExpression(ctx *context, n *cc.AdditiveExpression, oper string, t cc.Type) {
	g.w("(")
	defer g.w("%s)", convertOp(n.Operand, t))

	switch {
	case n.AdditiveExpression.Operand.Type().IsArithmeticType() && n.MultiplicativeExpression.Operand.Type().IsArithmeticType():
		g.additiveExpression(ctx, n.AdditiveExpression, n.Promote())
		g.w("%s", oper)
		g.multiplicativeExpression(ctx, n.MultiplicativeExpression, n.Promote())
	default:
		panic(todo(""))
	}
}

func (g *gen) multiplicativeExpression(ctx *context, n *cc.MultiplicativeExpression, t cc.Type) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		g.castExpression(ctx, n.CastExpression, t)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		g.binaryMultiplicativeExpression(ctx, n, "*", t)
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		g.binaryMultiplicativeExpression(ctx, n, "/", t)
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		g.binaryMultiplicativeExpression(ctx, n, "%", t)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) binaryMultiplicativeExpression(ctx *context, n *cc.MultiplicativeExpression, oper string, t cc.Type) {
	g.w("(")
	defer g.w("%s)", convertOp(n.Operand, t))

	g.multiplicativeExpression(ctx, n.MultiplicativeExpression, n.Promote())
	g.w("%s", oper)
	g.castExpression(ctx, n.CastExpression, n.Promote())
}

func (g *gen) castExpression(ctx *context, n *cc.CastExpression, t cc.Type) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		g.unaryExpression(ctx, n.UnaryExpression, t)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) unaryExpression(ctx *context, n *cc.UnaryExpression, t cc.Type) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		g.postfixExpression(ctx, n.PostfixExpression, t)
		// case cc.UnaryExpressionInc: // "++" UnaryExpression
		// 	panic(todo("", n.Position()))
		// case cc.UnaryExpressionDec: // "--" UnaryExpression
		// 	panic(todo("", n.Position()))
		// case cc.UnaryExpressionAddrof: // '&' CastExpression
		// 	panic(todo("", n.Position()))
		// case cc.UnaryExpressionDeref: // '*' CastExpression
		// 	panic(todo("", n.Position()))
		// case cc.UnaryExpressionPlus: // '+' CastExpression
		// 	panic(todo("", n.Position()))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		g.w("-")
		g.castExpression(ctx, n.CastExpression, t)
	// case cc.UnaryExpressionCpl: // '~' CastExpression
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionNot: // '!' CastExpression
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
	// 	panic(todo("", n.Position()))
	// case cc.UnaryExpressionReal: // "__real__" UnaryExpression
	// 	panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) postfixExpression(ctx *context, n *cc.PostfixExpression, t cc.Type) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		g.primaryExpression(ctx, n.PrimaryExpression, t)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		g.postfixExpression(ctx, n.PostfixExpression, n.PostfixExpression.Operand.Type())
		g.w("[")
		g.expression(ctx, n.Expression, n.Expression.Operand.Type())
		g.w("]")
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		g.postfixExpression(ctx, n.PostfixExpression, n.PostfixExpression.Operand.Type())
		g.argumentExpressionList(ctx, n.PostfixExpression, n.ArgumentExpressionList, ctx.vaLists[n])
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		g.postfixExpression(ctx, n.PostfixExpression, n.PostfixExpression.Operand.Type())
		g.w(".F%s", n.Token2.Value)
		// case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		// 	panic(todo("", n.Position()))
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		g.incDecPostfixExpression(ctx, n, true, t)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		g.incDecPostfixExpression(ctx, n, false, t)
	// case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
	// 	panic(todo("", n.Position()))
	// case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
	// 	panic(todo("", n.Position()))
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

	if len(args) > len(params) && !isVariadic {
		panic(todo(""))
	}

	paren := ""
	for i, arg := range args {
		g.w(", ")
		switch {
		case i < len(params):
			g.assignmentExpression(ctx, arg, arg.Promote())
		case i == len(params):
			g.w("%sVaList(bp+%d, ", g.task.crt, bpOff)
			paren = ")"
			fallthrough
		default:
			g.assignmentExpression(ctx, arg, arg.Promote())
		}
	}
	if isVariadic && len(args) == len(params) {
		g.w(", 0")
	}
	g.w("%s)", paren)
}

func (g *gen) primaryExpression(ctx *context, n *cc.PrimaryExpression, t cc.Type) {
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
		// case cc.PrimaryExpressionFloat: // FLOATCONST
		// 	panic(todo("", n.Position()))
		// case cc.PrimaryExpressionEnum: // ENUMCONST
		// 	panic(todo("", n.Position()))
	case cc.PrimaryExpressionChar: // CHARCONST
		g.charConst(n.Token.Src.String(), n.Operand, t)
	// case cc.PrimaryExpressionLChar: // LONGCHARCONST
	// 	panic(todo("", n.Position()))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		g.w("%s", g.stringLiteral(n.Operand.Value()))
	// case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
	// 	panic(todo("", n.Position()))
	// case cc.PrimaryExpressionExpr: // '(' Expression ')'
	// 	panic(todo("", n.Position()))
	// case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
	// 	panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (g *gen) stringLiteral(v cc.Value) string {
	switch x := v.(type) {
	case cc.StringValue:
		s := cc.StringID(x).String()
		off, ok := g.strings[s]
		if !ok {
			off = g.ts.Len()
			g.ts.WriteString(s)
			g.ts.WriteByte(0)
			g.strings[s] = off
		}
		return fmt.Sprintf("%s+%d", g.tsNameP, off)
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
		g.w("%sfunc %s(tls *%sTLS, _ int32, _ uintptr) int32", docComment(ctx.fndef.DeclarationSpecifiers, ctx.fndef.Declarator), tld.name, g.task.crt)
	default:
		g.w("%s", docComment(ctx.fndef.DeclarationSpecifiers, ctx.fndef.Declarator))
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

// func (g *gen) mangleDeclarator(ctx *context, d *cc.Declarator) string {
// 	s := d.Name().String()
// 	switch d.Linkage {
// 	case cc.External:
// 		panic(todo(""))
// 		// if _, found := g.tlds[d]; found {
// 		// 	return fmt.Sprintf("X%s", s)
// 		// }
//
// 		// if imported := g.imports[s]; imported != nil {
// 		// 	return fmt.Sprintf("%s.X%s", imported.name, s)
// 		// }
//
// 		panic(todo("%q", s))
// 	default:
// 		panic(todo("", s, d.Linkage))
// 	}
// }

func docComment(n ...cc.Node) (r string) {
	for _, v := range n {
		if s := sep(v); s != "" {
			r = s
			break
		}
	}
	if !strings.Contains(r, "\n") {
		r += "\n"
	}
	return r
}

func sep(n cc.Node) (r string) {
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
				g.structs[tag].emit(g, docComment(n.DeclarationSpecifiers))
			}
			return false
		})
	}

	// DeclarationSpecifiers InitDeclaratorList ';'
	for list := n.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
		g.initDeclarator(ctx, n.DeclarationSpecifiers, list.InitDeclarator)
	}
}

func (g *gen) initDeclarator(ctx *context, ds *cc.DeclarationSpecifiers, n *cc.InitDeclarator) {
	if ctx == nil {
		g.tld(ctx, ds, n)
		return
	}

	d := n.Declarator
	local := ctx.locals[d]
	if local == nil { // Dead declarator
		return
	}

	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		if ctx.block.topDecl {
			return
		}

		g.w("var %s %s\n", local.name, g.typ(d.Type()))
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		switch {
		case ctx.block.topDecl:
			panic(todo(""))
		default:
			g.w("%s := %s(", local.name, g.typ(d.Type()))
			g.initializer(ctx, n.Initializer, d.Type())
			g.w(")\n")
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
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
	tld := g.tlds[d]
	if tld == nil {
		return
	}

	panic(todo("", d.Position(), d.Name(), d.Type(), d.Linkage, d.Read, d.Write))
}
