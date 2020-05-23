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
)

type exprMode int

const (
	_           exprMode = iota
	exprAddrOf           // &foo
	exprBool             // foo != 0
	exprLValue           // foo in foo = bar
	exprPSelect          // foo in foo->bar
	exprSelect           // foo in foo.bar
	exprValue            // foo
	exprVoid             //
)

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
	inSwitchSeenBreak                    // In switch "case/default" and seen "break/return".
)

type function struct {
	block     *block
	blocks    map[*cc.CompoundStatement]*block
	bpName    string
	fndef     *cc.FunctionDefinition
	gen       *project
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

func newFunction(p *project, n *cc.FunctionDefinition) *function {
	d := n.Declarator
	t := d.Type()
	rt := t.Result()
	params := t.Parameters()
	var mainSignatureForced bool
	var ignore map[*cc.Declarator]bool
	if d.Name() == idMain && d.Linkage == cc.External {
		if rt.Kind() != cc.Int {
			rt = p.task.cfg.ABI.Type(cc.Int)
		}
		if len(params) != 2 {
			mainSignatureForced = true
			d1 := newDeclarator("argc")
			t1 := p.task.cfg.ABI.Type(cc.Int)
			d2 := newDeclarator("argv")
			t2 := p.task.cfg.ABI.Ptr(n, p.task.cfg.ABI.Type(cc.Void))
			params = []*cc.Parameter{
				cc.NewParameter(d1, t1),
				cc.NewParameter(d2, t2),
			}
			ignore = map[*cc.Declarator]bool{d1: true, d2: true}
		}
	}
	f := &function{
		blocks:              map[*cc.CompoundStatement]*block{},
		fndef:               n,
		gen:                 p,
		locals:              map[*cc.Declarator]*local{},
		mainSignatureForced: mainSignatureForced,
		params:              params,
		rt:                  rt,
		scope:               newScope(),
		vaLists:             map[*cc.PostfixExpression]uintptr{},
		ignore:              ignore,
	}
	f.scope.take("tls")
	f.layoutLocals(n.CompoundStatement, params)
	for _, v := range n.CompoundStatements() {
		f.renameLocals(v)
	}
	f.staticAllocs(n.CompoundStatement)
	return f
}

func newDeclarator(name string) *cc.Declarator {
	return &cc.Declarator{
		DirectDeclarator: &cc.DirectDeclarator{
			Case:  cc.DirectDeclaratorIdent,
			Token: cc.Token{Rune: cc.IDENTIFIER, Value: cc.String(name)},
		},
	}
}

func (f *function) staticAllocs(n *cc.CompoundStatement) {
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
			case cc.Array, cc.Ptr, cc.Double, cc.Float:
				need += 8
			default:
				panic(todo("", pos(x), t, t.Kind()))
			}
		}
		if need != 0 {
			f.vaLists[x] = roundup(f.off, 8)
			f.off += need
		}
		return true
	})
}

func (f *function) layoutLocals(n *cc.CompoundStatement, params []*cc.Parameter) {
	block := newBlock(n, n.Declarations(), params, n.IsJumpTarget())
	f.blocks[n] = block
	for _, v := range n.Children() {
		f.layoutLocals(v, nil)
		if len(v.LabeledStatements()) != 0 {
			vb := f.blocks[v]
			block.decls = append(block.decls, vb.decls...)
			vb.decls = nil
		}
	}
}

func (f *function) renameLocals(n *cc.CompoundStatement) {
	block := f.blocks[n]
	type item struct {
		ds *cc.DeclarationSpecifiers
		d  *cc.Declarator
	}
	var work []item
	for _, v := range block.params {
		if v.Type().Kind() == cc.Void {
			break
		}

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
		if !f.ignore[d] && d.IsStatic() {
			continue
		}

		local := &local{undeclare: d.Read == 0}
		f.locals[d] = local
		if d.AddressTaken {
			local.isPinned = true
			local.off = roundup(f.off, uintptr(d.Type().Align()))
			f.off = local.off + d.Type().Size()
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

func (s *taggedStruct) emit(p *project, ds *cc.DeclarationSpecifiers) {
	if s == nil || s.emitted {
		return
	}

	s.emitted = true
	p.w("%stype %s = %s;\n\n", comment("\n", ds), s.name, s.gotyp)
}

type project struct {
	buf     bytes.Buffer
	errors  scanner.ErrorList
	imports map[string]*imported // C name: import info
	scope   scope
	structs map[cc.StringID]*taggedStruct // key: C tag
	task    *task
	tlds    map[*cc.Declarator]*tld
	ts      bytes.Buffer // Text segment
	tsName  string
	tsNameP string
	tsOffs  map[string]uintptr

	isMain bool
}

func newProject(t *task) (*project, error) {
	p := &project{
		imports: map[string]*imported{},
		scope:   newScope(),
		tsOffs:  map[string]uintptr{},
		task:    t,
		tlds:    map[*cc.Declarator]*tld{},
	}
	if err := p.layout(); err != nil {
		return nil, err
	}

	for _, v := range t.imports {
		var err error
		if v.name, v.exports, err = t.capi(v.path); err != nil {
			return nil, err
		}

		v.qualifier = p.scope.take(v.name) + "."
		for k := range v.exports {
			if p.imports[k] == nil {
				p.imports[k] = v
			}
		}
	}
	p.tsNameP = p.scope.take("ts")
	p.tsName = p.scope.take("ts")
	return p, nil
}

func (p *project) err(n cc.Node, s string, args ...interface{}) {
	p.errors.Add(token.Position(n.Position()), fmt.Sprintf(s, args...))
}

func (p *project) Err() error {
	if len(p.errors) == 0 {
		return nil
	}

	var lpos token.Position
	w := 0
	for _, v := range p.errors {
		if lpos.Filename != "" {
			if v.Pos.Filename == lpos.Filename && v.Pos.Line == lpos.Line {
				continue
			}
		}

		p.errors[w] = v
		w++
		lpos = v.Pos
	}
	p.errors = p.errors[:w]
	sort.Slice(p.errors, func(i, j int) bool {
		a := p.errors[i]
		b := p.errors[j]
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
	a := make([]string, 0, len(p.errors))
	for _, v := range p.errors {
		a = append(a, v.Error())
	}
	return fmt.Errorf("%s", strings.Join(a, "\n"))
}

func (p *project) layout() error {
	if err := p.layoutTLDs(); err != nil {
		return err
	}

	if err := p.layoutStructs(); err != nil {
		return err
	}

	return p.layoutStaticLocals()
}

func (p *project) layoutStructs() error {
	m := map[cc.StringID]*taggedStruct{}
	for _, v := range p.task.asts {
		for tag, v := range v.StructTypes {
			s := m[tag]
			if s == nil {
				m[tag] = &taggedStruct{name: p.scope.take("S" + tag.String()), ctyp: v}
				continue
			}

			if v.String() != s.ctyp.String() { // Conflict, cannot use named struct/union types.
				return nil
			}
		}
	}

	p.structs = m
	for _, v := range m {
		v.gotyp = p.structType(v.ctyp)
	}
	return nil
}

func (p *project) structType(t cc.Type) string {
	switch t.Kind() {
	case cc.Struct, cc.Union:
		tag := t.Tag()
		if tag != 0 && p.structs != nil {
			s := p.structs[tag]
			if s.gotyp == "" {
				s.gotyp = p.structLiteral(t)
			}
			return s.gotyp
		}

		return p.structLiteral(t)
	default:
		panic(todo("internal error: %v", t.Kind()))
	}
}

func (p *project) structLiteral(t cc.Type) string {
	nf := t.NumField()
	idx := []int{0}
	var b strings.Builder
	switch t.Kind() {
	case cc.Struct:
		b.WriteString("struct {")
		for idx[0] = 0; idx[0] < nf; idx[0]++ {
			f := t.FieldByIndex(idx)
			fmt.Fprintf(&b, "F%s %s;", f.Name(), p.typ(f.Type()))
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

func (p *project) layoutStaticLocals() error {
	for _, v := range p.task.asts {
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

func (p *project) layoutTLDs() error {
	var a []*cc.Declarator
	externs := map[cc.StringID]*tld{}
	for _, v := range p.task.asts {
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
				p.isMain = true
			}
			if !isMain && (d.Read == 0 && d.Write == 0 || d.IsExtern()) {
				continue
			}

			nm := d.Name()
			switch d.Linkage {
			case cc.External:
				if _, ok := externs[nm]; !ok { // First definition.
					name := fmt.Sprintf("X%s", d.Name())
					p.scope.take(name)
					tld := &tld{name: name, hasInitilaizer: d.HasInitializer()}
					externs[nm] = tld
					p.tlds[d] = tld
					break
				}

				panic(todo(""))
			case cc.Internal:
				name := nm.String()
				if token.IsExported(name) {
					name = "s" + name
				}
				p.tlds[d] = &tld{name: p.scope.take(name), hasInitilaizer: d.HasInitializer()}
			default:
				panic(todo("", d.Linkage))
			}
		}
	}
	return nil
}

func (p *project) o(s string, args ...interface{}) {
	if oTraceG {
		fmt.Printf(s, args...)
	}
	fmt.Fprintf(p.task.out, s, args...)
}

func (p *project) w(s string, args ...interface{}) {
	if oTraceW {
		fmt.Printf(s, args...)
	}
	fmt.Fprintf(&p.buf, s, args...)
}

func (p *project) main() error {
	p.o(`// Code generated by '%s %s', DO NOT EDIT.
	
package %s
	
`,
		filepath.Base(p.task.args[0]),
		strings.Join(p.task.args[1:], " "),
		p.task.pkgName,
	)
	for _, v := range p.task.asts {
		p.oneAST(v)
	}
	sort.Slice(p.task.imports, func(i, j int) bool { return p.task.imports[i].path < p.task.imports[j].path })
	p.o(`import (
	"reflect"
	"unsafe"
`)
	first := true
	for _, v := range p.task.imports {
		if v.used {
			if first {
				p.o("\n")
				first = false
			}
			p.o("\t%q\n", v.path)
		}
	}
	p.o(`)
	
var _ unsafe.Pointer
var _ reflect.Kind
`)
	if p.isMain {
		p.o(`
func main() { %sStart(Xmain) }`, p.task.crt)
	}
	p.flushStructs()
	p.flushTS()
	if _, err := p.buf.WriteTo(p.task.out); err != nil {
		return err
	}

	return p.Err()
}

func (p *project) flushStructs() {
	var a []*taggedStruct
	for _, v := range p.structs {
		if !v.emitted {
			a = append(a, v)
		}
	}
	sort.Slice(a, func(i, j int) bool { return a[i].name < a[j].name })
	for _, v := range a {
		v.emit(p, nil)
	}
}

func (p *project) flushTS() {
	b := p.ts.Bytes()
	if len(b) == 0 {
		return
	}

	s := strings.TrimSpace(hex.Dump(b))
	a := strings.Split(s, "\n")
	p.w("//  %s\n", strings.Join(a, "\n//  "))
	p.w("var %s = %q\n", p.tsName, b)
	p.w("var %s = (*reflect.StringHeader)(unsafe.Pointer(&%s)).Data\n", p.tsNameP, p.tsName)
}

func (p *project) oneAST(ast *cc.AST) {
	for list := ast.TranslationUnit; list != nil; list = list.TranslationUnit {
		p.externalDeclaration(list.ExternalDeclaration)
	}
	p.w("\n%s\n", ast.TrailingSeperator)
}

func (p *project) externalDeclaration(n *cc.ExternalDeclaration) {
	switch n.Case {
	case cc.ExternalDeclarationFuncDef: // FunctionDefinition
		p.functionDefinition(n.FunctionDefinition)
	case cc.ExternalDeclarationDecl: // Declaration
		p.declaration(nil, n.Declaration)
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

func (p *project) functionDefinition(n *cc.FunctionDefinition) {
	// DeclarationSpecifiers Declarator DeclarationList CompoundStatement
	d := n.Declarator
	tld := p.tlds[d]
	if tld == nil {
		return
	}

	f := newFunction(p, n)
	p.functionDefinitionSignature(f, tld)
	p.w(" ")
	if need := f.off; need != 0 {
		f.bpName = f.scope.take("bp")
		p.w("{\n%s := tls.Alloc(%d)\n", f.bpName, need)
		p.w("defer tls.Free(%d)\n", need)
	}
	p.compoundStatement(f, n.CompoundStatement, false)
	p.w("\n\n")
}

func (p *project) compoundStatement(f *function, n *cc.CompoundStatement, forceNoBraces bool) {
	// '{' BlockItemList '}'
	brace := (!n.IsJumpTarget() || n.Parent() == nil) && !forceNoBraces
	if brace && (n.Parent() != nil || f.off == 0) {
		p.w("{")
	}
	f.block = f.blocks[n]
	if f.block.topDecl {
		panic(todo(""))
	}
	var r *cc.JumpStatement
	for list := n.BlockItemList; list != nil; list = list.BlockItemList {
		r = p.blockItem(f, list.BlockItem)
	}
	if n.Parent() == nil && r == nil && f.rt.Kind() != cc.Void {
		p.w("\nreturn ")
		p.w("0%s", p.convert(p.task.cfg.ABI.Type(cc.Int), f.rt))
	}
	if brace {
		p.w("}")
	}
}

func (p *project) blockItem(f *function, n *cc.BlockItem) (r *cc.JumpStatement) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		p.declaration(f, n.Declaration)
	case cc.BlockItemStmt: // Statement
		r = p.statement(f, n.Statement, false)
		p.w(";")
	case cc.BlockItemLabel: // LabelDeclaration
		panic(todo("", n.Position()))
		p.w(";")
	case cc.BlockItemFuncDef: // DeclarationSpecifiers Declarator CompoundStatement
		panic(todo("", n.Position()))
		p.w(";")
	case cc.BlockItemPragma: // PragmaSTDC
		panic(todo("", n.Position()))
		p.w(";")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) statement(f *function, n *cc.Statement, forceCompoundStmtBrace bool) (r *cc.JumpStatement) {
	if f.switchCtx == inSwitchSeenBreak && n.Case != cc.StatementLabeled {
		return nil
	}

	if forceCompoundStmtBrace {
		p.w(" {")
	}
	switch n.Case {
	case cc.StatementLabeled: // LabeledStatement
		p.labeledStatement(f, n.LabeledStatement)
	case cc.StatementCompound: // CompoundStatement
		p.compoundStatement(f, n.CompoundStatement, forceCompoundStmtBrace)
	case cc.StatementExpr: // ExpressionStatement
		p.expressionStatement(f, n.ExpressionStatement)
	case cc.StatementSelection: // SelectionStatement
		p.selectionStatement(f, n.SelectionStatement)
	case cc.StatementIteration: // IterationStatement
		p.iterationStatement(f, n.IterationStatement)
	case cc.StatementJump: // JumpStatement
		r = p.jumpStatement(f, n.JumpStatement)
	case cc.StatementAsm: // AsmStatement
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if forceCompoundStmtBrace {
		p.w("}")
	}
	return r
}

func (p *project) labeledStatement(f *function, n *cc.LabeledStatement) {
	switch {
	case f.block.block.IsJumpTarget():
		panic(todo(""))
	default:
		switch n.Case {
		case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
			panic(todo("", n.Position(), n.Case))
		case cc.LabeledStatementCaseLabel: // "case" ConstantExpression ':' Statement
			switch f.switchCtx {
			case inSwitchFirst:
				f.switchCtx = inSwitchCase
			case inSwitchCase:
				p.w("\nfallthrough;")
			case inSwitchSeenBreak:
				f.switchCtx = inSwitchCase
			default:
				panic(todo("", n.Position(), f.switchCtx))
			}
			p.w("%scase ", comment("\n", n))
			p.constantExpression(f, n.ConstantExpression, n.ConstantExpression.Operand.Type(), exprValue, true)
			p.w(":")
			p.statement(f, n.Statement, false)
		case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
			panic(todo("", n.Position(), n.Case))
		case cc.LabeledStatementDefault: // "default" ':' Statement
			switch f.switchCtx {
			case inSwitchFirst:
				f.switchCtx = inSwitchCase
			case inSwitchCase:
				p.w("\nfallthrough;")
			case inSwitchSeenBreak:
				f.switchCtx = inSwitchCase
			default:
				panic(todo("", n.Position(), f.switchCtx))
			}
			p.w("%sdefault:", comment("\n", n))
			p.statement(f, n.Statement, false)
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (p *project) constantExpression(f *function, n *cc.ConstantExpression, t cc.Type, mode exprMode, outermost bool) {
	// ConditionalExpression
	p.conditionalExpression(f, n.ConditionalExpression, t, mode, outermost)
}

func (p *project) selectionStatement(f *function, n *cc.SelectionStatement) {
	switch {
	case f.block.block.IsJumpTarget():
		panic(todo(""))
	default:
		p.w("%s", comment("\n", n))
		switch n.Case {
		case cc.SelectionStatementIf: // "if" '(' Expression ')' Statement
			p.w("if ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, true)
			p.statement(f, n.Statement, true)
		case cc.SelectionStatementIfElse: // "if" '(' Expression ')' Statement "else" Statement
			p.w("if ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, true)
			p.statement(f, n.Statement, true)
			p.w(" else ")
			p.statement(f, n.Statement2, true)
		case cc.SelectionStatementSwitch: // "switch" '(' Expression ')' Statement
			sv := f.switchCtx
			f.switchCtx = inSwitchFirst
			p.w("switch ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, true)
			p.statement(f, n.Statement, true)
			f.switchCtx = sv
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (p *project) iterationStatement(f *function, n *cc.IterationStatement) {
	sv := f.switchCtx
	f.switchCtx = 0

	defer func() { f.switchCtx = sv }()

	switch {
	case f.block.block.IsJumpTarget():
		panic(todo(""))
	default:
		p.w("%s", comment("\n", n))
		switch n.Case {
		case cc.IterationStatementWhile: // "while" '(' Expression ')' Statement
			p.w("for ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, true)
			p.statement(f, n.Statement, true)
		case cc.IterationStatementDo: // "do" Statement "while" '(' Expression ')' ';'
			v := f.scope.take("ok")
			p.w("for %v := true; %[1]v; %[1]v = ", v)
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, true)
			p.statement(f, n.Statement, true)
		case cc.IterationStatementFor: // "for" '(' Expression ';' Expression ';' Expression ')' Statement
			p.w("for ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, true)
			p.w("; ")
			p.expression(f, n.Expression2, n.Expression2.Operand.Type(), exprBool, true)
			p.w("; ")
			p.expression(f, n.Expression3, n.Expression3.Operand.Type(), exprVoid, true)
			p.statement(f, n.Statement, true)
		case cc.IterationStatementForDecl: // "for" '(' Declaration Expression ';' Expression ')' Statement
			panic(todo("", n.Position()))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (p *project) jumpStatement(f *function, n *cc.JumpStatement) (r *cc.JumpStatement) {
	p.w("%s", comment("\n", n))
	if _, ok := n.Context().(*cc.SelectionStatement); ok {
		switch f.switchCtx {
		case inSwitchCase:
			f.switchCtx = inSwitchSeenBreak
			if n.Case == cc.JumpStatementBreak {
				return
			}
		default:
			panic(todo("", n.Position(), f.switchCtx))
		}
	}

	switch n.Case {
	case cc.JumpStatementGoto: // "goto" IDENTIFIER ';'
		panic(todo("", n.Position()))
	case cc.JumpStatementGotoExpr: // "goto" '*' Expression ';'
		panic(todo("", n.Position()))
	case cc.JumpStatementContinue: // "continue" ';'
		panic(todo("", n.Position()))
	case cc.JumpStatementBreak: // "break" ';'
		p.w("break")
	case cc.JumpStatementReturn: // "return" Expression ';'
		r = n
		p.w("return ")
		if n.Expression != nil {
			p.expression(f, n.Expression, f.rt, exprValue, true)
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) expressionStatement(f *function, n *cc.ExpressionStatement) {
	p.w("%s", comment("\n", n))
	// Expression AttributeSpecifierList ';'
	p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, true)
}

func (p *project) expression(f *function, n *cc.Expression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, outermost)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpression(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, outermost bool) {
	switch {
	case mode == exprVoid:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			p.conditionalExpression(f, n.ConditionalExpression, n.ConditionalExpression.Operand.Type(), mode, outermost)
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), exprLValue, outermost)
			p.w(" = ")
			p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), exprValue, true)
		case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
			p.assignOp(f, n, t, mode, "*", outermost)
		case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
			p.assignOp(f, n, t, mode, "/", outermost)
		case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
			panic(todo("", n.Position()))
		case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
			p.assignOp(f, n, t, mode, "+", outermost)
		case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
			p.assignOp(f, n, t, mode, "-", outermost)
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
			p.conditionalExpression(f, n.ConditionalExpression, t, mode, outermost)
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			if d := n.UnaryExpression.Declarator(); d != nil {
				if local := f.locals[d]; local != nil {
					if local.isPinned {
						panic(todo("", n.Position()))
					}

					s := p.convert(d.Type(), n.Operand.Type())
					p.w("%sAssign%s(&%s, ", p.task.crt, p.helperType(d.Type()), local.name)
					p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), exprValue, true)
					p.w(")%s", s)
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

func (p *project) assignOp(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, oper string, outermost bool) {
	switch {
	case mode == exprVoid:
		if d := n.UnaryExpression.Declarator(); d != nil {
			p.declarator(f, d, d.Type(), exprLValue, true)
			p.w(" = ")
			s := p.convert(n.Promote(), d.Type())
			p.declarator(f, d, n.Promote(), exprValue, true)
			p.w(" %s ", oper)
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, false)
			p.w("%s", s)
			return
		}

		panic(todo(""))
	default:
		panic(todo(""))
	}
}

func (p *project) helperType(t cc.Type) string {
	s := p.typ(t)
	return strings.ToUpper(s[:1]) + s[1:]
}

func (p *project) convert(from, to cc.Type) string {
	if (from.IsArithmeticType() || from.IsScalarType()) && from.Kind() == to.Kind() {
		return ""
	}

	if from.IsIntegerType() {
		return p.convertInt(from, to)
	}

	switch from.Kind() {
	case cc.Function, cc.Struct, cc.Union, cc.Array, cc.Ptr:
		if from.Kind() == to.Kind() {
			return ""
		}
	case cc.Double, cc.Float:
		p.w("%s(", p.typ(to))
		return ")"
	}

	panic(todo("%q -> %q", from, to))
}

func (p *project) convertInt(from, to cc.Type) string {
	if to.IsArithmeticType() {
		p.w("%s(", p.typ(to))
		return ")"
	}

	panic(todo("%q -> %q", from, to))
}

func (p *project) conditionalExpression(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, outermost)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpression(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, outermost)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		if !outermost {
			p.w("(")
			defer p.w(")")
		}
		if mode != exprBool {
			defer p.w("%s", p.convert(n.Operand.Type(), t))

			p.w("%sBool32(", p.task.crt)

			defer p.w(")")
		}
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, false)
		p.w(" || ")
		p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), exprBool, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpression(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, outermost)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		if !outermost {
			p.w("(")
			defer p.w(")")
		}
		if mode != exprBool {
			defer p.w("%s", p.convert(n.Operand.Type(), t))

			p.w("%sBool32(", p.task.crt)

			defer p.w(")")
		}
		p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), exprBool, false)
		p.w(" && ")
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.InclusiveOrExpression.Operand.Type(), exprBool, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpression(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, outermost)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		if !outermost {
			p.w("(")
			defer p.w(")")
		}
		switch mode {
		case exprBool:
			p.w("(")
			defer p.w(") != 0")
		default:
			defer p.w("%s", p.convert(n.Operand.Type(), t))
		}
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), exprValue, false)
		p.w(" | ")
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpression(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, outermost)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		if !outermost {
			p.w("(")
			defer p.w(")")
		}
		switch mode {
		case exprBool:
			p.w("(")
			defer p.w(") != 0")
		default:
			defer p.w("%s", p.convert(n.Operand.Type(), t))
		}
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, false)
		p.w(" ^ ")
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpression(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, outermost)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		if !outermost {
			p.w("(")
			defer p.w(")")
		}
		switch mode {
		case exprBool:
			p.w("(")
			defer p.w(") != 0")
		default:
			defer p.w("%s", p.convert(n.Operand.Type(), t))
		}
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, false)
		p.w(" & ")
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpression(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, outermost)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		p.binaryEqualityExpression(f, n, " == ", t, mode, outermost)
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		p.binaryEqualityExpression(f, n, " != ", t, mode, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryEqualityExpression(f *function, n *cc.EqualityExpression, oper string, t cc.Type, mode exprMode, outermost bool) {
	if !outermost {
		p.w("(")
		defer p.w(")")
	}
	if mode != exprBool {
		defer p.w("%s", p.convert(n.Operand.Type(), t))

		p.w("%sBool32(", p.task.crt)

		defer p.w(")")
	}
	p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, false)
	p.w("%s", oper)
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), exprValue, false)
}

func (p *project) relationalExpression(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, outermost)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		p.binaryRelationalExpression(f, n, " < ", t, mode, outermost)
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		p.binaryRelationalExpression(f, n, " > ", t, mode, outermost)
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		p.binaryRelationalExpression(f, n, " <= ", t, mode, outermost)
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		p.binaryRelationalExpression(f, n, " >= ", t, mode, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryRelationalExpression(f *function, n *cc.RelationalExpression, oper string, t cc.Type, mode exprMode, outermost bool) {
	if !outermost {
		p.w("(")
		defer p.w(")")
	}
	if mode != exprBool {
		defer p.w("%s", p.convert(n.Operand.Type(), t))

		p.w("%sBool32(", p.task.crt)

		defer p.w(")")
	}
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), exprValue, false)
	p.w("%s", oper)
	p.shiftExpression(f, n.ShiftExpression, n.Promote(), exprValue, false)
}

func (p *project) shiftExpression(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, outermost)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		p.binaryShiftExpression(f, n, "<<", t, mode, outermost)
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		p.binaryShiftExpression(f, n, ">>", t, mode, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryShiftExpression(f *function, n *cc.ShiftExpression, oper string, t cc.Type, mode exprMode, outermost bool) {
	if !outermost {
		p.w("(")
		defer p.w(")")
	}
	switch mode {
	case exprBool:
		p.w("(")
		defer p.w(") != 0")
	default:
		defer p.w("%s", p.convert(n.Operand.Type(), t))
	}
	p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, false)
	p.w("%s", oper)
	p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, false)
}

func (p *project) additiveExpression(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, outermost)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "+", t, mode, outermost)
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "-", t, mode, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryAdditiveExpression(f *function, n *cc.AdditiveExpression, oper string, t cc.Type, mode exprMode, outermost bool) {
	if !outermost {
		p.w("(")
		defer p.w(")")
	}
	switch mode {
	case exprBool:
		p.w("(")
		defer p.w(") != 0")
	default:
		defer p.w("%s", p.convert(n.Operand.Type(), t))
	}
	switch {
	case n.AdditiveExpression.Operand.Type().IsArithmeticType() && n.MultiplicativeExpression.Operand.Type().IsArithmeticType():
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, false)
		p.w("%s", oper)
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, false)
	default:
		panic(todo(""))
	}
}

func (p *project) multiplicativeExpression(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, outermost)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		p.binaryMultiplicativeExpression(f, n, "*", t, mode, outermost)
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		p.binaryMultiplicativeExpression(f, n, "/", t, mode, outermost)
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		p.binaryMultiplicativeExpression(f, n, "%", t, mode, outermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryMultiplicativeExpression(f *function, n *cc.MultiplicativeExpression, oper string, t cc.Type, mode exprMode, outermost bool) {
	if !outermost {
		p.w("(")
		defer p.w(")")
	}
	switch mode {
	case exprBool:
		p.w("(")
		defer p.w(") != 0")
	default:
		defer p.w("%s", p.convert(n.Operand.Type(), t))
	}
	p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, false)
	p.w("%s", oper)
	p.castExpression(f, n.CastExpression, n.Promote(), exprValue, false)
}

func (p *project) castExpression(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, outermost)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		switch t := n.Operand.Type(); t.Kind() {
		case cc.Ptr:
			p.castExpression(f, n.CastExpression, t, exprValue, outermost)
		case cc.Void:
			p.castExpression(f, n.CastExpression, t, exprVoid, outermost)
		default:
			panic(todo("", n.Position(), t, t.Kind()))
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpression(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, outermost)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		p.castExpression(f, n.CastExpression, nil, exprAddrOf, false)
	case cc.UnaryExpressionDeref: // '*' CastExpression
		s := p.convert(n.CastExpression.Operand.Type().Elem(), t)
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, true)
		p.w("))%s", s)
	case cc.UnaryExpressionPlus: // '+' CastExpression
		p.w("+")
		p.castExpression(f, n.CastExpression, t, exprValue, outermost)
	case cc.UnaryExpressionMinus: // '-' CastExpression
		p.w("-")
		p.castExpression(f, n.CastExpression, t, exprValue, outermost)
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionNot: // '!' CastExpression
		p.w("%sBool32(!(", p.task.crt)
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprBool, true)
		p.w("))")
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		p.w("unsafe.Sizeof(")
		p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), exprValue, true)
		p.w(")")
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		t := n.TypeName.Type()
		s := "(0)"
		if !t.IsArithmeticType() {
			switch t.Kind() {
			case cc.Ptr:
				// ok
			default:
				panic(todo("", t.Kind()))
			}
		}
		p.w("unsafe.Sizeof(%s%s)", p.typ(t), s)
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

func (p *project) primaryExpression(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, outermost bool) {
	switch mode {
	case exprBool:
		if !outermost && n.Case != cc.PrimaryExpressionExpr {
			p.w("(")
			defer p.w(")")
		}

		defer p.w(" != 0")

		mode = exprValue
	}
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(f, d, t, mode, outermost)
		default:
			panic(todo("", n.Position()))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n.Token.Src.String(), n.Operand, t)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		p.floatConst(n.Token.Src.String(), n.Operand, t)
	case cc.PrimaryExpressionEnum: // ENUMCONST
		p.intConst(n.Token.Src.String(), n.Operand, t)
	case cc.PrimaryExpressionChar: // CHARCONST
		p.charConst(n.Token.Src.String(), n.Operand, t)
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", n.Position()))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		if !outermost {
			p.w("(")
			defer p.w(")")
		}
		p.expression(f, n.Expression, t, mode, true)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) floatConst(src string, op cc.Operand, to cc.Type) {
	bits := 64
	switch to.Kind() {
	case cc.Float:
		bits = 32
	}
	src = strings.TrimRight(src, "flFL")
	sn, err := strconv.ParseFloat(src, bits)
	snValid := err == nil
	switch x := op.Value().(type) {
	case cc.Float64Value:
		switch to.Kind() {
		case cc.Double:
			if snValid && sn == float64(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			panic(todo("", to))
		case cc.Float:
			if snValid && float32(sn) == float32(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			panic(todo("", to))
		default:
			defer p.w("%s", p.convert(op.Type(), to))

			if snValid && sn == float64(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			panic(todo("", to))
		}
	case cc.Float32Value:
		switch to.Kind() {
		case cc.Float:
			if snValid && float32(sn) == float32(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			panic(todo("", to))
		default:
			panic(todo("", to))
		}
	default:
		panic(todo("%T(%v)", x, x))
	}
}

func (p *project) declarator(f *function, d *cc.Declarator, t cc.Type, mode exprMode, outermost bool) {
	switch mode {
	case exprValue, exprLValue:
		p.declaratorValue(f, d, t, outermost)
	case exprAddrOf:
		p.declaratorAddrOf(f, d)
	case exprSelect:
		p.declaratorSelect(f, d)
	case exprPSelect:
		p.declaratorPSelect(f, d)
	default:
		panic(todo("", mode))
	}
}

func (p *project) declaratorValue(f *function, d *cc.Declarator, t cc.Type, outermost bool) {
	if d.Type().IsArithmeticType() {
		defer p.w("%s", p.convert(d.Type(), t))
	}
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			switch {
			case d.Type().Kind() == cc.Array && t.Kind() == cc.Ptr:
				if !outermost {
					p.w("(")
					defer p.w(")")
				}
				p.w("%s%s/* %s */", f.bpName, nonZeroUintptr(local.off), local.name)
			default:
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
			}
			return
		}

		p.w("%s", local.name)
		return
	}

	tld := p.tlds[d]
	switch {
	case tld != nil:
		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorAddrOf(f *function, d *cc.Declarator) {
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
			return
		}

		panic(todo("", d.Position()))
		return
	}

	tld := p.tlds[d]
	p.w("uintptr(unsafe.Pointer(&%s))", tld.name)
}

func (p *project) declaratorSelect(f *function, d *cc.Declarator) {
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
			return
		}

		p.w("%s", local.name)
		return
	}

	tld := p.tlds[d]
	p.w("%s", tld.name)
}

func (p *project) declaratorPSelect(f *function, d *cc.Declarator) {
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			panic(todo("", d.Position()))
			return
		}

		p.w("(*%s)(unsafe.Pointer(%s))", p.typ(d.Type().Elem()), local.name)
		return
	}

	// tld := p.tlds[d]
	panic(todo("", d.Position()))
}

func (p *project) postfixExpression(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, outermost bool) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, outermost)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		switch mode {
		case exprSelect, exprLValue, exprValue:
			p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprValue, false)
			p.w("[")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, true)
			p.w("]")
		case exprAddrOf:
			if !outermost {
				p.w("(")
				defer p.w(")")
			}
			p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), mode, false)
			p.w(" + uintptr(")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, true)
			p.w(")")
			if sz := n.PostfixExpression.Operand.Type().Decay().Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
		default:
			panic(todo("", mode))
		}
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprValue, false)
		p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		switch mode {
		case exprAddrOf:
			t := n.PostfixExpression.Operand.Type()
			var off uintptr
			fld, ok := t.FieldByName(n.Token2.Value)
			if !ok {
				p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
			} else {
				off = fld.Offset()
			}
			p.postfixExpression(f, n.PostfixExpression, nil, mode, outermost)
			if off != 0 {
				p.w("+%d", off)
			}
			p.w("/* &.%s */", n.Token2.Value)
		case exprValue, exprLValue:
			p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprSelect, false)
			p.w(".F%s", n.Token2.Value)
		default:
			panic(todo("", mode))
		}
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		switch mode {
		case exprValue:
			p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprPSelect, outermost)
			p.w(".F%s", n.Token2.Value)
		default:
			panic(todo("", mode))
		}
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postIncDecPostfixExpression(f, n, true, t, mode, outermost)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postIncDecPostfixExpression(f, n, false, t, mode, outermost)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", n.Position()))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postIncDecPostfixExpression(f *function, n *cc.PostfixExpression, up bool, t cc.Type, mode exprMode, outermost bool) {
	peType := n.PostfixExpression.Operand.Type()
	switch {
	case mode == exprVoid:
		p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprLValue, outermost)
		if peType.IsIntegerType() || peType.Kind() == cc.Ptr && p.incDelta(n, peType) == 1 {
			switch {
			case up:
				p.w("++")
			default:
				p.w("--")
			}
			return
		}

		panic(todo("", n.Position()))
	default:
		oper := "Dec"
		if up {
			oper = "Inc"
		}
		p.w("%sPost%s%s(&", p.task.crt, oper, p.helperType(t))
		p.postfixExpression(f, n.PostfixExpression, t, exprLValue, outermost)
		p.w(", %d)", p.incDelta(n.PostfixExpression, peType))
	}
}

func (p *project) incDelta(n cc.Node, t cc.Type) uintptr {
	if t.IsArithmeticType() {
		return 1
	}

	if t.Kind() == cc.Ptr {
		return t.Elem().Size()
	}

	panic(todo("", n.Position(), t.Kind()))
}

func (p *project) argumentExpressionList(f *function, pe *cc.PostfixExpression, n *cc.ArgumentExpressionList, bpOff uintptr) {
	p.w("(tls")
	ft := pe.Operand.Type()
	isVariadic := ft.IsVariadic()
	params := ft.Parameters()
	if len(params) == 1 && params[0].Type().Kind() == cc.Void {
		params = nil
	}
	var args []*cc.AssignmentExpression
	for ; n != nil; n = n.ArgumentExpressionList {
		args = append(args, n.AssignmentExpression)
	}
	if len(args) < len(params) {
		panic(todo(""))
	}

	va := true
	if len(args) > len(params) && !isVariadic {
		p.err(pe, "too many arguments")
		va = false
	}

	paren := ""
	for i, arg := range args {
		p.w(", ")
		switch {
		case i < len(params):
			p.assignmentExpression(f, arg, arg.Promote().Decay(), exprValue, true)
		case va && i == len(params):
			p.w("%sVaList(%s%s, ", p.task.crt, f.bpName, nonZeroUintptr(bpOff))
			paren = ")"
			fallthrough
		default:
			p.assignmentExpression(f, arg, arg.Promote().Decay(), exprValue, true)
		}
	}
	if isVariadic && len(args) == len(params) {
		p.w(", 0")
	}
	p.w("%s)", paren)
}

func nonZeroUintptr(n uintptr) string {
	if n == 0 {
		return ""
	}

	return fmt.Sprintf("%+d", n)
}

func (p *project) stringLiteral(v cc.Value) string {
	switch x := v.(type) {
	case cc.StringValue:
		s := cc.StringID(x).String()
		off, ok := p.tsOffs[s]
		if !ok {
			off = uintptr(p.ts.Len())
			p.ts.WriteString(s)
			p.ts.WriteByte(0)
			p.tsOffs[s] = off
		}
		return fmt.Sprintf("%s%s", p.tsNameP, nonZeroUintptr(off))
	default:
		panic(todo("%T", x))
	}
}

func (p *project) charConst(src string, op cc.Operand, to cc.Type) {
	switch {
	case to.IsArithmeticType():
		defer p.w("%s", p.convert(op.Type(), to))
	default:
		panic(todo("", op.Type(), to))
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
	case !to.IsIntegerType():
		// ok
		if rValid { // Prefer original form
			p.w("%s", src)
			return
		}

		p.w("%d", on)
		return
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
		p.w("%s", src)
		return
	}

	p.w("%d", mask&on)
}

func (p *project) intConst(src string, op cc.Operand, to cc.Type) {
	ptr := to.Kind() == cc.Ptr
	switch {
	case to.IsArithmeticType():
		defer p.w("%s", p.convert(op.Type(), to))
	case ptr:
		// ok
	default:
		panic(todo("", op.Type(), to))
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
	case ptr:
		switch to.Size() {
		case 4:
			mask = math.MaxUint32
		case 8:
			mask = math.MaxUint64
		default:
			panic(todo("", op.Type().Size()))
		}
	case !to.IsIntegerType():
		// ok
		if snValid { // Prefer original form
			p.w("%s", src)
			return
		}

		p.w("%d", on)
		return
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
		p.w("%s", src)
		return
	}

	p.w("%d", mask&on)
}

func (p *project) functionDefinitionSignature(f *function, tld *tld) {
	switch {
	case f.mainSignatureForced:
		p.w("%sfunc %s(tls *%sTLS, _ int32, _ uintptr) int32", comment("\n", f.fndef), tld.name, p.task.crt)
	default:
		p.w("%s", comment("\n", f.fndef))
		p.functionSignature(f, f.fndef.Declarator.Type(), tld.name)
	}
}

func (p *project) functionSignature(f *function, t cc.Type, nm string) {
	p.w("func")
	if nm != "" {
		p.w(" %s", nm)
	}
	p.w("(tls *%sTLS", p.task.crt)
	for _, v := range t.Parameters() {
		if v.Type().Kind() == cc.Void {
			break
		}

		nm := "_"
		if d := v.Declarator(); d != nil {
			if local := f.locals[d]; local != nil {
				nm = local.name
			}
		}
		p.w(", %s %s", nm, p.typ(v.Type()))
	}
	p.w(")")
	if rt := t.Result(); rt != nil && rt.Kind() != cc.Void {
		p.w(" %s", p.typ(rt))
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

func (p *project) declaration(f *function, n *cc.Declaration) {
	if n.InitDeclaratorList == nil {
		cc.Inspect(n.DeclarationSpecifiers, func(m cc.Node, entry bool) bool {
			x, ok := m.(*cc.StructOrUnionSpecifier)
			if !ok {
				return true
			}

			if tag := x.Token.Value; tag != 0 {
				p.structs[tag].emit(p, n.DeclarationSpecifiers)
			}
			return false
		})
	}

	// DeclarationSpecifiers InitDeclaratorList ';'
	sep := comment("\n", n)
	for list := n.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
		p.initDeclarator(f, list.InitDeclarator, sep)
		sep = "\n"
	}
}

func (p *project) initDeclarator(f *function, n *cc.InitDeclarator, sep string) {
	if f == nil {
		p.tld(f, n, sep)
		return
	}

	d := n.Declarator
	local := f.locals[d]
	if local == nil { // Dead declaration.
		return
	}

	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		if f.block.topDecl {
			return
		}

		if local.isPinned {
			p.w("%s// var %s %s at %s%s;", sep, local.name, p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off))
			return
		}

		p.w("%svar %s %s;", sep, local.name, p.typ(d.Type()))
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		if f.block.topDecl {
			return
		}

		var paren string
		p.w("%s", sep)
		p.declarator(f, d, d.Type(), exprLValue, true)
		switch {
		case local.isPinned:
			p.w(" = ")
			p.initializer(f, n.Initializer, d.Type())
			p.w(";")
		default:
			p.w(" := ")
			switch d.Type().Kind() {
			case cc.Ptr:
				// ok
			default:
				p.w("%s(", p.typ(d.Type()))
				paren = ")"
			}
			p.initializer(f, n.Initializer, d.Type())
			p.w("%s;", paren)
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if local.undeclare {
		p.w("_ = %s;", local.name)
	}
}

func (p *project) typ(t cc.Type) string {
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
	case cc.Double:
		return "float64"
	case cc.Float:
		return "float32"
	case cc.Array:
		n := t.Len()
		if n == 0 {
			panic(todo(""))
		}

		fmt.Fprintf(&b, "[%d]%s", n, p.typ(t.Elem()))
		return b.String()
	case cc.Struct, cc.Union:
		if tag := t.Tag(); tag != 0 {
			if s := p.structs[tag]; s != nil {
				return s.name
			}
		}

		return p.structType(t)
	}

	panic(todo("", t.Kind(), t))
}

func (p *project) tld(f *function, n *cc.InitDeclarator, sep string) {
	d := n.Declarator
	tld := p.tlds[d]
	if tld == nil { // Dead declaration.
		return
	}

	t := d.Type()
	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		p.w("%svar %s %s\t// %v:;", sep, tld.name, p.typ(t), pos(n))
		switch t.Kind() {
		case cc.Struct, cc.Union:
			p.structs[t.Tag()].emit(p, nil)
		}
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		if d.IsStatic() && d.Read == 0 && d.Write == 1 && n.Initializer.IsConst() { // Initialized with no side effects and unused.
			break
		}

		panic(todo("", d.Position(), d.Name()))
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
