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
	"math/big"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"modernc.org/cc/v3"
	"modernc.org/mathutil"
)

var (
	idMain  = cc.String("main")
	oTraceG bool
	oTraceW bool
)

type exprMode int

const (
	_           exprMode = iota
	exprAddrOf           // &foo as uinptr
	exprBool             // foo != 0
	exprLValue           // foo in foo = bar
	exprPSelect          // foo in foo->bar
	exprPointer          // &foo as unsafe.Pointer
	exprSelect           // foo in foo.bar
	exprValue            // foo
	exprVoid             //
)

type flags byte

const (
	fOutermost flags = 1 << iota
	fIntLiteral
	fForceConv
	fForceRuntimeConv
)

type imported struct {
	path      string              // Eg. "example.com/user/foo".
	name      string              // Eg. "foo" from "package foo".
	qualifier string              // Eg. "foo." or "foo2." if renamed due to name conflict.
	exports   map[string]struct{} // Eg. {"New": {}, "Close": {}, ...}.

	used bool
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

type tld struct {
	name string // Can differ from the original one.

	hasInitilaizer bool
}

type block struct {
	block      *cc.CompoundStatement
	decls      []*cc.Declaration // What to declare in this block.
	enumConsts map[cc.StringID]string
	enumSpecs  map[*cc.EnumSpecifier]*enumSpec
	params     []*cc.Parameter
	parent     *block
	scope      scope

	isFlat  bool
	topDecl bool // Declare locals at block start to avoid "jumps over declaration".
}

func newBlock(parent *block, n *cc.CompoundStatement, decls []*cc.Declaration, params []*cc.Parameter, scope scope, topDecl bool) *block {
	return &block{
		block:   n,
		decls:   decls,
		params:  params,
		parent:  parent,
		scope:   scope,
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
	block      *block
	blocks     map[*cc.CompoundStatement]*block
	bpName     string
	fndef      *cc.FunctionDefinition
	gen        *project
	ignore     map[*cc.Declarator]bool // Pseudo declarators
	labelNames map[cc.StringID]string
	labels     scope
	locals     map[*cc.Declarator]*local
	off        uintptr         // bp+off allocs
	params     []*cc.Parameter // May differ from what fndef says
	project    *project
	rt         cc.Type // May differ from what fndef says
	scope      scope
	switchCtx  switchState
	tlsName    string
	vaLists    map[*cc.PostfixExpression]uintptr

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
		project:             p,
		rt:                  rt,
		scope:               p.newScope(),
		vaLists:             map[*cc.PostfixExpression]uintptr{},
		ignore:              ignore,
	}
	f.tlsName = f.scope.take("tls")
	f.layoutLocals(nil, n.CompoundStatement, params)
	var extern []cc.StringID
	for _, v := range n.CompoundStatements() { // testdata/gcc-9.1.0/gcc/testsuite/gcc.c-torture/execute/scope-1.c
		for _, v := range v.Declarations() {
			for list := v.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
				if d := list.InitDeclarator.Declarator; d != nil && d.IsExtern() {
					extern = append(extern, d.Name())
				}
			}
		}
	}
	for _, v := range n.CompoundStatements() {
		block := f.blocks[v]
		for _, v := range extern {
			tld := f.project.externs[v]
			block.scope.take(tld.name)
		}
	}
	for _, v := range n.CompoundStatements() {
		f.layoutBlocks(v)
	}
	f.renameLabels()
	f.staticAllocsAndPinned(n.CompoundStatement)
	return f
}

func (f *function) renameLabels() {
	var a []cc.StringID
	for _, v := range f.fndef.Labels {
		if v.Case != cc.LabeledStatementLabel {
			continue
		}

		a = append(a, v.Token.Value)
	}
	if len(a) == 0 {
		return
	}
	sort.Slice(a, func(i, j int) bool { return a[i].String() < a[j].String() })
	f.labels = newScope()
	f.labelNames = map[cc.StringID]string{}
	for _, id := range a {
		f.labelNames[id] = f.labels.take(id.String())
	}
}

func (f *function) staticAllocsAndPinned(n *cc.CompoundStatement) {
	cc.Inspect(n, func(n cc.Node, entry bool) bool {
		if !entry {
			return true
		}

		switch x := n.(type) {
		case *cc.AssignmentExpression:
			switch x.Case {
			case cc.AssignmentExpressionAssign: // foo = bar
				if d := x.AssignmentExpression.Declarator(); d != nil { // bar
					if isArray(d.Type()) && !d.IsParameter {
						f.pin(d)
					}
				}
			}
		case *cc.Initializer:
			if x.AssignmentExpression == nil {
				break
			}

			if d := x.AssignmentExpression.Declarator(); d != nil && isArray(d.Type()) {
				f.pin(d)
			}
		}

		x, ok := n.(*cc.PostfixExpression)
		if !ok || x.Case != cc.PostfixExpressionCall {
			return true
		}

		ft := funcType(x.PostfixExpression.Operand.Type())
		for list := x.ArgumentExpressionList; list != nil; list = list.ArgumentExpressionList {
			if d := list.AssignmentExpression.Declarator(); d != nil {
				if isArray(d.Type()) && !d.IsParameter {
					f.pin(d)
				}
			}
		}
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
			va := roundup(f.off, 8)
			f.vaLists[x] = va
			f.off = va + need
		}
		return true
	})
}

func (f *function) mustFlatten(n *cc.CompoundStatement) bool {
	if !n.IsJumpTarget() {
		return false
	}

	for _, v := range n.Children() {
		if v.IsJumpTarget() {
			return true
		}
	}

	return false
}

func funcType(t cc.Type) cc.Type {
	if t.Kind() == cc.Ptr {
		t = t.Elem()
	}
	return t
}

func isArray(t cc.Type) bool { return t.Kind() == cc.Array }

func roundup(n, to uintptr) uintptr {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}

// Return n's position with path reduced to baseName(path).
func pos(n cc.Node) (r token.Position) {
	r = token.Position(n.Position())
	if r.IsValid() {
		r.Filename = filepath.Base(r.Filename)
	}
	return r
}

func (f *function) pin(d *cc.Declarator) {
	local := f.locals[d]
	if local == nil || local.isPinned {
		return
	}

	local.isPinned = true
	local.off = roundup(f.off, uintptr(d.Type().Align()))
	f.off = local.off + d.Type().Size()
}

func (f *function) layoutBlocks(n *cc.CompoundStatement) {
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
	var enumList []*cc.EnumSpecifier
	enums := map[*cc.EnumSpecifier]*enumSpec{}
	for _, decl := range block.decls {
		ds := decl.DeclarationSpecifiers
		cc.Inspect(ds, func(n cc.Node, entry bool) bool {
			if !entry {
				return true
			}

			x, ok := n.(*cc.EnumSpecifier)
			if !ok || x.Case != cc.EnumSpecifierDef {
				return true
			}

			enumList = append(enumList, x)
			enums[x] = &enumSpec{decl: decl, spec: x}
			return true
		})
		for list := decl.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
			work = append(work, item{ds, list.InitDeclarator.Declarator})
		}
	}
	if len(enums) != 0 {
		block.enumSpecs = enums
		block.enumConsts = map[cc.StringID]string{}
		for _, v := range enumList {
			for list := v.EnumeratorList; list != nil; list = list.EnumeratorList {
				en := list.Enumerator
				nm := en.Token.Value
				block.enumConsts[nm] = block.scope.take(nm.String())
			}
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
			f.pin(d)
		}
		local.name = block.scope.take(d.Name().String())
	}
	if !n.IsJumpTarget() {
		return
	}

	for _, v := range n.Children() {
		if v.IsJumpTarget() {
			block.isFlat = true
			return
		}
	}
}

func (f *function) layoutLocals(parent *block, n *cc.CompoundStatement, params []*cc.Parameter) {
	block := newBlock(parent, n, n.Declarations(), params, f.project.newScope(), n.IsJumpTarget())
	f.blocks[n] = block
	for _, v := range n.Children() {
		f.layoutLocals(block, v, nil)
		if len(v.LabeledStatements()) != 0 {
			vb := f.blocks[v]
			block.decls = append(block.decls, vb.decls...)
			vb.decls = nil
		}
	}
}

func newDeclarator(name string) *cc.Declarator {
	return &cc.Declarator{
		DirectDeclarator: &cc.DirectDeclarator{
			Case:  cc.DirectDeclaratorIdent,
			Token: cc.Token{Rune: cc.IDENTIFIER, Value: cc.String(name)},
		},
	}
}

type enumSpec struct {
	decl *cc.Declaration
	spec *cc.EnumSpecifier

	emitted bool
}

func (n *enumSpec) emit(p *project, enumConsts map[cc.StringID]string) {
	if n == nil || n.emitted {
		return
	}

	n.emitted = true
	p.w("%s", comment("\n", n.decl))
	p.w("const ( /* %v: */", pos(n.decl))
	for list := n.spec.EnumeratorList; list != nil; list = list.EnumeratorList {
		en := list.Enumerator
		p.w("%s%s = %v;", comment("\n", en), enumConsts[en.Token.Value], en.Operand.Value())
	}
	p.w(")")
}

type project struct {
	ast         *cc.AST
	buf         bytes.Buffer
	enumConsts  map[cc.StringID]string
	enumSpecs   map[*cc.EnumSpecifier]*enumSpec
	errors      scanner.ErrorList
	externs     map[cc.StringID]*tld
	imports     map[string]*imported // C name: import info
	mainName    string
	scope       scope
	staticQueue []*cc.InitDeclarator
	structs     map[cc.StringID]*taggedStruct // key: C tag
	task        *task
	tlds        map[*cc.Declarator]*tld
	ts          bytes.Buffer // Text segment
	ts4         []rune       // Text segment, alignment 4
	ts4Name     string
	ts4NameP    string
	ts4Offs     map[cc.StringID]uintptr
	tsName      string
	tsNameP     string
	tsOffs      map[cc.StringID]uintptr

	isMain bool
}

func newProject(t *task) (*project, error) {
	if t.cfg.ABI.Types[cc.Int].Size != 4 { // We're assuming wchar_t is int32.
		return nil, fmt.Errorf("unsupported C int size")
	}

	p := &project{
		enumConsts: map[cc.StringID]string{},
		externs:    map[cc.StringID]*tld{},
		imports:    map[string]*imported{},
		scope:      newScope(),
		task:       t,
		tlds:       map[*cc.Declarator]*tld{},
		ts4Offs:    map[cc.StringID]uintptr{},
		tsOffs:     map[cc.StringID]uintptr{},
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
	p.ts4NameP = p.scope.take("wtext")
	p.ts4Name = p.scope.take("wtext")
	return p, nil
}

func (p *project) newScope() scope {
	s := newScope()
	var b []cc.StringID
	for k := range p.structs {
		b = append(b, k)
	}
	sort.Slice(b, func(i, j int) bool { return b[i].String() < b[j].String() })
	for _, k := range b {
		s.take(p.structs[k].name)
	}
	b = b[:0]
	for k := range p.enumConsts {
		b = append(b, k)
	}
	sort.Slice(b, func(i, j int) bool { return b[i].String() < b[j].String() })
	for _, k := range b {
		s.take(p.enumConsts[k])
	}
	return s
}

func (p *project) err(n cc.Node, s string, args ...interface{}) {
	p.errors.Add(token.Position(n.Position()), fmt.Sprintf(s, args...))
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

func (p *project) layout() error {
	if err := p.layoutTLDs(); err != nil {
		return err
	}

	if err := p.layoutStructs(); err != nil {
		return err
	}

	if err := p.layoutEnums(); err != nil {
		return err
	}

	return p.layoutStaticLocals()
}

func (p *project) layoutEnums() error {
	m := map[cc.StringID]cc.Value{}
	var enumList []*cc.EnumSpecifier
	enums := map[*cc.EnumSpecifier]*enumSpec{}
	for _, v := range p.task.asts {
		for list := v.TranslationUnit; list != nil; list = list.TranslationUnit {
			decl := list.ExternalDeclaration
			switch decl.Case {
			case cc.ExternalDeclarationDecl: // Declaration
				// ok
			default:
				continue
			}

			cc.Inspect(decl.Declaration.DeclarationSpecifiers, func(n cc.Node, entry bool) bool {
				if !entry {
					return true
				}

				x, ok := n.(*cc.EnumSpecifier)
				if !ok || x.Case != cc.EnumSpecifierDef {
					return true
				}

				enumList = append(enumList, x)
				enums[x] = &enumSpec{decl: decl.Declaration, spec: x}
				for list := x.EnumeratorList; list != nil; list = list.EnumeratorList {
					en := list.Enumerator
					nm := en.Token.Value
					v := en.Operand.Value()
					if ex := m[nm]; ex != nil {
						if ex != v { // Conflict, cannot use named enumeration constants.
							enums = nil
							m = nil
							return false
						}

						continue
					}

					m[nm] = v
				}
				return true
			})
		}
	}

	for _, v := range enumList {
		for list := v.EnumeratorList; list != nil; list = list.EnumeratorList {
			en := list.Enumerator
			nm := en.Token.Value
			p.enumConsts[nm] = p.scope.take(nm.String())
		}
	}
	p.enumSpecs = enums
	return nil
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

					p.tlds[x] = &tld{name: p.scope.take(x.Name().String()), hasInitilaizer: x.HasInitializer()}
				}
				return true
			})
		}
	}
	return nil
}

func (p *project) layoutStructs() error {
	m := map[cc.StringID]*taggedStruct{}
	var tags []cc.StringID
	for _, v := range p.task.asts {
		for tag, v := range v.StructTypes {
			tags = append(tags, tag)
			s := m[tag]
			if s == nil {
				m[tag] = &taggedStruct{ctyp: v}
				continue
			}

			if v.String() != s.ctyp.String() { // Conflict, cannot use named struct/union types w/o tag renaming
				return nil
			}
		}
	}

	p.structs = m
	sort.Slice(tags, func(i, j int) bool { return tags[i].String() < tags[j].String() })
	for _, k := range tags {
		v := m[k]
		v.name = p.scope.take(k.String())
	}
	for _, k := range tags {
		v := m[k]
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
			ft := f.Type()
			if ft.IsBitFieldType() {
				//TODO panic(todo("bit fields not supported"))
			}

			fmt.Fprintf(&b, "%s %s;", p.fieldName(f.Name()), p.typ(ft))
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
			fmt.Fprintf(&b, "_ [%d]byte;", sz)
		case 2:
			fmt.Fprintf(&b, "_ [%d]int16;", sz/2)
		case 4:
			fmt.Fprintf(&b, "_ [%d]int32;", sz/4)
		case 8:
			fmt.Fprintf(&b, "_ [%d]int64;", sz/8)
		default:
			panic(todo("", al))
		}
		if pad := sz % al; pad != 0 {
			fmt.Fprintf(&b, "_ [%d]byte;", pad)
		}
		b.WriteByte('}')
	default:
		panic(todo("internal error: %v", t.Kind()))
	}
	return b.String()
}

func (p *project) fieldName(id cc.StringID) string {
	if !p.task.exportFieldsValid {
		s := id.String()
		if !reservedNames[s] {
			return s
		}

		return "__" + s
	}

	if s := p.task.exportFields; s != "" {
		return s + id.String()
	}

	return capitalize(id.String())
}

func (p *project) typ(t cc.Type) string {
	if t.IsAliasType() {
		if tld := p.tlds[t.AliasDeclarator()]; tld != nil {
			return tld.name
		}
	}

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
		//TODO- if n == 0 && t.LenExpr().Operand.Value() == nil {
		//TODO- 	panic(todo("", t))
		//TODO- }

		fmt.Fprintf(&b, "[%d]%s", n, p.typ(t.Elem()))
		return b.String()
	case cc.Struct, cc.Union:
		if tag := t.Tag(); tag != 0 {
			if s := p.structs[tag]; s != nil {
				if s.name == "" {
					panic(todo("internal error %q", tag))
				}

				return s.name
			}
		}

		return p.structType(t)
	}

	panic(todo("", t.Kind(), t))
}

func (p *project) layoutTLDs() error {
	const (
		doNotExport = iota
		exportCapitalize
		exportPrefix
	)

	var export int
	if p.task.exportExternValid {
		switch {
		case p.task.exportExtern != "":
			export = exportPrefix
		default:
			export = exportCapitalize
		}
	}
	var a []*cc.Declarator
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
				p.scope.take("main")
			}
			if !isMain && !d.IsTypedefName && (d.Read == 0 && d.Write == 0 || d.IsExtern()) || d.IsFunctionPrototype() {
				continue
			}

			nm := d.Name()
			switch d.Linkage {
			case cc.External:
				if _, ok := p.externs[nm]; !ok { // First definition.
					name := d.Name().String()
					switch export {
					case doNotExport:
						// nop
					case exportCapitalize:
						name = capitalize(name)
					case exportPrefix:
						name = p.task.exportExtern + name
					}
					name = p.scope.take(name)
					if isMain {
						p.mainName = name
					}
					tld := &tld{name: name, hasInitilaizer: d.HasInitializer()}
					p.externs[nm] = tld
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
			case cc.None:
				if d.IsTypedefName {
					name := nm.String()
					if strings.HasPrefix(name, "__") {
						break
					}

					p.tlds[d] = &tld{name: p.scope.take(name)}
					break
				}

				panic(todo(""))
			default:
				panic(todo("", d.Linkage))
			}
		}
	}
	return nil
}

func capitalize(s string) string {
	a := []rune(s)
	return strings.ToUpper(string(a[0])) + string(a[1:])
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
func main() { %sStart(%s) }`, p.task.crt, p.mainName)
	}
	p.flushStructs()
	p.flushTS()
	if _, err := p.buf.WriteTo(p.task.out); err != nil {
		return err
	}

	return p.Err()
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

func (p *project) flushTS() {
	b := p.ts.Bytes()
	if len(b) != 0 {
		s := strings.TrimSpace(hex.Dump(b))
		a := strings.Split(s, "\n")
		p.w("//  %s\n", strings.Join(a, "\n//  "))
		p.w("var %s = %q\n", p.tsName, b)
		p.w("var %s = (*reflect.StringHeader)(unsafe.Pointer(&%s)).Data\n", p.tsNameP, p.tsName)
	}
	if len(p.ts4) != 0 {
		p.w("var %s = [...]int32{", p.ts4Name)
		for _, v := range p.ts4 {
			p.w("%d, ", v)
		}
		p.w("}\n")
		p.w("var %s = unsafe.Pointer(&%s[0])\n", p.ts4NameP, p.ts4Name)
	}
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

func (p *project) oneAST(ast *cc.AST) {
	p.ast = ast
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
		p.declaration(nil, n.Declaration, false)
	case cc.ExternalDeclarationAsm: // AsmFunctionDefinition
		panic(todo("", pos(n)))
	case cc.ExternalDeclarationAsmStmt: // AsmStatement
		panic(todo("", pos(n)))
	case cc.ExternalDeclarationEmpty: // ';'
		panic(todo("", pos(n)))
	case cc.ExternalDeclarationPragma: // PragmaSTDC
		panic(todo("", pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) declaration(f *function, n *cc.Declaration, topDecl bool) {
	cc.Inspect(n.DeclarationSpecifiers, func(m cc.Node, entry bool) bool {
		switch x := m.(type) {
		case *cc.EnumSpecifier:
			if f != nil {
				f.block.enumSpecs[x].emit(p, f.block.enumConsts)
				break
			}

			p.enumSpecs[x].emit(p, p.enumConsts)
		case *cc.StructOrUnionSpecifier:
			if tag := x.Token.Value; tag != 0 {
				p.structs[tag].emit(p, n.DeclarationSpecifiers)
			}
		}
		return true
	})

	if n.InitDeclaratorList == nil {
		return
	}

	// DeclarationSpecifiers InitDeclaratorList ';'
	sep := comment("\n", n)
	for list := n.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
		p.initDeclarator(f, list.InitDeclarator, sep, topDecl)
		sep = "\n"
	}
}

func (p *project) initDeclarator(f *function, n *cc.InitDeclarator, sep string, topDecl bool) {
	if f == nil {
		p.tld(f, n, sep)
		return
	}

	d := n.Declarator
	if d.IsExtern() || d.IsTypedefName {
		return
	}

	if tld := p.tlds[d]; tld != nil { // static local
		p.staticQueue = append(p.staticQueue, n)
		return
	}

	local := f.locals[d]
	if local == nil { // Dead declaration.
		return
	}

	block := f.block
	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		if block.topDecl && !topDecl {
			return
		}

		p.initDeclaratorDecl(f, n, sep)
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		if f.block.topDecl {
			switch {
			case topDecl:
				p.initDeclaratorDecl(f, n, sep)
				if local.undeclare {
					p.w("_ = %s;", local.name)
				}
			default:
				p.w("%s = ", local.name)
				p.initializer(f, n.Initializer, d.Type())
				p.w(";")
			}
			return
		}

		p.w("%s", sep)
		switch {
		case local.isPinned:
			p.declarator(f, d, d.Type(), nil, exprLValue, fOutermost)
			p.w(" = ")
			p.initializer(f, n.Initializer, d.Type())
			p.w(";")
		default:
			p.w("var %s ", local.name)
			if !isAggregateType(d.Type()) {
				p.w("%s ", p.typ(d.Type()))
			}
			p.w("= ")
			p.initializer(f, n.Initializer, d.Type())
			p.w(";")
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if local.undeclare {
		p.w("_ = %s;", local.name)
	}
}

func typedef(n cc.Node) (r cc.StringID) {
	cc.Inspect(n, func(n cc.Node, entry bool) bool {
		if !entry {
			return true
		}

		if x, ok := n.(*cc.TypeSpecifier); ok && x.Case == cc.TypeSpecifierTypedefName {
			r = x.Token.Value
			return false
		}

		return true
	})
	return r
}

func (p *project) initDeclaratorDecl(f *function, n *cc.InitDeclarator, sep string) {
	d := n.Declarator
	local := f.locals[d]
	if local.isPinned {
		p.w("%s// var %s %s at %s%s;", sep, local.name, p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off))
		return
	}

	p.w("%svar %s %s;", sep, local.name, p.typ(d.Type()))
}

func (p *project) declarator(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue, exprLValue:
		p.declaratorValue(f, d, t, flags)
	case exprAddrOf:
		p.declaratorAddrOf(f, d)
	case exprPointer:
		p.declaratorPointer(f, d)
	case exprSelect:
		p.declaratorSelect(f, d, s)
	case exprPSelect:
		p.declaratorPSelect(f, d)
	default:
		panic(todo("", d.Position(), d.Name(), t, mode))
	}
}

func (p *project) declaratorPSelect(f *function, d *cc.Declarator) {
	if d.Type().Kind() == cc.Union {
		panic(todo("", pos(d)))
	}

	if local := f.locals[d]; local != nil {
		if local.isPinned {
			panic(todo("", d.Position()))
			return
		}

		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		p.w("(*%s)(unsafe.Pointer(%s))", p.typ(d.Type().Elem()), local.name)
		return
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		p.w("(*%s)(unsafe.Pointer(%s))", p.typ(d.Type().Elem()), tld.name)
	default:
		panic(todo(""))
	}
}

func (p *project) declaratorSelect(f *function, d *cc.Declarator, s cc.Type) {
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			if s == nil {
				s = d.Type()
			}
			p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(s), f.bpName, nonZeroUintptr(local.off), local.name)
			return
		}

		p.w("%s", local.name)
		return
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		p.w("%s", tld.name)
	default:
		panic(todo(""))
	}
}

func (p *project) declaratorAddrOf(f *function, d *cc.Declarator) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			if d.IsParameter && d.Type().Kind() == cc.Array {
				p.w("%s", local.name)
				return
			}

			panic(todo("", pos(d)))
			return
		}
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		p.w("uintptr(unsafe.Pointer(&%s))", tld.name)
	default:
		panic(todo(""))
	}
}

func (p *project) declaratorPointer(f *function, d *cc.Declarator) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			panic(todo("", pos(d)))
		}
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		p.w("unsafe.Pointer(&%s)", tld.name)
	default:
		panic(todo(""))
	}
}

func (p *project) declaratorValue(f *function, d *cc.Declarator, t cc.Type, flags flags) {
	if d.Type().Kind() == cc.Union {
		panic(todo("", pos(d)))
	}

	if d.Type().IsArithmeticType() {
		defer p.w("%s", p.convertType(d.Type(), t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				switch {
				case d.Type().Kind() == cc.Array && t.Kind() == cc.Ptr:
					if d.Type().Kind() == cc.Union {
						panic(todo("", pos(d)))
					}

					if flags&fOutermost == 0 {
						p.w("(")
						defer p.w(")")
					}
					p.w("%s%s/* %s */", f.bpName, nonZeroUintptr(local.off), local.name)
				default:
					if d.Type().Kind() == cc.Union {
						panic(todo("", pos(d)))
					}

					p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
				}
				return
			}

			p.w("%s", local.name)
			return
		}
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) convertType(from, to cc.Type, flags flags) string {
	force := flags&fForceConv != 0
	if from == nil {
		p.w("%s(", p.typ(to))
		return ")"
	}

	if from.IsScalarType() {
		switch {
		case force:
			p.w("%s(", p.helperType2(from, to))
			return ")"
		case from.Kind() == to.Kind():
			return ""
		default:
			p.w("%s(", p.typ(to))
			return ")"
		}
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

	panic(todo("", from, to))
}

func (p *project) convert(op cc.Operand, to cc.Type, flags flags) string {
	if flags&fForceRuntimeConv != 0 {
		flags |= fForceConv
	}
	if op == nil {
		p.w("%s(", p.typ(to))
		return ")"
	}

	force := flags&fForceConv != 0
	from := op.Type()
	if !force && from.IsScalarType() && from.Kind() == to.Kind() {
		return ""
	}

	if from.IsIntegerType() {
		return p.convertInt(op, to, flags)
	}

	switch from.Kind() {
	case cc.Function, cc.Struct, cc.Union, cc.Array, cc.Ptr:
		if !force && from.Kind() == to.Kind() {
			return ""
		}
	case cc.Double, cc.Float:
		p.w("%s(", p.typ(to))
		return ")"
	}

	panic(todo("%q -> %q", from, to))
}

func (p *project) convertInt(op cc.Operand, to cc.Type, flags flags) string {
	force := flags&fForceConv != 0
	if !force && flags&fIntLiteral != 0 {
		return ""
	}

	value := op.Value()
	if value == nil || !to.IsIntegerType() {
		if to.IsScalarType() {
			p.w("%s(", p.typ(to))
			return ")"
		}

		panic(todo("", op.Type(), to))
	}

	if flags&fForceRuntimeConv != 0 {
		p.w("%s(", p.helperType2(op.Type(), to))
		return ")"
	}

	switch from := op.Type(); {
	case from.IsSignedType():
		switch {
		case to.IsSignedType():
			switch x := value.(type) {
			case cc.Int64Value:
				switch to.Size() {
				case 1:
					if x >= math.MinInt8 && x <= math.MaxInt8 {
						switch {
						case !force && from.Size() == to.Size():
							return ""
						default:
							p.w("int8(")
							return ")"
						}
					}

					p.w("%sInt8FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 2:
					if x >= math.MinInt16 && x <= math.MaxInt16 {
						switch {
						case !force && from.Size() == to.Size():
							return ""
						default:
							p.w("int16(")
							return ")"
						}
					}

					p.w("%sInt16FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 4:
					if x >= math.MinInt32 && x <= math.MaxInt32 {
						switch {
						case !force && from.Size() == to.Size():
							return ""
						default:
							p.w("int32(")
							return ")"
						}
					}

					p.w("%sInt32FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 8:
					switch {
					case !force && from.Size() == to.Size():
						return ""
					default:
						p.w("int64(")
						return ")"
					}
				default:
					panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
				}
			default:
				panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
			}
		default: // to is unsigned
			switch x := value.(type) {
			case cc.Int64Value:
				switch to.Size() {
				case 1:
					if x >= 0 && x <= math.MaxUint8 {
						p.w("%s(", p.typ(to))
						return ")"
					}

					p.w("%sUint8FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 2:
					if x >= 0 && x <= math.MaxUint16 {
						p.w("%s(", p.typ(to))
						return ")"
					}

					p.w("%sUint16FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 4:
					if x >= 0 && x <= math.MaxUint32 {
						p.w("%s(", p.typ(to))
						return ")"
					}

					p.w("%sUint32FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 8:
					if x >= 0 {
						p.w("uint64(")
						return ")"
					}

					p.w("%sUint64FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				default:
					panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
				}
			default:
				panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
			}
		}
	default: // from is unsigned
		switch {
		case to.IsSignedType():
			switch x := value.(type) {
			case cc.Uint64Value:
				switch to.Size() {
				case 1:
					if x <= math.MaxInt8 {
						p.w("int8(")
						return ")"
					}

					p.w("%sInt8FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 2:
					if x <= math.MaxInt16 {
						p.w("int16(")
						return ")"
					}

					p.w("%sInt16FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 4:
					if x <= math.MaxInt32 {
						p.w("int32(")
						return ")"
					}

					p.w("%sInt32FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 8:
					if x <= math.MaxInt64 {
						p.w("int64(")
						return ")"
					}

					p.w("%sInt64FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				default:
					panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
				}
			default:
				panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
			}
		default: // to is unsigned
			switch x := value.(type) {
			case cc.Uint64Value:
				switch to.Size() {
				case 1:
					switch {
					case !force && from.Size() == 1:
						return ""
					default:
						p.w("uint8(")
						return ")"
					}
				case 2:
					switch {
					case !force && from.Size() == 2:
						return ""
					default:
						p.w("uint16(")
						return ")"
					}
				case 4:
					switch {
					case !force && from.Size() == 4:
						return ""
					default:
						p.w("uint32(")
						return ")"
					}
				case 8:
					switch {
					case !force && from.Size() == 8:
						return ""
					default:
						p.w("uint64(")
						return ")"
					}
				default:
					panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
				}
			default:
				panic(todo("%T(%v) %v -> %v", x, op.Value(), from, to))
			}
		}
	}
}

func nonZeroUintptr(n uintptr) string {
	if n == 0 {
		return ""
	}

	return fmt.Sprintf("%+d", n)
}

func (p *project) tld(f *function, n *cc.InitDeclarator, sep string) {
	d := n.Declarator
	tld := p.tlds[d]
	if tld == nil { // Dead declaration.
		return
	}

	t := d.Type()
	if d.IsTypedefName {
		if t.Kind() != cc.Void {
			p.w("%stype %s = %s;", sep, tld.name, p.typ(t))
		}
		return
	}

	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		p.w("%svar %s %s\t// %v:", sep, tld.name, p.typ(t), pos(n))
		switch t.Kind() {
		case cc.Struct, cc.Union:
			p.structs[t.Tag()].emit(p, nil)
		}
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		if d.IsStatic() && d.Read == 0 && d.Write == 1 && n.Initializer.IsConst() { // Initialized with no side effects and unused.
			break
		}

		p.w("%svar %s ", sep, tld.name)
		if !isAggregateType(d.Type()) {
			p.w("%s ", p.typ(d.Type()))
		}
		p.w("= ")
		p.initializer(f, n.Initializer, d.Type())
		p.w("; /* %v */", pos(d))
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
	comment := fmt.Sprintf("/* %v: */", pos(d))
	if need := f.off; need != 0 {
		f.bpName = f.scope.take("bp")
		p.w("{%s\n%s := %s.Alloc(%d)\n", comment, f.bpName, f.tlsName, need)
		p.w("defer %s.Free(%d)\n", f.tlsName, need)
		for _, v := range d.Type().Parameters() {
			if local := f.locals[v.Declarator()]; local != nil && local.isPinned { // Pin it.
				p.w("*(*%s)(unsafe.Pointer(%s%s)) = %s\n", p.typ(v.Declarator().Type()), f.bpName, nonZeroUintptr(local.off), local.name)
			}
		}
		comment = ""
	}
	p.compoundStatement(f, n.CompoundStatement, comment, false)
	p.flushStaticTLDs()
}

func (p *project) flushStaticTLDs() {
	for _, v := range p.staticQueue {
		p.tld(nil, v, "\n")
	}
	p.staticQueue = nil
}

func (p *project) compoundStatement(f *function, n *cc.CompoundStatement, comment string, forceNoBraces bool) {
	// '{' BlockItemList '}'
	brace := (!n.IsJumpTarget() || n.Parent() == nil) && !forceNoBraces
	if brace && (n.Parent() != nil || f.off == 0) {
		p.w("{%s", comment)
	}
	f.block = f.blocks[n]
	if f.block.topDecl {
		for _, v := range f.block.decls {
			p.declaration(f, v, true)
		}
	}
	var r *cc.JumpStatement
	for list := n.BlockItemList; list != nil; list = list.BlockItemList {
		r = p.blockItem(f, list.BlockItem)
	}
	if n.Parent() == nil && r == nil && f.rt.Kind() != cc.Void {
		p.w("\nreturn ")
		p.w("0%s", p.convertType(p.task.cfg.ABI.Type(cc.Int), f.rt, 0))
	}
	if brace {
		p.w("}")
	}
}

func (p *project) blockItem(f *function, n *cc.BlockItem) (r *cc.JumpStatement) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		p.declaration(f, n.Declaration, false)
	case cc.BlockItemStmt: // Statement
		r = p.statement(f, n.Statement, false)
		p.w(";")
	case cc.BlockItemLabel: // LabelDeclaration
		panic(todo("", pos(n)))
		p.w(";")
	case cc.BlockItemFuncDef: // DeclarationSpecifiers Declarator CompoundStatement
		panic(todo("", pos(n)))
		p.w(";")
	case cc.BlockItemPragma: // PragmaSTDC
		panic(todo("", pos(n)))
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
		p.compoundStatement(f, n.CompoundStatement, "", forceCompoundStmtBrace)
	case cc.StatementExpr: // ExpressionStatement
		p.expressionStatement(f, n.ExpressionStatement)
	case cc.StatementSelection: // SelectionStatement
		p.selectionStatement(f, n.SelectionStatement)
	case cc.StatementIteration: // IterationStatement
		p.iterationStatement(f, n.IterationStatement)
	case cc.StatementJump: // JumpStatement
		r = p.jumpStatement(f, n.JumpStatement)
	case cc.StatementAsm: // AsmStatement
		panic(todo("", pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if forceCompoundStmtBrace {
		p.w("}")
	}
	return r
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
		p.w("goto %s", f.labelNames[n.Token2.Value])
	case cc.JumpStatementGotoExpr: // "goto" '*' Expression ';'
		panic(todo("", pos(n)))
	case cc.JumpStatementContinue: // "continue" ';'
		p.w("continue")
	case cc.JumpStatementBreak: // "break" ';'
		p.w("break")
	case cc.JumpStatementReturn: // "return" Expression ';'
		r = n
		p.w("return ")
		if n.Expression != nil {
			p.expression(f, n.Expression, f.rt, nil, exprValue, fOutermost)
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) expression(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		switch mode {
		case exprVoid:
			p.expression(f, n.Expression, t, s, mode, flags)
			p.w(";")
			p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
		default:
			panic(todo("", pos(n), mode))
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpression(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoid:
		p.voidAssignmentExpression(f, n, t, mode, flags)
	case exprBool:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
		default:
			if flags&fOutermost == 0 {
				p.w("(")
				defer p.w(")")
			}
			defer p.w(" != 0")
			p.assignmentExpression(f, n, t, s, exprValue, flags|fOutermost)
		}
	default:
		switch n.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			if d := n.UnaryExpression.Declarator(); d != nil {
				if local := f.locals[d]; local != nil {
					if local.isPinned {
						panic(todo("", pos(n)))
					}

					defer p.w(")%s", p.convertType(d.Type(), n.Operand.Type(), flags))
					p.w("%sAssign%s(&%s, ", p.task.crt, p.helperType(d.Type()), local.name)
					p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), nil, exprValue, flags|fOutermost)
					break
				}

				panic(todo("", pos(n)))
			}

			defer p.w(")%s", p.convertType(n.UnaryExpression.Operand.Type(), n.Operand.Type(), flags))
			p.w("%sAssignPtr%s(", p.task.crt, p.helperType(n.UnaryExpression.Operand.Type()))
			p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), s, exprAddrOf, flags)
			p.w(", ")
			p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), nil, exprValue, flags|fOutermost)
		case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
			panic(todo("", pos(n)))
		case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
			panic(todo("", pos(n)))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (p *project) helperType(t cc.Type) string {
	if t.IsAliasType() {
		t = t.Alias()
	}
	s := p.typ(t)
	return strings.ToUpper(s[:1]) + s[1:]
}

func (p *project) helperType2(from, to cc.Type) string {
	if from.Kind() == to.Kind() {
		return fmt.Sprintf("%s%s", p.task.crt, p.helperType(from))
	}

	return fmt.Sprintf("%s%sFrom%s", p.task.crt, p.helperType(to), p.helperType(from))
}

func (p *project) conditionalExpression(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		switch mode {
		case exprVoid:
			p.w("if ")
			p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags|fOutermost)
			p.w(" {")
			p.expression(f, n.Expression, t, nil, mode, flags|fOutermost)
			p.w("} else {")
			p.conditionalExpression(f, n.ConditionalExpression, t, nil, mode, flags|fOutermost)
			p.w("}")
		default:
			p.w(" func() %s { if ", p.typ(t))
			p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags|fOutermost)
			p.w(" { return ")
			p.expression(f, n.Expression, t, nil, exprValue, flags|fOutermost)
			p.w("}; return ")
			p.conditionalExpression(f, n.ConditionalExpression, t, nil, exprValue, flags|fOutermost)
			p.w("}()")
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpression(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		p.binaryLogicalOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryLogicalOrExpression(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags&^fOutermost)
	p.w(" ||%s", comment(" ", &n.Token))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), nil, exprBool, flags&^fOutermost)
}

func (p *project) booleanBinaryExpression(from cc.Operand, to cc.Type, mode *exprMode, flags flags) (r string) {
	if flags&fOutermost == 0 {
		p.w("(")
		r = ")"
	}
	switch *mode {
	case exprBool:
		*mode = exprValue
	default:
		r = p.convert(from, to, flags) + r
		p.w("%sBool32(", p.task.crt)
		r = ")" + r
	}
	return r
}

func (p *project) logicalAndExpression(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		p.binaryLogicalAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryLogicalAndExpression(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), nil, exprBool, flags&^fOutermost)
	p.w(" &&%s", comment(" ", &n.Token))
	p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.InclusiveOrExpression.Operand.Type(), nil, exprBool, flags&^fOutermost)
}

func (p *project) inclusiveOrExpression(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		p.binaryInclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryInclusiveOrExpression(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case orOverflows(n.InclusiveOrExpression.Operand, n.ExclusiveOrExpression.Operand, n.Promote()):
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" |%s", comment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags|fOutermost|fForceRuntimeConv)
	default:
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" |%s", comment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
	}
}

func orOverflows(lo, ro cc.Operand, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	return overflows(a.Or(a, b), promote)
}

func (p *project) artithmeticBinaryExpression(from cc.Operand, to cc.Type, mode *exprMode, flags flags) (r string) {
	if flags&fOutermost == 0 {
		p.w("(")
		r = ")"
	}
	switch *mode {
	case exprBool:
		p.w("(")
		r = ") != 0" + r
		*mode = exprValue
	default:
		r = p.convert(from, to, flags) + r
	}
	return r
}

func (p *project) exclusiveOrExpression(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		p.binaryExclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryExclusiveOrExpression(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case xorOverflows(n.ExclusiveOrExpression.Operand, n.AndExpression.Operand, n.Promote()):
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" ^%s", comment(" ", &n.Token))
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags|fOutermost|fForceRuntimeConv)
	default:
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" ^%s", comment(" ", &n.Token))
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
	}
}

func xorOverflows(lo, ro cc.Operand, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	return overflows(a.Xor(a, b), promote)
}

func (p *project) andExpression(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		p.binaryAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryAndExpression(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case andOverflows(n.AndExpression.Operand, n.EqualityExpression.Operand, n.Promote()):
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" &%s", comment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags|fOutermost|fForceRuntimeConv)
	default:
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" &%s", comment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
	}
}

func andOverflows(lo, ro cc.Operand, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	return overflows(a.And(a, b), promote)
}

func (p *project) equalityExpression(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, s, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		p.binaryEqualityExpression(f, n, " == ", t, mode, flags)
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		p.binaryEqualityExpression(f, n, " != ", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryEqualityExpression(f *function, n *cc.EqualityExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
	p.w(" %s%s", oper, comment(" ", &n.Token))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
}

func (p *project) relationalExpression(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, s, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		p.binaryRelationalExpression(f, n, " < ", t, mode, flags)
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		p.binaryRelationalExpression(f, n, " > ", t, mode, flags)
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		p.binaryRelationalExpression(f, n, " <= ", t, mode, flags)
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		p.binaryRelationalExpression(f, n, " >= ", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryRelationalExpression(f *function, n *cc.RelationalExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
	p.w(" %s%s", oper, comment(" ", &n.Token))
	p.shiftExpression(f, n.ShiftExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
}

func (p *project) shiftExpression(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, s, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		p.binaryShiftExpression(f, n, "<<", t, mode, flags)
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		p.binaryShiftExpression(f, n, ">>", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryShiftExpression(f *function, n *cc.ShiftExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case shiftOverflows(n.ShiftExpression.Operand, n.AdditiveExpression.Operand, oper, n.Operand.Type()):
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags|fOutermost|fForceRuntimeConv)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags|fOutermost|fForceRuntimeConv)
	case isConstInteger(n.ShiftExpression.Operand) && n.ShiftExpression.Operand.Type().Size() > 4:
		s := p.convert(nil, n.ShiftExpression.Operand.Type(), 0)
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags|fOutermost)
		p.w("%s %s%s", s, oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
	default:
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags&^fOutermost)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
	}
}

func shiftOverflows(lo, ro cc.Operand, oper string, result cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	if !b.IsUint64() {
		return true
	}

	bits := b.Uint64()
	if bits > mathutil.MaxUint {
		return true
	}

	switch oper {
	case "<<":
		return overflows(a.Lsh(a, uint(bits)), result)
	case ">>":
		return overflows(a.Rsh(a, uint(bits)), result)
	default:
		panic(todo(""))
	}
}

func (p *project) additiveExpression(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, s, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "+", t, mode, flags)
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "-", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryAdditiveExpression(f *function, n *cc.AdditiveExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	lo := n.AdditiveExpression.Operand
	ro := n.MultiplicativeExpression.Operand
	lt := lo.Type()
	rt := ro.Type()
	switch mode {
	case exprValue, exprLValue, exprAddrOf:
		switch {
		case lt.IsArithmeticType() && rt.IsArithmeticType(): // x +- y
			switch {
			case intAddOverflows(lo, ro, oper, n.Promote()): // i +- j
				p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
				p.w(" %s%s", oper, comment(" ", &n.Token))
				p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags|fOutermost|fForceRuntimeConv)
			default:
				p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
				p.w(" %s%s", oper, comment(" ", &n.Token))
				p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
			}
		case lt.Kind() == cc.Ptr && rt.IsIntegerType(): // p +- i
			p.additiveExpression(f, n.AdditiveExpression, lt, nil, exprValue, flags&^fOutermost)
			p.w(" %s%s uintptr(", oper, comment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, nil, exprValue, flags&^fOutermost)
			p.w(")")
			if sz := lt.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
		case lt.Kind() == cc.Array && rt.IsIntegerType(): // p +- i
			p.additiveExpression(f, n.AdditiveExpression, lt, nil, exprAddrOf, flags&^fOutermost)
			p.w(" %s%s uintptr(", oper, comment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, nil, exprValue, flags&^fOutermost)
			p.w(")")
			if sz := lt.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
		case lt.IsIntegerType() && rt.Kind() == cc.Ptr: // i +- p
			panic(todo(""))
		case lt.IsIntegerType() && rt.Kind() == cc.Array: // i +- p
			panic(todo(""))
		case lt.Kind() == cc.Ptr && rt.Kind() == cc.Ptr && oper == "-": // p - q
			p.w("(")
			p.additiveExpression(f, n.AdditiveExpression, lt, nil, exprValue, flags&^fOutermost)
			p.w(" %s%s", oper, comment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, nil, exprValue, flags&^fOutermost)
			p.w(")/%d", lt.Elem().Size())
		default:
			panic(todo("", n.Position(), lt, rt, oper))
		}
	default:
		panic(todo("", mode))
	}
}

func intAddOverflows(lo, ro cc.Operand, oper string, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	switch oper {
	case "+":
		return overflows(a.Add(a, b), promote)
	case "-":
		return overflows(a.Sub(a, b), promote)
	default:
		panic(todo(""))
	}
}

func getIntOperands(a, b cc.Operand) (x, y *big.Int, ok bool) {
	switch n := a.Value().(type) {
	case cc.Int64Value:
		x = big.NewInt(int64(n))
	case cc.Uint64Value:
		x = big.NewInt(0).SetUint64(uint64(n))
	default:
		return nil, nil, false
	}

	switch n := b.Value().(type) {
	case cc.Int64Value:
		return x, big.NewInt(int64(n)), true
	case cc.Uint64Value:
		return x, big.NewInt(0).SetUint64(uint64(n)), true
	default:
		return nil, nil, false
	}
}

func overflows(n *big.Int, promote cc.Type) bool {
	switch {
	case isSigned(promote):
		switch promote.Size() {
		case 4:
			return n.Cmp(minInt32) < 0 || n.Cmp(maxInt32) > 0
		case 8:
			return n.Cmp(minInt64) < 0 || n.Cmp(maxInt64) > 0
		}
	default:
		switch promote.Size() {
		case 4:
			return n.Sign() < 0 || n.Cmp(maxUint32) > 0
		case 8:
			return n.Sign() < 0 || n.Cmp(maxUint64) > 0
		}
	}
	panic(todo("", promote.Size(), promote))
}

func (p *project) multiplicativeExpression(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		p.binaryMultiplicativeExpression(f, n, "*", t, mode, flags)
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		p.binaryMultiplicativeExpression(f, n, "/", t, mode, flags)
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		p.binaryMultiplicativeExpression(f, n, "%", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryMultiplicativeExpression(f *function, n *cc.MultiplicativeExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case intMulOverflows(n.MultiplicativeExpression.Operand, n.CastExpression.Operand, oper, n.Promote()):
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.castExpression(f, n.CastExpression, n.Promote(), nil, exprValue, flags|fOutermost|fForceRuntimeConv)
	default:
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.castExpression(f, n.CastExpression, n.Promote(), nil, exprValue, flags&^fOutermost)
	}
}

func intMulOverflows(lo, ro cc.Operand, oper string, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	switch oper {
	case "*":
		return overflows(a.Mul(a, b), promote)
	case "/":
		if b.Sign() == 0 {
			return true
		}

		return overflows(a.Div(a, b), promote)
	case "%":
		if b.Sign() == 0 {
			return true
		}

		return overflows(a.Mod(a, b), promote)
	default:
		panic(todo(""))
	}
}

func (p *project) castExpression(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		op := n.CastExpression.Operand
		tn := n.TypeName.Type()
		switch {
		case op.Type().Kind() == cc.Ptr && tn.IsArithmeticType():
			defer p.w("%s", p.convertType(nil, t, flags|fForceConv))
			p.castExpression(f, n.CastExpression, op.Type(), s, mode, flags)
		case tn.IsArithmeticType():
			switch {
			case isNegativeInt(op) && isUnsigned(t):
				defer p.w("%s", p.convertType(tn, t, flags|fForceConv))
				p.castExpression(f, n.CastExpression, tn, s, exprValue, flags|fOutermost)
			default:
				defer p.w("%s", p.convertType(tn, t, flags))
				p.castExpression(f, n.CastExpression, tn, s, exprValue, flags)
			}
		default:
			switch tn.Kind() {
			case cc.Ptr:
				p.castExpression(f, n.CastExpression, tn, s, mode, flags)
			case cc.Void:
				p.castExpression(f, n.CastExpression, tn, s, exprVoid, flags)
			default:
				panic(todo("", n.Position(), t, t.Kind()))
			}
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpression(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		p.castExpression(f, n.CastExpression, nil, nil, exprAddrOf, flags&^fOutermost)
	case cc.UnaryExpressionDeref: // '*' CastExpression
		p.unaryExpressionDeref(f, n, t, s, mode, flags)
	case cc.UnaryExpressionPlus: // '+' CastExpression
		p.w(" +")
		p.castExpression(f, n.CastExpression, t, s, exprValue, flags)
	case cc.UnaryExpressionMinus: // '-' CastExpression
		switch {
		case isNonNegativeInt(n.CastExpression.Operand) && isUnsigned(n.Operand.Type()):
			p.w(" -")
			defer p.w("%s", p.convert(n.CastExpression.Operand, t, flags|fForceRuntimeConv))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags)
		default:
			defer p.w("%s", p.convert(n.Operand, t, flags))
			p.w(" -")
			p.castExpression(f, n.CastExpression, n.Operand.Type(), s, exprValue, flags)
		}
	case cc.UnaryExpressionCpl: // '~' CastExpression
		switch {
		case isNonNegativeInt(n.CastExpression.Operand) && isUnsigned(n.CastExpression.Operand.Type()):
			p.w("^")
			defer p.w("%s", p.convert(n.CastExpression.Operand, t, flags|fForceConv))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags|fOutermost)
		default:
			defer p.w("%s", p.convert(n.Operand, t, flags))
			p.w(" ^")
			p.castExpression(f, n.CastExpression, n.Operand.Type(), s, exprValue, flags)
		}
	case cc.UnaryExpressionNot: // '!' CastExpression
		switch mode {
		case exprValue:
			p.w("%sBool32(!(", p.task.crt)
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprBool, flags|fOutermost)
			p.w("))")
		case exprBool:
			p.w("!(")
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprBool, flags|fOutermost)
			p.w(")")
		default:
			panic(todo("", pos(n), mode))
		}
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		defer p.w("%s", p.convert(nil, t, flags))
		t := n.UnaryExpression.Operand.Type()
		if isArray(t) {
			p.w("%d", t.Len()*t.Elem().Size())
			break
		}

		s := "(0)"
		if !t.IsArithmeticType() {
			switch t.Kind() {
			case cc.Ptr:
				// ok
			case cc.Struct, cc.Union:
				s = "{}"
			default:
				panic(todo("", t.Kind()))
			}
		}
		p.w("unsafe.Sizeof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		defer p.w("%s", p.convert(nil, t, flags))
		t := n.TypeName.Type()
		if isArray(t) {
			p.w("%d", t.Len()*t.Elem().Size())
			break
		}

		s := "(0)"
		if !t.IsArithmeticType() {
			switch t.Kind() {
			case cc.Ptr:
				// ok
			case cc.Struct, cc.Union:
				s = "{}"
			default:
				panic(todo("", t.Kind()))
			}
		}
		p.w("unsafe.Sizeof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", pos(n)))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		if n.TypeName.Type().Kind() == cc.Void {
			p.intConst("", n.Operand, t, flags)
			break
		}

		defer p.w("%s", p.convert(nil, t, flags))
		t := n.UnaryExpression.Operand.Type()
		if isArray(t) {
			p.w("%d", t.Len()*t.Elem().Size())
			break
		}

		s := "(0)"
		if !t.IsArithmeticType() {
			switch t.Kind() {
			case cc.Ptr:
				// ok
			case cc.Struct, cc.Union:
				s = "{}"
			default:
				panic(todo("", t.Kind()))
			}
		}
		p.w("unsafe.Alignof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		if n.TypeName.Type().Kind() == cc.Void {
			p.intConst("", n.Operand, t, flags)
			break
		}

		defer p.w("%s", p.convert(nil, t, flags))
		t := n.TypeName.Type()
		if isArray(t) {
			p.w("%d", t.Len()*t.Elem().Size())
			break
		}

		s := "(0)"
		if !t.IsArithmeticType() {
			switch t.Kind() {
			case cc.Ptr:
				// ok
			case cc.Struct, cc.Union:
				s = "{}"
			default:
				panic(todo("", t.Kind()))
			}
		}
		p.w("unsafe.Alignof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", pos(n)))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func isSigned(t cc.Type) bool   { return t.IsIntegerType() && t.IsSignedType() }
func isUnsigned(t cc.Type) bool { return t.IsIntegerType() && !t.IsSignedType() }

func isConstInteger(op cc.Operand) bool {
	switch op.Value().(type) {
	case cc.Int64Value, cc.Uint64Value:
		return true
	default:
		return false
	}
}

func isNegativeInt(op cc.Operand) bool {
	switch x := op.Value().(type) {
	case cc.Int64Value:
		return x < 0
	default:
		return false
	}
}

func isNonNegativeInt(op cc.Operand) bool {
	switch x := op.Value().(type) {
	case cc.Int64Value:
		return x >= 0
	case cc.Uint64Value:
		return true
	default:
		return false
	}
}

func (p *project) unaryExpressionPreIncDec(f *function, n *cc.UnaryExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	ut := n.UnaryExpression.Operand.Type()
	switch mode {
	case exprVoid:
		p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), s, exprLValue, flags)
		if ut.IsIntegerType() || ut.Kind() == cc.Ptr && p.incDelta(n, ut) == 1 {
			p.w("%s", oper)
			return
		}

		if ut.Kind() == cc.Ptr {
			p.w("%s %d", oper2, p.incDelta(n, ut))
			return
		}

		panic(todo("", pos(n), ut))
	case exprValue:
		defer p.w("%s", p.convert(n.UnaryExpression.Operand, t, flags))
		x := "Dec"
		if oper == "++" {
			x = "Inc"
		}
		p.w("%sPre%s%s(&", p.task.crt, x, p.helperType(ut))
		p.unaryExpression(f, n.UnaryExpression, ut, s, exprLValue, flags)
		p.w(", %d)", p.incDelta(n.PostfixExpression, ut))
	default:
		panic(todo("", pos(n), mode))
	}
}

func (p *project) unaryExpressionDeref(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprBool:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.unaryExpressionDeref(f, n, t, s, exprValue, flags)
		p.w(" != 0")
	case exprValue, exprLValue:
		switch {
		case isArray(n.CastExpression.Operand.Type()):
			defer p.w(")%s", p.convertType(n.CastExpression.Operand.Type().Elem(), t, flags))
			p.w("*(*%s)(", p.typ(n.Operand.Type()))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprPointer, flags|fOutermost)
		default:
			defer p.w("))%s", p.convertType(n.CastExpression.Operand.Type().Elem(), t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, mode, flags|fOutermost)
		}
	case exprAddrOf:
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, mode, flags|fOutermost)
	default:
		panic(todo("", mode))
	}
}

func (p *project) postfixExpression(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionIndex(f, n, t, s, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, t, s, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionPSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postPostfixExpressionIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postPostfixExpressionIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postPostfixExpressionIncDec(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	peType := n.PostfixExpression.Operand.Type()
	switch mode {
	case exprVoid:
		p.postfixExpression(f, n.PostfixExpression, peType, s, exprLValue, flags)
		if peType.IsIntegerType() || peType.Kind() == cc.Ptr && p.incDelta(n, peType) == 1 {
			p.w("%s", oper)
			return
		}

		if peType.Kind() == cc.Ptr {
			p.w("%s %d", oper2, p.incDelta(n, peType))
			return
		}

		panic(todo("", pos(n)))
	case exprBool:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.postPostfixExpressionIncDec(f, n, oper, oper2, t, s, exprValue, flags)
		p.w(" != 0")
	case exprValue, exprLValue:
		defer p.w("%s", p.convert(n.PostfixExpression.Operand, t, flags))
		x := "Dec"
		if oper == "++" {
			x = "Inc"
		}
		p.w("%sPost%s%s(&", p.task.crt, x, p.helperType(peType))
		p.postfixExpression(f, n.PostfixExpression, peType, s, exprLValue, flags)
		p.w(", %d)", p.incDelta(n.PostfixExpression, peType))
	default:
		panic(todo("", pos(n), mode))
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

func (p *project) postfixExpressionPSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	peType := n.PostfixExpression.Operand.Type()
	switch mode {
	case exprValue, exprLValue, exprSelect, exprPSelect:
		if peType.Kind() == cc.Union {
			panic(todo("", pos(n)))
		}

		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, peType, s, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
	case exprAddrOf:
		p.postfixExpression(f, n.PostfixExpression, peType, s, exprValue, flags)
		p.fldOff(peType.Elem(), n.Token2)
	case exprBool:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpressionPSelect(f, n, t, s, exprValue, flags)
	default:
		panic(todo("", pos(n), mode))
	}
}

func (p *project) fldOff(t cc.Type, tok cc.Token) {
	var off uintptr
	fld, ok := t.FieldByName(tok.Value)
	if fld.Type().IsBitFieldType() {
		//TODO panic(todo("bit fields not supported"))
	}

	if !ok {
		p.err(&tok, "uknown field: %s", tok.Value)
	} else {
		off = fld.Offset()
	}
	if off != 0 {
		p.w("+%d", off)
	}
	p.w("/* &.%s */", tok.Value)
}

func (p *project) postfixExpressionSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	switch mode {
	case exprAddrOf:
		p.postfixExpression(f, n.PostfixExpression, nil, s, mode, flags)
		p.fldOff(pe, n.Token2)
	case exprValue, exprLValue, exprSelect, exprPSelect:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		switch {
		case pe.Kind() == cc.Union:
			fld, ok := pe.FieldByName(n.Token2.Value)
			if fld.Type().IsBitFieldType() {
				//TODO panic(todo("bit fields not supported"))
			}

			if !ok {
				p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
				return
			}

			p.postfixExpression(f, n.PostfixExpression, pe, fld.Type(), exprSelect, flags&^fOutermost)
		default:
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprSelect, flags&^fOutermost)
			p.w(".%s", p.fieldName(n.Token2.Value))
		}
	case exprBool:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpressionSelect(f, n, t, s, exprValue, flags)
	default:
		panic(todo("", pos(n), mode))
	}
}

func (p *project) postfixExpressionCall(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoid:
		// ok
	case exprBool:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
	case exprValue:
		defer p.w("%s", p.convert(n.Operand, t, flags))
	default:
		panic(todo("", n.Position(), mode))
	}
	peType := n.PostfixExpression.Operand.Type()
	p.postfixExpression(f, n.PostfixExpression, peType, s, exprValue, flags&^fOutermost)
	p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
}

func (p *project) argumentExpressionList(f *function, pe *cc.PostfixExpression, n *cc.ArgumentExpressionList, bpOff uintptr) {
	p.w("(%s", f.tlsName)
	ft := funcType(pe.Operand.Type())
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
		mode := exprValue
		if at := arg.Operand.Type(); isArray(at) {
			mode = exprAddrOf
		}
		switch {
		case i < len(params):
			p.assignmentExpression(f, arg, arg.Promote(), nil, mode, fOutermost)
		case va && i == len(params):
			p.w("%sVaList(%s%s, ", p.task.crt, f.bpName, nonZeroUintptr(bpOff))
			paren = ")"
			fallthrough
		default:
			p.assignmentExpression(f, arg, arg.Promote(), nil, mode, fOutermost)
		}
	}
	if isVariadic && len(args) == len(params) {
		p.w(", 0")
	}
	p.w("%s)", paren)
}

func (p *project) postfixExpressionIndex(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	peType := n.PostfixExpression.Operand.Type()
	switch mode {
	case exprSelect, exprLValue, exprValue:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		d := n.PostfixExpression.Declarator()
		switch {
		case isArray(peType) && (d == nil || !d.IsParameter):
			p.postfixExpression(f, n.PostfixExpression, peType, s, exprValue, flags&^fOutermost)
			p.w("[")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost)
			p.w("]")
		default:
			p.w("*(*%s)(unsafe.Pointer(", p.typ(peType.Elem()))
			p.postfixExpression(f, n.PostfixExpression, peType, s, exprValue, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := peType.Decay().Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		}
	case exprAddrOf:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		switch {
		case peType.Kind() == cc.Ptr:
			p.postfixExpression(f, n.PostfixExpression, peType, s, exprValue, flags&^fOutermost)
		default:
			p.postfixExpression(f, n.PostfixExpression, peType, s, mode, flags&^fOutermost)
		}
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := peType.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	case exprBool:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.postfixExpression(f, n, t, s, exprValue, flags)
		p.w(" != 0")
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) uintptr(f func(), op cc.Operand) {
	if op.Type().IsIntegerType() {
		switch {
		case isNegativeInt(op):
			p.w(" %sUintptrFrom%s(", p.task.crt, p.helperType(op.Type()))
		default:
			p.w(" uintptr(")
		}
		f()
		p.w(")")
		return
	}

	panic(todo(""))
}

func (p *project) primaryExpression(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprBool:
		if flags&fOutermost == 0 && n.Case != cc.PrimaryExpressionExpr {
			p.w("(")
			defer p.w(")")
		}

		if n.Case != cc.PrimaryExpressionExpr {
			defer p.w(" != 0")
			mode = exprValue
		}
	}
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			if mode == exprValue && isArray(d.Type()) && t.Kind() == cc.Ptr {
				mode = exprAddrOf
			}
			p.declarator(f, d, t, s, mode, flags)
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		p.floatConst(n.Token.Src.String(), n.Operand, t)
	case cc.PrimaryExpressionEnum: // ENUMCONST
		if f != nil {
			for block := f.block; block != nil; block = block.parent {
				if s := block.enumConsts[n.Token.Value]; s != "" {
					p.w("%s", s)
					return
				}
			}
		}

		if s := p.enumConsts[n.Token.Value]; s != "" {
			p.w("%s", s)
			return
		}

		p.intConst("", n.Operand, t, flags)
	case cc.PrimaryExpressionChar: // CHARCONST
		p.charConst(n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("%v: %T", n.Position(), n.Operand.Value()))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.w("(")
		defer p.w(")")
		p.expression(f, n.Expression, t, s, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "expression statements not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) stringLiteral(v cc.Value) string {
	switch x := v.(type) {
	case cc.StringValue:
		id := cc.StringID(x)
		off, ok := p.tsOffs[id]
		if !ok {
			s := id.String()
			off = uintptr(p.ts.Len())
			p.ts.WriteString(s)
			p.ts.WriteByte(0)
			p.tsOffs[id] = off
		}
		return fmt.Sprintf("%s%s", p.tsNameP, nonZeroUintptr(off))
	default:
		panic(todo("%T", x))
	}
}

func (p *project) wideStringLiteral(v cc.Value) string {
	switch x := v.(type) {
	case cc.WideStringValue:
		id := cc.StringID(x)
		off, ok := p.ts4Offs[id]
		if !ok {
			off = 4 * uintptr(len(p.ts4))
			s := []rune(id.String())
			p.ts4 = append(p.ts4, s...)
			p.ts4 = append(p.ts4, 0)
			p.tsOffs[id] = off
		}
		return fmt.Sprintf("%s%s", p.ts4NameP, nonZeroUintptr(off))
	default:
		panic(todo("%T", x))
	}
}

func (p *project) charConst(src string, op cc.Operand, to cc.Type, flags flags) {
	switch {
	case to.IsArithmeticType():
		defer p.w("%s", p.convert(op, to, flags))
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
			defer p.w("%s", p.convert(op, to, 0))
			if snValid && sn == float64(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			panic(todo("", to))
		}
	case cc.Float32Value:
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
			panic(todo("", to))
		}
	default:
		panic(todo("%T(%v)", x, x))
	}
}

func (p *project) intConst(src string, op cc.Operand, to cc.Type, flags flags) {
	ptr := to.Kind() == cc.Ptr
	switch {
	case to.IsArithmeticType():
		defer p.w("%s", p.convert(op, to, flags|fIntLiteral))
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
	case cc.Uint64Value:
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

func (p *project) voidAssignmentExpression(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, n.ConditionalExpression.Operand.Type(), nil, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), nil, exprLValue, flags)
		p.w(" = ")
		p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), nil, exprValue, flags|fOutermost)
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "*", "Mul", flags)
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "/", "Div", flags)
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "%", "Mod", flags)
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "+", "Add", flags)
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "-", "Sub", flags)
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, ">>", "Shl", flags)
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, ">>", "Shr", flags)
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "&", "And", flags)
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "^", "Xor", flags)
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		p.assignOp(f, n, t, nil, mode, "|", "Or", flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignOp(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	switch {
	case mode == exprVoid:
		if d := n.UnaryExpression.Declarator(); d != nil {
			p.declarator(f, d, d.Type(), s, exprLValue, flags|fOutermost)
			p.w(" = ")
			x := p.convertType(n.Promote(), d.Type(), flags)
			p.declarator(f, d, n.Promote(), s, exprValue, flags|fOutermost)
			p.w(" %s ", oper)
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
			p.w("%s", x)
			return
		}

		lhs := n.UnaryExpression
		p.w("*(*%s)(unsafe.Pointer(", p.typ(lhs.Operand.Type()))
		p.unaryExpression(f, lhs, lhs.Operand.Type(), s, exprAddrOf, flags|fOutermost)
		p.w(")) %s= ", oper)
		p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), s, exprValue, flags|fOutermost)
	default:
		panic(todo(""))
	}
}

func (p *project) iterationStatement(f *function, n *cc.IterationStatement) {
	sv := f.switchCtx
	f.switchCtx = 0
	defer func() { f.switchCtx = sv }()
	switch {
	case f.block.isFlat:
		//TODO panic(todo("", pos(n)))
		fallthrough
	default:
		p.w("%s", comment("\n", n))
		switch n.Case {
		case cc.IterationStatementWhile: // "while" '(' Expression ')' Statement
			p.w("for ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.statement(f, n.Statement, true)
		case cc.IterationStatementDo: // "do" Statement "while" '(' Expression ')' ';'
			v := f.scope.take("ok")
			p.w("for %v := true; %[1]v; %[1]v = ", v)
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.statement(f, n.Statement, true)
		case cc.IterationStatementFor: // "for" '(' Expression ';' Expression ';' Expression ')' Statement
			p.w("for ")
			if n.Expression != nil {
				p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprVoid, fOutermost)
			}
			p.w("; ")
			if n.Expression2 != nil {
				p.expression(f, n.Expression2, n.Expression2.Operand.Type(), nil, exprBool, fOutermost)
			}
			p.w("; ")
			if n.Expression3 != nil {
				p.expression(f, n.Expression3, n.Expression3.Operand.Type(), nil, exprVoid, fOutermost)
			}
			p.statement(f, n.Statement, true)
		case cc.IterationStatementForDecl: // "for" '(' Declaration Expression ';' Expression ')' Statement
			panic(todo("", pos(n)))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (p *project) selectionStatement(f *function, n *cc.SelectionStatement) {
	switch {
	case f.block.isFlat:
		//TODO panic(todo("", pos(n)))
		fallthrough
	default:
		p.w("%s", comment("\n", n))
		switch n.Case {
		case cc.SelectionStatementIf: // "if" '(' Expression ')' Statement
			p.w("if ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.statement(f, n.Statement, true)
		case cc.SelectionStatementIfElse: // "if" '(' Expression ')' Statement "else" Statement
			p.w("if ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.statement(f, n.Statement, true)
			p.w(" else ")
			p.statement(f, n.Statement2, true)
		case cc.SelectionStatementSwitch: // "switch" '(' Expression ')' Statement
			sv := f.switchCtx
			f.switchCtx = inSwitchFirst
			p.w("switch ")
			p.expression(f, n.Expression, n.Promote(), nil, exprValue, fOutermost)
			p.statement(f, n.Statement, true)
			f.switchCtx = sv
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	}
}

func (p *project) expressionStatement(f *function, n *cc.ExpressionStatement) {
	p.w("%s", comment("\n", n))
	// Expression AttributeSpecifierList ';'
	if n.Expression == nil || n.Expression.IsSideEffectsFree {
		return
	}

	p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprVoid, fOutermost)
}

func (p *project) labeledStatement(f *function, n *cc.LabeledStatement) {
	if f.block.isFlat {
		//TODO p.labeledStatementFlat(f, n)
		//TODO return
	}

	switch n.Case {
	case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
		p.w("%s%s:", comment("\n", n), f.labelNames[n.Token.Value])
		p.statement(f, n.Statement, false)
	case
		cc.LabeledStatementCaseLabel, // "case" ConstantExpression ':' Statement
		cc.LabeledStatementDefault:   // "default" ':' Statement

		p.labeledStatementCase(f, n)
	case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
		panic(todo("", n.Position(), n.Case))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) labeledStatementFlat(f *function, n *cc.LabeledStatement) {
	switch n.Case {
	case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
		p.w("%s%s:", comment("\n", n), f.labelNames[n.Token.Value])
		p.statement(f, n.Statement, false)
	case
		cc.LabeledStatementCaseLabel, // "case" ConstantExpression ':' Statement
		cc.LabeledStatementDefault:   // "default" ':' Statement

		panic(todo("", pos(n), n.Case))
	case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
		panic(todo("", n.Position(), n.Case))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) labeledStatementCase(f *function, n *cc.LabeledStatement) {
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
	switch n.Case {
	case cc.LabeledStatementCaseLabel: // "case" ConstantExpression ':' Statement
		p.w("%scase ", comment("\n", n))
		p.constantExpression(f, n.ConstantExpression, n.ConstantExpression.Operand.Type(), exprValue, fOutermost)
		p.w(":")
	case cc.LabeledStatementDefault: // "default" ':' Statement
		p.w("%sdefault:", comment("\n", n))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	p.statement(f, n.Statement, false)
}

func (p *project) constantExpression(f *function, n *cc.ConstantExpression, t cc.Type, mode exprMode, flags flags) {
	// ConditionalExpression
	p.conditionalExpression(f, n.ConditionalExpression, t, nil, mode, flags)
}

func (p *project) functionDefinitionSignature(f *function, tld *tld) {
	switch {
	case f.mainSignatureForced:
		p.w("%sfunc %s(%s *%sTLS, _ int32, _ uintptr) int32", comment("\n", f.fndef), tld.name, f.tlsName, p.task.crt)
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
	p.w("(%s *%sTLS", f.tlsName, p.task.crt)
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
		p.w(", %s %s", nm, p.paramTyp(v.Type()))
	}
	p.w(")")
	if rt := t.Result(); rt != nil && rt.Kind() != cc.Void {
		p.w(" %s", p.typ(rt))
	}
}

func (p *project) paramTyp(t cc.Type) string {
	if t.Kind() == cc.Array {
		return "uintptr"
	}

	return p.typ(t)
}
