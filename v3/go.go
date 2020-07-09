// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"io/ioutil"
	//TODO- "runtime/debug"
	"math"
	"math/big"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"modernc.org/cc/v3"
	"modernc.org/mathutil"
)

var (
	idAligned = cc.String("aligned") // int __attribute__ ((aligned (8))) foo;
	idMain    = cc.String("main")
	idVaArg   = cc.String("__ccgo_va_arg")
	idVaEnd   = cc.String("__ccgo_va_end")
	idVaList  = cc.String("va_list")
	idVaStart = cc.String("__ccgo_va_start")

	oTraceG bool
	oTraceW bool
)

type exprMode int

const (
	_              exprMode = iota
	exprAddrOf              // &foo as uinptr (must be static)
	exprBool                // foo in foo != 0
	exprCondInit            // foo or bar in int i = x ? foo : bar;
	exprCondReturn          // foo or bar in return x ? foo : bar;
	exprFunc                // foo in foo(bar)
	exprLValue              // foo in foo = bar
	exprPSelect             // foo in foo->bar
	exprSelect              // foo in foo.bar
	exprValue               // foo in bar = foo
	exprVoid                //

	tooManyErrors = "too many errors"
)

type opKind int

const (
	opNormal opKind = iota
	opArray
	opArrayParameter
	opFunction
	opUnion
	opBitfield
	opStruct
)

type flags byte

const (
	fOutermost flags = 1 << iota
	fForceConv
	fForceRuntimeConv
	fNoCondAssignment
	fAddrOfFuncPtrOk
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

	conflicts bool
	emitted   bool
}

func (s *taggedStruct) emit(p *project, ds *cc.DeclarationSpecifiers) {
	if s == nil || s.emitted {
		return
	}

	s.emitted = true
	p.w("%stype %s = %s;\n\n", tidyComment("\n", ds), s.name, s.gotyp)
}

// Return first non empty token separator within n or dflt otherwise.
func comment(dflt string, n cc.Node) string {
	if s := tokenSeparator(n); s != "" {
		return s
	}

	return dflt
}

// tidyComment is like comment but makes comment more Go-like.
func tidyComment(dflt string, n cc.Node) (r string) {
	defer func() {
		if !strings.Contains(r, "// <blockquote><pre>") {
			return
		}

		a := strings.Split(r, "\n")
		in := false
		for i, v := range a {
			switch {
			case in:
				if strings.HasPrefix(v, "// </pre></blockquote>") {
					in = false
					a[i] = "//"
					break
				}

				a[i] = fmt.Sprintf("//\t%s", v[3:])
			default:
				if strings.HasPrefix(v, "// <blockquote><pre>") {
					a[i] = "//"
					in = true
				}
			}
		}
		r = strings.Join(a, "\n")
	}()

	s := comment(dflt, n)
	var b strings.Builder
	for len(s) != 0 {
		c := s[0]
		s = s[1:]
		if len(s) == 0 {
			b.WriteByte(c)
			break
		}

		if c != '/' {
			b.WriteByte(c)
			continue
		}

		c2 := s[0]
		s = s[1:]
		switch c2 {
		case '/': // line comment start
			b.WriteByte(c)
			b.WriteByte(c2)
			for {
				c := s[0]
				s = s[1:]
				b.WriteByte(c)
				if c == '\n' {
					break
				}
			}
		case '*': // block comment start
			var b2 strings.Builder
			for {
				c := s[0]
				s = s[1:]
				if c != '*' {
					b2.WriteByte(c)
					continue
				}

			more:
				c2 := s[0]
				s = s[1:]
				if c2 == '*' {
					b2.WriteByte(c)
					goto more
				}

				if c2 != '/' {
					b2.WriteByte(c)
					b2.WriteByte(c2)
					continue
				}

				break
			}
			s2 := b2.String() // comment sans /* prefix and */ suffix
			a := strings.Split(s2, "\n")
			nl := len(s) != 0 && s[0] == '\n'
			if len(a) == 1 { // /* foo */ form
				if nl {
					s = s[1:]
					fmt.Fprintf(&b, "//%s\n", s2)
					break
				}

				fmt.Fprintf(&b, "/*%s*/", s2)
				break
			}

			if !nl {
				fmt.Fprintf(&b, "/*%s*/", s2)
				break
			}

			// Block comment followed by a newline can be safely replaced by a sequence of
			// line comments.  Try to enhance the comment.
			if commentForm1(&b, a) ||
				commentForm2(&b, a) ||
				commentForm3(&b, a) {
				break
			}

			// No enhancement posibilities detected, use the default form.
			if a[len(a)-1] == "" {
				a = a[:len(a)-1]
			}
			fmt.Fprintf(&b, "//%s", a[0])
			for _, v := range a[1:] {
				fmt.Fprintf(&b, "\n//%s", v)
			}
		default:
			b.WriteByte(c)
			b.WriteByte(c2)
		}
	}
	return b.String()
}

func commentForm1(b *strings.Builder, a []string) bool {
	// Example
	//
	//	/*
	//	** Initialize this module.
	//	**
	//	** This Tcl module contains only a single new Tcl command named "sqlite".
	//	** (Hence there is no namespace.  There is no point in using a namespace
	//	** if the extension only supplies one new name!)  The "sqlite" command is
	//	** used to open a new SQLite database.  See the DbMain() routine above
	//	** for additional information.
	//	**
	//	** The EXTERN macros are required by TCL in order to work on windows.
	//	*/
	if strings.TrimSpace(a[0]) != "" {
		return false
	}

	if strings.TrimSpace(a[len(a)-1]) != "" {
		return false
	}

	a = a[1 : len(a)-1]
	if len(a) == 0 {
		return false
	}

	for i, v := range a {
		v = strings.TrimSpace(v)
		if !strings.HasPrefix(v, "*") {
			return false
		}

		a[i] = strings.TrimLeft(v, "*")
	}

	fmt.Fprintf(b, "//%s", a[0])
	for _, v := range a[1:] {
		fmt.Fprintf(b, "\n//%s", v)
	}
	return true
}

func commentForm2(b *strings.Builder, a []string) bool {
	// Example
	//
	//	/**************************** sqlite3_column_  *******************************
	//	** The following routines are used to access elements of the current row
	//	** in the result set.
	//	*/
	if strings.TrimSpace(a[len(a)-1]) != "" {
		return false
	}

	a = a[:len(a)-1]
	if len(a) == 0 {
		return false
	}

	for i, v := range a[1:] {
		v = strings.TrimSpace(v)
		if !strings.HasPrefix(v, "*") {
			return false
		}

		a[i+1] = strings.TrimLeft(v, "*")
	}

	fmt.Fprintf(b, "// %s", strings.TrimSpace(a[0]))
	if strings.HasPrefix(a[0], "**") && strings.HasSuffix(a[0], "**") {
		fmt.Fprintf(b, "\n//")
	}
	for _, v := range a[1:] {
		fmt.Fprintf(b, "\n//%s", v)
	}
	return true
}

func commentForm3(b *strings.Builder, a []string) bool {
	// Example
	//
	//	/* Call sqlite3_shutdown() once before doing anything else. This is to
	//	** test that sqlite3_shutdown() can be safely called by a process before
	//	** sqlite3_initialize() is. */
	for i, v := range a[1:] {
		v = strings.TrimSpace(v)
		if !strings.HasPrefix(v, "*") {
			return false
		}

		a[i+1] = strings.TrimLeft(v, "*")
	}

	fmt.Fprintf(b, "// %s", strings.TrimSpace(a[0]))
	if strings.HasPrefix(a[0], "**") && strings.HasSuffix(a[0], "**") {
		fmt.Fprintf(b, "\n//")
	}
	for _, v := range a[1:] {
		fmt.Fprintf(b, "\n//%s", v)
	}
	return true
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
	return strings.ReplaceAll(tok.Sep.String(), "\x0c", "")
}

type initPatch struct {
	t    cc.Type
	init *cc.Initializer
	fld  cc.Field
}

type tld struct {
	name    string // Can differ from the original one.
	patches []initPatch
}

type block struct {
	block      *cc.CompoundStatement
	decls      []*cc.Declaration // What to declare in this block.
	enumConsts map[cc.StringID]string
	enumSpecs  map[*cc.EnumSpecifier]*enumSpec
	params     []*cc.Parameter
	parent     *block
	scope      scope

	noDecl  bool // Locals declared in one of the parent scopes.
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

func (b *block) isFlat() bool {
	//TODO return b.block.IsJumpTarget()
	return true
}

type local struct {
	name string
	off  uintptr // If isPinned: bp+off

	forceRead bool // Possibly never read.
	isPinned  bool // Prevent this local from being placed in Go movable stack.
}

type switchState int

const (
	_                 switchState = iota // Not in switch.
	inSwitchFirst                        // Before seeing "case/default".
	inSwitchCase                         // Seen "case/default".
	inSwitchSeenBreak                    // In switch "case/default" and seen "break/return".
	inSwitchFlat
)

type function struct {
	block            *block
	blocks           map[*cc.CompoundStatement]*block
	bpName           string
	breakCtx         int //TODO merge with continueCtx
	condInitPrefix   func()
	continueCtx      int
	flatLabels       int
	flatSwitchLabels map[*cc.LabeledStatement]int
	fndef            *cc.FunctionDefinition
	gen              *project
	ifCtx            cc.Node
	ignore           map[*cc.Declarator]bool // Pseudo declarators
	labelNames       map[cc.StringID]string
	labels           scope
	locals           map[*cc.Declarator]*local
	off              uintptr         // bp+off allocs
	params           []*cc.Parameter // May differ from what fndef says
	project          *project
	rt               cc.Type // May differ from what fndef says
	scope            scope
	switchCtx        switchState
	tlsName          string
	top              *block
	unusedLabels     map[cc.StringID]struct{}
	vaLists          map[*cc.PostfixExpression]uintptr
	vaName           string
	vaType           cc.Type

	hasJumps            bool
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
		hasJumps:            n.CompoundStatement.IsJumpTarget(),
		ignore:              ignore,
		locals:              map[*cc.Declarator]*local{},
		mainSignatureForced: mainSignatureForced,
		params:              params,
		project:             p,
		rt:                  rt,
		scope:               p.newScope(),
		unusedLabels:        map[cc.StringID]struct{}{},
		vaLists:             map[*cc.PostfixExpression]uintptr{},
	}
	f.tlsName = f.scope.take("tls")
	if t.IsVariadic() {
		f.vaName = f.scope.take("va")
	}
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
			if tld := f.project.externs[v]; tld != nil {
				block.scope.take(tld.name)
			}
		}
	}
	for _, v := range n.CompoundStatements() {
		f.layoutBlocks(v)
	}
	f.renameLabels()
	f.staticAllocsAndPinned(n.CompoundStatement)
	return f
}

func (f *function) flatLabel() int {
	f.flatLabels++
	return f.flatLabels
}

func (f *function) renameLabels() {
	var a []cc.StringID
	for _, v := range f.fndef.Labels {
		if v.Case != cc.LabeledStatementLabel {
			continue
		}

		a = append(a, v.Token.Value)
		f.unusedLabels[v.Token.Value] = struct{}{}
	}
	for _, v := range f.fndef.Gotos {
		delete(f.unusedLabels, v.Token2.Value)
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
	for _, v := range f.params {
		switch {
		case v.Type().Kind() == cc.Array && v.Type().IsVLA():
			f.project.err(f.fndef, "variable length arrays not supported")
		case v.Type().Kind() == cc.Union:
			f.pin(v.Declarator())
		case v.Type().Kind() == cc.Struct && mustPinStruct(v.Type()):
			f.pin(v.Declarator())
		}
	}

	cc.Inspect(n, func(n cc.Node, entry bool) bool {
		if !entry {
			return true
		}

		switch x := n.(type) {
		case *cc.Declarator:
			if x.Type().Kind() == cc.Array && x.Type().IsVLA() {
				f.project.err(x, "variable length arrays not supported")
			}
			switch {
			case x.IsTypedefName:
				// nop
			case x.Linkage == cc.None && x.Type().Kind() == cc.Union:
				f.pin(x)
			case x.Linkage == cc.None && x.Type().Kind() == cc.Struct && mustPinStruct(x.Type()):
				f.pin(x)
			case x.Linkage == cc.None && x.Type().Kind() == cc.Array:
				t := x.Type().Elem()
				for ; t.Kind() == cc.Array; t = t.Elem() {
				}
				if t.Kind() == cc.Struct || t.Kind() == cc.Union {
					f.pin(x)
				}
			case x.Linkage == cc.None && x.Type().Kind() == cc.Ptr && x.Type().Elem().Kind() == cc.Function:
				f.pin(x)
			}
		case *cc.CastExpression:
			switch x.Case {
			case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
				if x.TypeName.Type().Kind() != cc.Void {
					break
				}

				if d := x.CastExpression.Declarator(); d != nil {
					if local := f.locals[d]; local != nil {
						local.forceRead = true
					}
				}
			}
		case *cc.AssignmentExpression:
			switch x.Case {
			case cc.AssignmentExpressionAssign: // foo = bar;
				// UnaryExpression '=' AssignmentExpression
				if d := x.AssignmentExpression.Declarator(); d != nil { // bar
					if f.project.isArrayDeclarator(d) {
						f.pin(d)
					}
				}
				lhs := x.UnaryExpression
				if t := lhs.Operand.Type(); t.IsBitFieldType() {
					if d := lhs.Declarator(); d != nil {
						f.pin(d)
					}

				}
			case
				cc.AssignmentExpressionMul,
				cc.AssignmentExpressionDiv,
				cc.AssignmentExpressionMod,
				cc.AssignmentExpressionAdd,
				cc.AssignmentExpressionSub,
				cc.AssignmentExpressionLsh,
				cc.AssignmentExpressionRsh,
				cc.AssignmentExpressionAnd,
				cc.AssignmentExpressionXor,
				cc.AssignmentExpressionOr:

				var d *cc.Declarator
				if f.project.detectArray(f, x.UnaryExpression, false, true, &d) {
					f.pin(d)
				}
			}
		case *cc.AdditiveExpression:
			var d *cc.Declarator
			if f.project.detectArray(f, x, false, true, &d) {
				f.pin(d)
			}
		case *cc.PostfixExpression:
			switch x.Case {
			case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
				var d *cc.Declarator
				if f.project.detectArray(f, x.PostfixExpression, false, false, &d) {
					f.pin(d)
				}
			case
				cc.PostfixExpressionInc, // PostfixExpression "++"
				cc.PostfixExpressionDec: // PostfixExpression "--"
				if x.Operand.Type().IsBitFieldType() {
					var d *cc.Declarator
					switch pe := x.PostfixExpression; pe.Case {
					case cc.PostfixExpressionSelect:
						d = pe.PostfixExpression.Declarator()
					case cc.PostfixExpressionPSelect:
						// nop
					default:
						panic(todo("", pe.Case))
					}
					if d != nil {
						f.pin(d)
					}
				}
			}
		}

		x, ok := n.(*cc.PostfixExpression)
		if !ok || x.Case != cc.PostfixExpressionCall {
			return true
		}

		ft := funcType(x.PostfixExpression.Operand.Type())
		if ft.Kind() != cc.Function {
			return true
		}

		for list := x.ArgumentExpressionList; list != nil; list = list.ArgumentExpressionList {
			if d := list.AssignmentExpression.Declarator(); d != nil {
				if f.project.isArrayDeclarator(d) {
					f.pin(d)
					continue
				}
			}

			var d *cc.Declarator
			if f.project.detectArray(f, list.AssignmentExpression, false, false, &d) {
				f.pin(d)
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
			case cc.Array, cc.Ptr, cc.Double, cc.Float, cc.Function:
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

func mustPinStruct(t cc.Type) bool {
	for idx := []int{0}; idx[0] < t.NumField(); idx[0]++ {
		f := t.FieldByIndex(idx)
		if f.IsBitField() {
			return true
		}

		switch ft := f.Type(); ft.Kind() {
		case cc.Struct:
			if mustPinStruct(f.Type()) {
				return true
			}
		case cc.Array, cc.Union:
			return true
		case cc.Ptr:
			if ft.Elem().Kind() == cc.Function {
				return true
			}
		}
	}
	return false
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

type declarator interface {
	Declarator() *cc.Declarator
	cc.Node
}

func (p *project) isArrayParameterDeclarator(d *cc.Declarator) bool {
	if d.Type().Kind() == cc.Array {
		if d.Type().IsVLA() {
			p.err(d, "variable length arrays not supported")
			return false
		}

		return d.IsParameter
	}

	return false
}

func (p *project) isArrayDeclarator(d *cc.Declarator) bool {
	if d.Type().Kind() == cc.Array {
		if d.Type().IsVLA() {
			p.err(d, "variable length arrays not supported")
			return false
		}

		return !d.IsParameter
	}

	return false
}

func (p *project) isArrayParameter(n declarator, t cc.Type) bool {
	if t.Kind() != cc.Array {
		return false
	}

	if t.IsVLA() {
		p.err(n, "variable length arrays not supported")
		return false
	}

	if d := n.Declarator(); d != nil {
		return d.IsParameter
	}

	return false
}

func (p *project) isArrayOrPinnedArray(f *function, n declarator, t cc.Type) (r bool) {
	if t.Kind() != cc.Array {
		return false
	}

	if t.IsVLA() {
		p.err(n, "variable length arrays not supported")
		return false
	}

	if d := n.Declarator(); d != nil {
		return !d.IsParameter
	}

	return p.detectArray(f, n.(cc.Node), true, true, nil)
}

func (p *project) detectArray(f *function, n cc.Node, pinnedOk, recursiveOk bool, out **cc.Declarator) bool {
	switch x := n.(type) {
	case *cc.AssignmentExpression:
		switch x.Case {
		case cc.AssignmentExpressionCond: // ConditionalExpression
			return p.detectArray(f, x.ConditionalExpression, pinnedOk, recursiveOk, out)
		case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
			return p.detectArray(f, x.UnaryExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.ConditionalExpression:
		switch x.Case {
		case cc.ConditionalExpressionLOr: // LogicalOrExpression
			return p.detectArray(f, x.LogicalOrExpression, pinnedOk, recursiveOk, out)
		case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
			return p.detectArray(f, x.LogicalOrExpression, pinnedOk, recursiveOk, out) ||
				p.detectArray(f, x.Expression, pinnedOk, recursiveOk, out) ||
				p.detectArray(f, x.ConditionalExpression, pinnedOk, recursiveOk, out)
		default:
			panic(todo("", pos(x), x.Case))
		}
	case *cc.LogicalOrExpression:
		switch x.Case {
		case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
			return p.detectArray(f, x.LogicalAndExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.LogicalAndExpression:
		switch x.Case {
		case cc.LogicalAndExpressionOr: // InclusiveOrExpression
			return p.detectArray(f, x.InclusiveOrExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.InclusiveOrExpression:
		switch x.Case {
		case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
			return p.detectArray(f, x.ExclusiveOrExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.ExclusiveOrExpression:
		switch x.Case {
		case cc.ExclusiveOrExpressionAnd: // AndExpression
			return p.detectArray(f, x.AndExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.AndExpression:
		switch x.Case {
		case cc.AndExpressionEq: // EqualityExpression
			return p.detectArray(f, x.EqualityExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.EqualityExpression:
		switch x.Case {
		case cc.EqualityExpressionRel: // RelationalExpression
			return p.detectArray(f, x.RelationalExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.RelationalExpression:
		switch x.Case {
		case cc.RelationalExpressionShift: // ShiftExpression
			return p.detectArray(f, x.ShiftExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.ShiftExpression:
		switch x.Case {
		case cc.ShiftExpressionAdd: // AdditiveExpression
			return p.detectArray(f, x.AdditiveExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.AdditiveExpression:
		switch x.Case {
		case cc.AdditiveExpressionMul: // MultiplicativeExpression
			return p.detectArray(f, x.MultiplicativeExpression, pinnedOk, recursiveOk, out)
		case
			cc.AdditiveExpressionSub, // AdditiveExpression '-' MultiplicativeExpression
			cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression

			return p.detectArray(f, x.AdditiveExpression, pinnedOk, recursiveOk, out) || p.detectArray(f, x.MultiplicativeExpression, pinnedOk, recursiveOk, out)
		default:
			panic(todo("", pos(x), x.Case))
		}
	case *cc.MultiplicativeExpression:
		switch x.Case {
		case cc.MultiplicativeExpressionCast: // CastExpression
			return p.detectArray(f, x.CastExpression, pinnedOk, recursiveOk, out)
		default:
			return false
		}
	case *cc.CastExpression:
		switch x.Case {
		case cc.CastExpressionUnary: // UnaryExpression
			return p.detectArray(f, x.UnaryExpression, pinnedOk, recursiveOk, out)
		case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
			return p.detectArray(f, x.CastExpression, pinnedOk, recursiveOk, out)
		default:
			panic(todo("", pos(x), x.Case))
		}
	case *cc.UnaryExpression:
		switch x.Case {
		case cc.UnaryExpressionPostfix: // PostfixExpression
			return p.detectArray(f, x.PostfixExpression, pinnedOk, recursiveOk, out)
		case
			cc.UnaryExpressionDeref,  // '*' CastExpression
			cc.UnaryExpressionAddrof: // '&' CastExpression

			return p.detectArray(f, x.CastExpression, pinnedOk, recursiveOk, out)
		case
			cc.UnaryExpressionSizeofExpr,  // "sizeof" UnaryExpression
			cc.UnaryExpressionSizeofType,  // "sizeof" '(' TypeName ')'
			cc.UnaryExpressionMinus,       // '-' CastExpression
			cc.UnaryExpressionCpl,         // '~' CastExpression
			cc.UnaryExpressionAlignofExpr, // "_Alignof" UnaryExpression
			cc.UnaryExpressionAlignofType, // "_Alignof" '(' TypeName ')'
			cc.UnaryExpressionNot,         // '!' CastExpression
			cc.UnaryExpressionInc,         // "++" UnaryExpression
			cc.UnaryExpressionDec,         // "--" UnaryExpression
			cc.UnaryExpressionPlus:        // '+' CastExpression

			return false
		default:
			panic(todo("", pos(x), x.Case))
		}
	case *cc.PostfixExpression:
		switch x.Case {
		case cc.PostfixExpressionPrimary: // PrimaryExpression
			return p.detectArray(f, x.PrimaryExpression, pinnedOk, recursiveOk, out)
		case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
			return recursiveOk && p.detectArray(f, x.PostfixExpression, pinnedOk, recursiveOk, out)
		case
			cc.PostfixExpressionSelect,  // PostfixExpression '.' IDENTIFIER
			cc.PostfixExpressionDec,     // PostfixExpression "--"
			cc.PostfixExpressionInc,     // PostfixExpression "++"
			cc.PostfixExpressionCall,    // PostfixExpression '(' ArgumentExpressionList ')'
			cc.PostfixExpressionComplit, // '(' TypeName ')' '{' InitializerList ',' '}'
			cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER

			return false
		default:
			panic(todo("", pos(x), x.Case))
		}
	case *cc.PrimaryExpression:
		switch x.Case {
		case
			cc.PrimaryExpressionString,  // STRINGLITERAL
			cc.PrimaryExpressionEnum,    // ENUMCONST
			cc.PrimaryExpressionChar,    // CHARCONST
			cc.PrimaryExpressionLChar,   // LONGCHARCONST
			cc.PrimaryExpressionLString, // LONGSTRINGLITERAL
			cc.PrimaryExpressionFloat,   // FLOATCONST
			cc.PrimaryExpressionInt:     // INTCONST

			return false
		case cc.PrimaryExpressionIdent: // IDENTIFIER
			d := x.Declarator()
			if d == nil || d.IsParameter {
				return false
			}

			if d.Type().Kind() != cc.Array {
				return false
			}

			if d.Type().IsVLA() {
				return false
			}

			if pinnedOk {
				if out != nil {
					*out = d
				}
				return true
			}

			local := f.locals[d]
			if local == nil || local.isPinned {
				return false
			}

			if out != nil {
				*out = d
			}
			return true
		case cc.PrimaryExpressionExpr: // '(' Expression ')'
			return p.detectArray(f, x.Expression, pinnedOk, recursiveOk, out)
		case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
			p.err(x, "statement expressions not supported")
			return false
		default:
			panic(todo("", pos(x), x.Case))
		}
	case *cc.Expression:
		switch x.Case {
		case cc.ExpressionAssign: // AssignmentExpression
			return p.detectArray(f, x.AssignmentExpression, pinnedOk, recursiveOk, out)
		case cc.ExpressionComma: // Expression ',' AssignmentExpression
			return p.detectArray(f, x.Expression, pinnedOk, recursiveOk, out) || p.detectArray(f, x.AssignmentExpression, pinnedOk, recursiveOk, out)
		default:
			panic(todo("", pos(x), x.Case))
		}
	default:
		panic(todo("%T", x))
	}
}

func (p *project) isArray(f *function, n declarator, t cc.Type) (r bool) {
	if t.Kind() != cc.Array {
		return false
	}

	if t.IsVLA() {
		p.err(n, "variable length arrays not supported")
		return false
	}

	if f == nil {
		return true
	}

	if d := n.Declarator(); d != nil {
		local := f.locals[d]
		return !d.IsParameter && (local == nil || !local.isPinned)
	}

	return p.detectArray(f, n.(cc.Node), false, true, nil)
}

func roundup(n, to uintptr) uintptr {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}

// Return n's position with path reduced to baseName(path).
func pos(n cc.Node) (r token.Position) {
	if n == nil {
		return r
	}

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
	f.off = local.off + paramTypeDecay(d).Size()
}

func paramTypeDecay(d *cc.Declarator) (r cc.Type) {
	r = d.Type()
	if d.IsParameter && r.Kind() == cc.Array {
		r = r.Decay()
	}
	return r
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
	block.scope.take(f.tlsName)
	if f.vaName != "" {
		block.scope.take(f.vaName)
	}
	for _, item := range work {
		d := item.d
		if f.ignore[d] {
			continue
		}

		if !f.ignore[d] && d.IsStatic() {
			continue
		}

		if d.IsFunctionPrototype() || d.IsExtern() {
			continue
		}

		local := &local{forceRead: d.Read == 0}
		if t := d.Type(); t != nil && t.Name() == idVaList {
			local.forceRead = true
		}
		f.locals[d] = local
		if d.AddressTaken {
			f.pin(d)
		}
		local.name = block.scope.take(d.Name().String())
	}
}

func (f *function) layoutLocals(parent *block, n *cc.CompoundStatement, params []*cc.Parameter) {
	block := newBlock(parent, n, n.Declarations(), params, f.project.newScope(), n.IsJumpTarget())
	f.blocks[n] = block
	if parent == nil {
		f.top = block
		f.top.topDecl = f.hasJumps
	}
	for _, ch := range n.Children() {
		f.layoutLocals(block, ch, nil)
		if f.hasJumps {
			chb := f.blocks[ch]
			chb.noDecl = true
			f.top.decls = append(f.top.decls, chb.decls...)
			chb.decls = nil
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
	p.w("%s", tidyComment("\n", n.decl))
	p.w("const ( /* %v: */", pos(n.decl))
	for list := n.spec.EnumeratorList; list != nil; list = list.EnumeratorList {
		en := list.Enumerator
		p.w("%s%s = %v;", tidyComment("\n", en), enumConsts[en.Token.Value], en.Operand.Value())
	}
	p.w(")")
}

type typedef struct {
	sig string
	tld *tld
}

type project struct {
	ast                *cc.AST
	buf                bytes.Buffer
	capi               []string
	defines            []string
	enumConsts         map[cc.StringID]string
	enumSpecs          map[*cc.EnumSpecifier]*enumSpec
	errors             scanner.ErrorList
	externs            map[cc.StringID]*tld
	imports            map[string]*imported // C name: import info
	localTaggedStructs []func()
	mainName           string
	scope              scope
	staticQueue        []*cc.InitDeclarator
	structs            map[cc.StringID]*taggedStruct // key: C tag
	task               *task
	tlds               map[*cc.Declarator]*tld
	ts                 bytes.Buffer // Text segment
	ts4                []rune       // Text segment, alignment 4
	ts4Name            string
	ts4NameP           string
	ts4Offs            map[cc.StringID]uintptr
	tsName             string
	tsNameP            string
	tsOffs             map[cc.StringID]uintptr
	typedefTypes       map[cc.StringID]*typedef
	typedefsEmited     map[string]struct{}
	wanted             map[*cc.Declarator]struct{}

	isMain bool
}

func newProject(t *task) (*project, error) {
	if t.cfg.ABI.Types[cc.Int].Size != 4 { // We're assuming wchar_t is int32.
		return nil, fmt.Errorf("unsupported C int size")
	}

	p := &project{
		enumConsts:     map[cc.StringID]string{},
		externs:        map[cc.StringID]*tld{},
		imports:        map[string]*imported{},
		scope:          newScope(),
		task:           t,
		tlds:           map[*cc.Declarator]*tld{},
		ts4Offs:        map[cc.StringID]uintptr{},
		tsOffs:         map[cc.StringID]uintptr{},
		typedefTypes:   map[cc.StringID]*typedef{},
		typedefsEmited: map[string]struct{}{},
		wanted:         map[*cc.Declarator]struct{}{},
	}
	p.scope.take("CAPI")
	for _, v := range t.imported {
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
	if err := p.layout(); err != nil {
		return nil, err
	}

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
	if len(p.errors) >= 10 {
		return
	}

	switch {
	case n == nil:
		p.errors.Add(token.Position{}, fmt.Sprintf(s, args...))
	default:
		p.errors.Add(token.Position(n.Position()), fmt.Sprintf(s, args...))
		if len(p.errors) == 10 {
			p.errors.Add(token.Position(n.Position()), tooManyErrors)
		}
	}
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

	if err := p.layoutDefines(); err != nil {
		return err
	}

	return p.layoutStaticLocals()
}

func (p *project) layoutDefines() error {
	if !p.task.exportDefinesValid {
		return nil
	}

	var prefix = p.task.exportDefines
	taken := map[cc.StringID]struct{}{}
	for _, ast := range p.task.asts {
		var a []cc.StringID
		for nm, m := range ast.Macros {
			if m.IsFnLike() {
				continue
			}

			if strings.HasPrefix(nm.String(), "__") {
				continue
			}

			if _, ok := taken[nm]; ok {
				continue
			}

			taken[nm] = struct{}{}
			a = append(a, nm)
		}
		sort.Slice(a, func(i, j int) bool { return a[i].String() < a[j].String() })
		for _, nm := range a {
			m := ast.Macros[nm]
			src := evalMacro(m, ast)
			if src == "" {
				continue
			}

			name := nm.String()
			switch {
			case prefix == "":
				name = capitalize(name)
			default:
				name = prefix + name
			}
			name = p.scope.take(name)
			p.defines = append(p.defines, fmt.Sprintf("%s = %s", name, src))
		}
	}
	return nil
}

func evalMacro(m *cc.Macro, ast *cc.AST) string {
	toks := m.ReplacementTokens()
	if len(toks) != 1 {
		return evalMacro2(m, ast)
	}

	src := strings.TrimSpace(toks[0].Src.String())
	if len(src) == 0 {
		return ""
	}

	neg := ""
	switch src[0] {
	case '"':
		if _, err := strconv.Unquote(src); err == nil {
			return src
		}
	case '-':
		neg = "-"
		src = src[1:]
		fallthrough
	default:
		src = strings.TrimRight(src, "lLuUfF")
		if _, err := strconv.ParseUint(src, 0, 64); err == nil {
			return neg + src
		}

		if _, err := strconv.ParseFloat(src, 64); err == nil {
			return neg + src
		}
	}

	return evalMacro2(m, ast)
}

func evalMacro2(m *cc.Macro, ast *cc.AST) string {
	op, err := ast.Eval(m)
	if err != nil {
		return ""
	}

	switch x := op.Value().(type) {
	case cc.Int64Value:
		return fmt.Sprintf("%d", x)
	default:
		panic(todo(""))
	}
}

func (p *project) layoutEnums() error {
	return nil //TODO
	m := map[cc.StringID]cc.Value{}
	var enumList []*cc.EnumSpecifier
	enums := map[*cc.EnumSpecifier]*enumSpec{}
out:
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
			if enums == nil {
				enumList = enumList[:0]
				break out
			}
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

					p.tlds[x] = &tld{name: p.scope.take(x.Name().String())}
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
		cc.Inspect(v.TranslationUnit, func(n cc.Node, entry bool) bool {
			if entry {
				switch d := n.(type) {
				case *cc.Declarator:
					if nm := d.Name().String(); strings.HasPrefix(nm, "_") {
						break
					}

					captureStructTags(d, d.Type(), m, &tags)
				}
			}
			return true
		})
	}
	sort.Slice(tags, func(i, j int) bool { return tags[i].String() < tags[j].String() })
	for _, k := range tags {
		v := m[k]
		//TODO rename conflicts
		if v.conflicts {
			delete(m, k)
			continue
		}

		v.name = p.scope.take(k.String())
	}
	for _, k := range tags {
		v := m[k]
		if v != nil {
			v.gotyp = p.structType(v.ctyp)
		}
	}
	p.structs = m
	return nil
}

func captureStructTags(n cc.Node, t cc.Type, m map[cc.StringID]*taggedStruct, tags *[]cc.StringID) {
	t = t.Alias()
	for t.Kind() == cc.Ptr {
		t = t.Alias().Elem().Alias()
	}
	if t.Kind() == cc.Invalid || t.IsIncomplete() {
		return
	}

	switch t.Kind() {
	case cc.Struct, cc.Union:
		tag := t.Tag()
		if tag == 0 {
			return
		}

		ex := m[tag]
		if ex != nil {
			ts := typeSignature(nil, t)
			exs := typeSignature(nil, ex.ctyp)
			if ts != exs {
				ex.conflicts = true
			}
			return
		}

		nf := t.NumField()
		m[tag] = &taggedStruct{ctyp: t}
		for idx := []int{0}; idx[0] < nf; idx[0]++ {
			captureStructTags(n, t.FieldByIndex(idx).Type(), m, tags)
		}
		*tags = append(*tags, tag)
	case cc.Array:
		captureStructTags(n, t.Elem(), m, tags)
	}
}

func (p *project) structType(t cc.Type) string {
	switch t.Kind() {
	case cc.Struct, cc.Union:
		tag := t.Tag()
		if tag != 0 && p.structs != nil {
			s := p.structs[tag]
			if s == nil {
				return p.structLiteral(t)
			}

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
	var b strings.Builder
	switch t.Kind() {
	case cc.Struct:
		info := p.structLayout(t)
		b.WriteString("struct {")
		for _, off := range info.offs {
			flds := info.flds[off]
			f := flds[0]
			switch pad := info.padBefore[f]; {
			case pad < 0:
				continue
			case pad > 0:
				fmt.Fprintf(&b, "_ [%d]byte;", pad)
			}
			switch {
			case f.IsBitField():
				var a []string
				for _, f := range flds {
					if !f.IsBitField() {
						panic(todo("internal error"))
					}
					a = append(a, fmt.Sprintf("%s %s: %d", f.Type(), f.Name(), f.BitFieldWidth()))
				}
				fmt.Fprintf(&b, "%s uint%d /* %s */;", p.bitFieldName(f), f.BitFieldBlockWidth(), strings.Join(a, ", "))
			default:
				fmt.Fprintf(&b, "%s %s;", p.fieldName2(f), p.typ(nil, f.Type()))
			}
		}
		if info.padAfter != 0 {
			fmt.Fprintf(&b, "_ [%d]byte;", info.padAfter)
		}
		b.WriteByte('}')
	case cc.Union:
		b.WriteString("struct {")
		al := uintptr(t.Align())
		sz := t.Size()
		if al > sz {
			panic(todo(""))
		}

		f := t.FieldByIndex([]int{0})
		ft := f.Type()
		if f.IsBitField() {
			panic(todo(""))
		}

		fmt.Fprintf(&b, "%s %s;", p.fieldName2(f), p.typ(nil, ft))
		if pad := sz - ft.Size(); pad != 0 {
			fmt.Fprintf(&b, "_ [%d]byte;", pad)
		}
		b.WriteByte('}')
	default:
		panic(todo("internal error: %v", t.Kind()))
	}
	return b.String()
}

type structInfo struct {
	offs      []uintptr
	flds      map[uintptr][]cc.Field
	padBefore map[cc.Field]int
	padAfter  int
}

func (p *project) structLayout(t cc.Type) *structInfo {
	nf := t.NumField()
	flds := map[uintptr][]cc.Field{}
	for idx := []int{0}; idx[0] < nf; idx[0]++ {
		f := t.FieldByIndex(idx)
		off := f.Offset()
		flds[off] = append(flds[off], f)
	}
	var offs []uintptr
	for k := range flds {
		offs = append(offs, k)
	}
	sort.Slice(offs, func(i, j int) bool { return offs[i] < offs[j] })
	var pads map[cc.Field]int
	var pos uintptr
	for i, off := range offs {
		f := flds[off][0]
		ft := f.Type()
		pos = roundup(pos, uintptr(ft.Align()))
		if i > 0 {
			if p := int(off - pos); p != 0 {
				if pads == nil {
					pads = map[cc.Field]int{}
				}
				pads[f] = p
				pos = off
			}
		}
		switch {
		case ft.IsBitFieldType():
			pos += uintptr(f.BitFieldBlockWidth()) >> 3
		default:
			pos += ft.Size()
		}
	}
	return &structInfo{
		offs:      offs,
		flds:      flds,
		padBefore: pads,
		padAfter:  int(t.Size() - pos),
	}
}

func (p *project) bitFieldName(f cc.Field) string {
	if id := f.Name(); id != 0 {
		return p.fieldName(id)
	}

	return fmt.Sprintf("__%d", f.Offset())
}

func (p *project) fieldName2(f cc.Field) string {
	if f.Name() != 0 {
		return p.fieldName(f.Name())
	}

	return p.fieldName(cc.String(fmt.Sprintf("__%d", f.Offset())))
}

func (p *project) fieldName(id cc.StringID) string {
	if id == 0 {
		panic(todo(""))
	}

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

func (p *project) typ(nd cc.Node, t cc.Type) string {
	if t.IsIncomplete() {
		panic(todo("", pos(nd), t))
	}

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
		if t.Size() > 8 {
			p.err(nil, "unsupported C type: %v", t)
		}
		fmt.Fprintf(&b, "int%d", 8*t.Size())
		return b.String()
	}

	switch t.Kind() {
	case cc.Ptr, cc.Function:
		return "uintptr"
	case cc.Double:
		return "float64"
	case cc.Float:
		return "float32"
	case cc.Array:
		n := t.Len()
		if n == 0 {
			p.err(nil, "unsupported C type: %v", t)
		}
		fmt.Fprintf(&b, "[%d]%s", n, p.typ(nd, t.Elem()))
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

	panic(todo("", pos(nd), t.Kind(), t))
}

func (p *project) layoutTLDs() error {
	const (
		doNotExport = iota
		exportCapitalize
		exportPrefix
	)

	var exportExtern, exportTypedef int
	if p.task.exportExternsValid {
		switch {
		case p.task.exportExterns != "":
			exportExtern = exportPrefix
		default:
			exportExtern = exportCapitalize
		}
	}
	if p.task.exportTypedefsValid {
		switch {
		case p.task.exportTypedefs != "":
			exportTypedef = exportPrefix
		default:
			exportTypedef = exportCapitalize
		}
	}
	var a []*cc.Declarator
	if p.task.pkgName == "" || p.task.pkgName == "main" {
	out:
		for _, ast := range p.task.asts {
			if a := ast.Scope[idMain]; len(a) != 0 {
				switch x := a[0].(type) {
				case *cc.Declarator:
					if x.Linkage == cc.External {
						p.isMain = true
						p.scope.take("main")
						break out
					}
				}
			}
		}
	}
	for _, ast := range p.task.asts {
		a = a[:0]
		for d := range ast.TLD {
			if d.IsFunctionPrototype() {
				continue
			}

			a = append(a, d)
			p.wanted[d] = struct{}{}
		}
		sort.Slice(a, func(i, j int) bool {
			return a[i].NameTok().Seq() < a[j].NameTok().Seq()
		})
		for _, d := range a {
			switch d.Type().Kind() {
			case cc.Struct, cc.Union:
				p.checkAlignAttr(d.Type())
			}
			nm := d.Name()
			switch d.Linkage {
			case cc.External:
				if ex := p.externs[nm]; ex != nil {
					panic(todo("", ex.name, d.Position(), d.Name()))
				}

				isMain := p.isMain && nm == idMain
				name := d.Name().String()
				switch exportExtern {
				case doNotExport:
					// nop
				case exportCapitalize:
					name = capitalize(name)
				case exportPrefix:
					name = p.task.exportExterns + name
				}
				name = p.scope.take(name)
				if isMain {
					p.mainName = name
				}
				tld := &tld{name: name}
				p.externs[nm] = tld
				for _, v := range ast.Scope[nm] {
					if d, ok := v.(*cc.Declarator); ok {
						p.tlds[d] = tld
					}
				}
				if !isMain {
					p.capi = append(p.capi, d.Name().String())
				}
			case cc.Internal:
				name := nm.String()
				if token.IsExported(name) && !p.isMain && p.task.exportExternsValid {
					name = "s" + name
				}
				tld := &tld{name: p.scope.take(name)}
				for _, v := range ast.Scope[nm] {
					if d, ok := v.(*cc.Declarator); ok {
						p.tlds[d] = tld
					}
				}
			case cc.None:
				if d.IsTypedefName {
					if d.Type().IsIncomplete() {
						break
					}

					name := nm.String()
					if strings.HasPrefix(name, "__") {
						break
					}

					ex, ok := p.typedefTypes[d.Name()]
					if ok {
						sig := typeSignature(d, d.Type())
						if ex.sig == sig {
							tld := ex.tld
							for _, v := range ast.Scope[nm] {
								if d, ok := v.(*cc.Declarator); ok {
									p.tlds[d] = tld
								}
							}
							break
						}
					}

					switch exportTypedef {
					case doNotExport:
						// nop
					case exportCapitalize:
						name = capitalize(name)
					case exportPrefix:
						name = p.task.exportExterns + name
					}
					tld := &tld{name: p.scope.take(name)}
					p.typedefTypes[d.Name()] = &typedef{typeSignature(d, d.Type()), tld}
					for _, v := range ast.Scope[nm] {
						if d, ok := v.(*cc.Declarator); ok {
							p.tlds[d] = tld
						}
					}
				}
			default:
				panic(todo("", pos(d), nm, d.Linkage))
			}
		}
	}
	for _, ast := range p.task.asts {
		for list := ast.TranslationUnit; list != nil; list = list.TranslationUnit {
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
					if x.IsFunctionPrototype() {
						nm := x.Name()
						if extern := p.externs[nm]; extern != nil {
							break
						}

						tld := &tld{name: nm.String()}
						for _, nd := range ast.Scope[nm] {
							if d, ok := nd.(*cc.Declarator); ok {
								p.tlds[d] = tld
							}
						}
					}

				}
				return true
			})
		}
	}
	return nil
}

func (p *project) checkAlignAttr(t cc.Type) (r bool) {
	r = true
	for _, v := range t.Attributes() {
		cc.Inspect(v, func(n cc.Node, entry bool) bool {
			if !entry {
				return true
			}

			switch x := n.(type) {
			case *cc.AttributeValue:
				if x.Token.Value != idAligned {
					break
				}

				switch v := x.ExpressionList.AssignmentExpression.Operand.Value().(type) {
				default:
					panic(todo("%T(%v)", v, v))
				}
			}
			return true
		})
	}
	switch t.Kind() {
	case cc.Struct, cc.Union:
		for i := []int{0}; i[0] < t.NumField(); i[0]++ {
			f := t.FieldByIndex(i)
			if !p.checkAlignAttr(f.Type()) {
				return false
			}

			sd := f.Declarator()
			if sd == nil {
				continue
			}

			cc.Inspect(sd.StructDeclaration().SpecifierQualifierList, func(n cc.Node, entry bool) bool {
				if !entry {
					return true
				}

				switch x := n.(type) {
				case *cc.AttributeValue:
					if x.Token.Value != idAligned {
						break
					}

					switch v := x.ExpressionList.AssignmentExpression.Operand.Value().(type) {
					case cc.Int64Value:
						if int(v) != t.Align() {
							p.err(sd, "unsupported alignment")
							r = false
							return false
						}
					default:
						panic(todo("%T(%v)", v, v))
					}
				}
				return true
			})
			if !r {
				return false
			}
		}
	}
	return r
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
	if len(p.defines) != 0 {
		p.w("\nconst (")
		p.w("%s", strings.Join(p.defines, "\n"))
		p.w("\n)\n\n")
	}
	for _, v := range p.task.asts {
		p.oneAST(v)
	}
	sort.Slice(p.task.imported, func(i, j int) bool { return p.task.imported[i].path < p.task.imported[j].path })
	p.o(`import (
	"math"
	"reflect"
	"unsafe"
`)
	first := true
	for _, v := range p.task.imported {
		if v.used {
			if first {
				p.o("\n")
				first = false
			}
			p.o("\t%q\n", v.path)
		}
	}
	p.o(`)

var _ = math.Pi
var _ reflect.Kind
var _ unsafe.Pointer
`)
	if p.isMain {
		p.o(`
func main() { %sStart(%s) }`, p.task.crt, p.mainName)
	}
	p.flushStructs()
	p.initPatches()
	p.flushTS()
	p.flushCAPI()
	if _, err := p.buf.WriteTo(p.task.out); err != nil {
		return err
	}

	return p.Err()
}

func (p *project) flushCAPI() {
	if p.isMain {
		return
	}

	b := bytes.NewBuffer(nil)
	fmt.Fprintf(b, `// Code generated by '%s %s', DO NOT EDIT.

package %s

`,
		filepath.Base(p.task.args[0]),
		strings.Join(p.task.args[1:], " "),
		p.task.pkgName,
	)
	fmt.Fprintf(b, "\n\nvar CAPI = map[string]struct{}{")
	sort.Strings(p.capi)
	for _, nm := range p.capi {
		fmt.Fprintf(b, "\n%q: {},", nm)
	}
	fmt.Fprintf(b, "\n}\n")
	if err := ioutil.WriteFile(p.task.capif, b.Bytes(), 0644); err != nil {
		p.err(nil, "%v", err)
		return
	}

	if out, err := exec.Command("gofmt", "-l", "-s", "-w", p.task.capif).CombinedOutput(); err != nil {
		p.err(nil, "%s: %v", out, err)
	}
}

func (p *project) initPatches() {
	var tlds []*tld
	for _, tld := range p.tlds {
		if len(tld.patches) != 0 {
			tlds = append(tlds, tld)
		}
	}
	if len(tlds) == 0 {
		return
	}

	sort.Slice(tlds, func(i, j int) bool { return tlds[i].name < tlds[j].name })
	p.w("\nfunc init() {")
	for _, tld := range tlds {
		for _, patch := range tld.patches {
			var fld string
			if patch.fld != nil {
				fld = fmt.Sprintf("/* .%s */", patch.fld.Name())
			}
			init := patch.init
			expr := init.AssignmentExpression
			d := expr.Declarator()
			switch {
			case d != nil && d.Type().Kind() == cc.Function:
				p.w("\n*(*")
				p.functionSignature(nil, d.Type(), "")
				p.w(")(unsafe.Pointer(uintptr(unsafe.Pointer(&%s))+%d%s)) = ", tld.name, init.Offset, fld)
				p.declarator(init, nil, d, d.Type(), exprFunc, 0)
			default:
				p.w("\n*(*%s)(unsafe.Pointer(uintptr(unsafe.Pointer(&%s))+%d%s)) = ", p.typ(init, patch.t), tld.name, init.Offset, fld)
				p.assignmentExpression(nil, expr, patch.t, exprValue, fOutermost)
			}
			p.w("// %s:", pos(init))
		}
	}
	p.w("\n}\n")
}

func (p *project) Err() error {
	if len(p.errors) == 0 {
		return nil
	}

	var lpos token.Position
	w := 0
	for _, v := range p.errors {
		if lpos.Filename != "" {
			if v.Pos.Filename == lpos.Filename && v.Pos.Line == lpos.Line && !strings.HasPrefix(v.Msg, tooManyErrors) {
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
		if a.Msg == tooManyErrors {
			return false
		}

		b := p.errors[j]
		if b.Msg == tooManyErrors {
			return true
		}

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
		//TODO add cmd line option for this
		//TODO s := strings.TrimSpace(hex.Dump(b))
		//TODO a := strings.Split(s, "\n")
		//TODO p.w("//  %s\n", strings.Join(a, "\n//  "))
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
				switch {
				case f == nil:
					p.structs[tag].emit(p, n.DeclarationSpecifiers)
				default:
					p.localTaggedStructs = append(p.localTaggedStructs, func() {
						p.structs[tag].emit(p, n.DeclarationSpecifiers)
					})
				}
			}
		}
		return true
	})

	if n.InitDeclaratorList == nil {
		return
	}

	// DeclarationSpecifiers InitDeclaratorList ';'
	sep := tidyComment("\n", n)
	for list := n.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
		p.initDeclarator(f, list.InitDeclarator, sep, topDecl)
		sep = "\n"
	}
}

func (p *project) initDeclarator(f *function, n *cc.InitDeclarator, sep string, topDecl bool) {
	if f == nil {
		p.tld(f, n, sep, false)
		return
	}

	d := n.Declarator
	if d.IsExtern() || d.IsTypedefName {
		return
	}

	if tld := p.tlds[d]; tld != nil && !topDecl { // static local
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
		if block.noDecl || block.topDecl && !topDecl {
			return
		}

		p.initDeclaratorDecl(f, n, sep)
	case cc.InitDeclaratorInit: // Declarator AttributeSpecifierList '=' Initializer
		if f.block.topDecl {
			switch {
			case topDecl:
				p.initDeclaratorDecl(f, n, sep)
				if local.forceRead && !local.isPinned {
					p.w("_ = %s;", local.name)
				}
			default:
				sv := f.condInitPrefix
				f.condInitPrefix = func() {
					p.declarator(d, f, d, d.Type(), exprLValue, fOutermost)
					p.w(" = ")
				}
				switch {
				case p.isConditionalInitializer(n.Initializer):
					p.assignmentExpression(f, n.Initializer.AssignmentExpression, d.Type(), exprCondInit, 0)
				default:
					f.condInitPrefix()
					p.initializer(f, n.Initializer, d.Type(), nil, nil)
				}
				f.condInitPrefix = sv
				p.w(";")
			}
			return
		}

		p.w("%s", sep)
		switch {
		case local.isPinned:
			sv := f.condInitPrefix
			f.condInitPrefix = func() {
				p.declarator(d, f, d, d.Type(), exprLValue, fOutermost)
				p.w(" = ")
			}
			switch {
			case p.isConditionalInitializer(n.Initializer):
				p.assignmentExpression(f, n.Initializer.AssignmentExpression, d.Type(), exprCondInit, 0)
			default:
				f.condInitPrefix()
				p.initializer(f, n.Initializer, d.Type(), nil, nil)
				p.w(";")
			}
			f.condInitPrefix = sv
			p.w(";")
		default:
			var semi string
			switch {
			case block.noDecl:
				semi = ""
			default:
				p.w("var %s ", local.name)
				if !isAggregateType(d.Type()) {
					p.w("%s ", p.typ(n, d.Type()))
				}
				semi = ";"
			}
			switch {
			case p.isConditionalInitializer(n.Initializer):
				p.w("%s", semi)
				sv := f.condInitPrefix
				f.condInitPrefix = func() { p.w("%s = ", local.name) }
				p.assignmentExpression(f, n.Initializer.AssignmentExpression, d.Type(), exprCondInit, 0)
				f.condInitPrefix = sv
			default:
				if block.noDecl {
					p.w("%s", local.name)
				}
				p.w(" = ")
				p.initializer(f, n.Initializer, d.Type(), nil, nil)
			}
			p.w(";")
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if !block.noDecl && local.forceRead && !local.isPinned {
		p.w("_ = %s;", local.name)
	}
}

func (p *project) isConditionalInitializer(n *cc.Initializer) bool {
	return n.Case == cc.InitializerExpr && p.isConditionalAssignmentExpr(n.AssignmentExpression)
}

func (p *project) isConditionalAssignmentExpr(n *cc.AssignmentExpression) bool {
	return n.Case == cc.AssignmentExpressionCond &&
		n.ConditionalExpression.Case == cc.ConditionalExpressionCond
}

//TODO- func typedef(n cc.Node) (r cc.StringID) {
//TODO- 	cc.Inspect(n, func(n cc.Node, entry bool) bool {
//TODO- 		if !entry {
//TODO- 			return true
//TODO- 		}
//TODO-
//TODO- 		if x, ok := n.(*cc.TypeSpecifier); ok && x.Case == cc.TypeSpecifierTypedefName {
//TODO- 			r = x.Token.Value
//TODO- 			return false
//TODO- 		}
//TODO-
//TODO- 		return true
//TODO- 	})
//TODO- 	return r
//TODO- }

func (p *project) initDeclaratorDecl(f *function, n *cc.InitDeclarator, sep string) {
	d := n.Declarator
	local := f.locals[d]
	if strings.TrimSpace(sep) == "" {
		sep = "\n"
	}
	if local.isPinned {
		p.w("%s// var %s %s at %s%s, %d\n", sep, local.name, p.typ(n, d.Type()), f.bpName, nonZeroUintptr(local.off), d.Type().Size())
		return
	}

	p.w("%svar %s %s;", sep, local.name, p.typ(n, d.Type()))
}

func (p *project) declarator(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprLValue:
		p.declaratorLValue(n, f, d, t, mode, flags)
	case exprFunc:
		p.declaratorFunc(n, f, d, t, mode, flags)
	case exprValue:
		p.declaratorValue(n, f, d, t, mode, flags)
	case exprAddrOf:
		p.declaratorAddrOf(n, f, d, t, flags)
	case exprSelect:
		p.declaratorSelect(n, f, d)
	case exprPSelect:
		p.declaratorPSelect(n, f, d, t)
	default:
		panic(todo("", mode))
	}
}

func (p *project) declaratorValue(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	switch k := p.declaratorKind(d); k {
	case opNormal:
		p.declaratorValueNormal(n, f, d, t, mode, flags)
	case opArray:
		p.declaratorValueArray(n, f, d, t, mode, flags)
	case opFunction:
		p.declarator(n, f, d, t, exprAddrOf, flags)
	case opUnion:
		p.declaratorValueUnion(n, f, d, t, mode, flags)
	case opArrayParameter:
		p.declaratorValueArrayParameter(n, f, d, t, mode, flags)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorValueArrayParameter(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(n, d.Type(), t, flags))
	}
	local := f.locals[d]
	if local.isPinned {
		p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(n, paramTypeDecay(d)), f.bpName, nonZeroUintptr(local.off), local.name)
		return
	}

	p.w("%s", local.name)
}

func (p *project) declaratorValueUnion(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(n, d.Type(), t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d, d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorValueArray(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	if t.IsIntegerType() {
		defer p.w("%s", p.convertType(n, nil, t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("%s%s/* %s */", f.bpName, nonZeroUintptr(local.off), local.name)
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
		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorValueNormal(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(n, d.Type(), t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d, d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorFunc(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	switch k := p.declaratorKind(d); k {
	case opFunction:
		p.declaratorFuncFunc(n, f, d, t, exprValue, flags)
	case opNormal:
		p.declaratorFuncNormal(n, f, d, t, exprValue, flags)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorFuncNormal(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	u := d.Type()
	switch u.Kind() {
	case cc.Ptr:
		u = u.Elem()
		if u.Kind() == cc.Function {
			if local := f.locals[d]; local != nil {
				if local.isPinned {
					p.w("(*(*")
					p.functionSignature(f, u, "")
					p.w(")(unsafe.Pointer(%s%s)))", f.bpName, nonZeroUintptr(local.off))
					return
				}

				if d.IsParameter {
					p.w("(*(*")
					p.functionSignature(f, u, "")
					p.w(")(unsafe.Pointer(&%s)))", local.name)
					return
				}

				panic(todo("", pos(d)))
			}

			tld := p.tlds[d]
			if tld == nil {
				tld = p.externs[d.Name()]
			}
			switch {
			case tld != nil:
				p.w("(*(*")
				p.functionSignature(f, u, "")
				p.w(")(unsafe.Pointer(&%s)))", tld.name)
				return
			default:
				nm := d.Name()
				imp := p.imports[nm.String()]
				if imp == nil {
					p.err(n, "undefined: %s", d.Name())
					p.w("%s", nm)
					return
				}

				imp.used = true
				panic(todo(""))
			}
		}

		panic(todo("", pos(d), u))
	default:
		panic(todo("", pos(d), u))
	}
}

func (p *project) declaratorFuncFunc(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	switch d.Type().Kind() {
	case cc.Function:
		// ok
	default:
		panic(todo("", pos(d), d.Type(), d.Type().Kind()))
	}

	if f != nil {
		if local := f.locals[d]; local != nil {
			panic(todo("", n.Position(), pos(d)))
		}
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorLValue(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	switch k := p.declaratorKind(d); k {
	case opNormal, opArrayParameter, opUnion:
		p.declaratorLValueNormal(n, f, d, t, mode, flags)
	case opArray:
		p.declaratorLValueArray(n, f, d, t, mode, flags)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorLValueArray(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d, d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		panic(todo(""))
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorLValueNormal(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(n, d.Type(), t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d, d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
		p.w("%s", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorKind(d *cc.Declarator) opKind {
	switch {
	case p.isArrayParameterDeclarator(d):
		return opArrayParameter
	case p.isArrayDeclarator(d):
		return opArray
	case d.Type().Kind() == cc.Function:
		return opFunction
	case d.Type().Kind() == cc.Union:
		return opUnion
	default:
		return opNormal
	}
}

func (p *project) declaratorPSelect(n cc.Node, f *function, d *cc.Declarator, t cc.Type) {
	switch k := p.declaratorKind(d); k {
	case opNormal:
		p.declaratorPSelectNormal(n, f, d, t)
	case opArray:
		panic(todo(""))
		p.declaratorPSelectArray(n, f, d)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorPSelectArray(n cc.Node, f *function, d *cc.Declarator) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(d, d.Type().Elem()), f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			panic(todo(""))
			return
		}
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		panic(todo(""))
	default:
		panic(todo(""))
	}
}

func (p *project) declaratorPSelectNormal(n cc.Node, f *function, d *cc.Declarator, t cc.Type) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("(*%s)(unsafe.Pointer(*(*unsafe.Pointer)(unsafe.Pointer(%s%s/* &%s */))))", p.typ(d, d.Type().Elem()), f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			if t == nil {
				t = d.Type()
			}
			p.w("(*%s)(unsafe.Pointer(%s))", p.typ(d, t.Elem()), local.name)
			return
		}
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		p.w("(*%s)(unsafe.Pointer(%s))", p.typ(d, d.Type().Elem()), tld.name)
	default:
		panic(todo(""))
	}
}

func (p *project) declaratorSelect(n cc.Node, f *function, d *cc.Declarator) {
	switch k := p.declaratorKind(d); k {
	case opNormal:
		p.declaratorSelectNormal(n, f, d)
	case opArray:
		p.declaratorSelectArray(n, f, d)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorSelectArray(n cc.Node, f *function, d *cc.Declarator) {
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			panic(todo(""))
			p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(d, d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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

func (p *project) declaratorSelectNormal(n cc.Node, f *function, d *cc.Declarator) {
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(d, d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorAddrOf(n cc.Node, f *function, d *cc.Declarator, t cc.Type, flags flags) {
	switch k := p.declaratorKind(d); k {
	case opArray:
		p.declaratorAddrOfArray(n, f, d)
	case opNormal:
		p.declaratorAddrOfNormal(n, f, d, flags)
	case opUnion:
		p.declaratorAddrOfUnion(n, f, d)
	case opFunction:
		p.declaratorAddrOfFunction(n, f, d)
	case opArrayParameter:
		p.declaratorAddrOfArrayParameter(n, f, d)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorAddrOfArrayParameter(n cc.Node, f *function, d *cc.Declarator) {
	p.w("%s", f.locals[d].name)
}

func (p *project) declaratorAddrOfFunction(n cc.Node, f *function, d *cc.Declarator) {
	if d.Type().Kind() != cc.Function {
		panic(todo(""))
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		p.w("*(*uintptr)(unsafe.Pointer(&struct{f ")
		p.functionSignature(f, d.Type(), "")
		p.w("}{%s}))", tld.name)
	default:
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		p.w("*(*uintptr)(unsafe.Pointer(&struct{f ")
		p.functionSignature(f, d.Type(), "")
		p.w("}{%sX%s}))", imp.qualifier, nm)
	}
}

func (p *project) declaratorAddrOfUnion(n cc.Node, f *function, d *cc.Declarator) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			panic(todo(""))
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
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		panic(todo("", pos(d), d.Name()))
	}
}

func (p *project) declaratorAddrOfNormal(n cc.Node, f *function, d *cc.Declarator, flags flags) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			if flags&fAddrOfFuncPtrOk != 0 {
				if d.Type().Kind() == cc.Ptr && d.Type().Elem().Kind() == cc.Function {
					p.w("&%s", local.name)
					return
				}
			}

			panic(todo("", pos(d), d.Name(), d.Type(), d.IsParameter, d.AddressTaken))
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
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		p.w("uintptr(unsafe.Pointer(&%sX%s))", imp.qualifier, nm)
	}
}

func (p *project) declaratorAddrOfArray(n cc.Node, f *function, d *cc.Declarator) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			panic(todo("", pos(d), d.Name(), d.Type(), d.IsParameter))
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
		nm := d.Name()
		imp := p.imports[nm.String()]
		if imp == nil {
			p.err(n, "undefined: %s", d.Name())
			p.w("%s", nm)
			return
		}

		imp.used = true
		panic(todo("", pos(d), d.Name()))
	}
}

func (p *project) convertType(n cc.Node, from, to cc.Type, flags flags) string {
	// trc("%v: %v -> %v\n%s", pos(n), from, to, debug.Stack()[:600]) //TODO-
	force := flags&fForceConv != 0
	if from == nil {
		p.w("%s(", p.typ(nil, to))
		return ")"
	}

	if from.IsScalarType() {
		switch {
		case force:
			p.w("%s(", p.helperType2(n, from, to))
			return ")"
		case from.Kind() == to.Kind():
			return ""
		default:
			p.w("%s(", p.typ(n, to))
			return ")"
		}
	}

	switch from.Kind() {
	case cc.Function, cc.Struct, cc.Union, cc.Array, cc.Ptr:
		if from.Kind() == to.Kind() {
			return ""
		}

		panic(todo("", from, to))
	case cc.Double, cc.Float:
		p.w("%s(", p.typ(n, to))
		return ")"
	}

	panic(todo("", from, to))
}

func (p *project) convert(n cc.Node, op cc.Operand, to cc.Type, flags flags) string {
	if flags&fForceRuntimeConv != 0 {
		flags |= fForceConv
	}
	if op == nil {
		p.w("%s(", p.typ(n, to))
		return ")"
	}

	force := flags&fForceConv != 0
	from := op.Type()
	if !force && from.IsScalarType() && from.Kind() == to.Kind() {
		return ""
	}

	if from.IsIntegerType() {
		return p.convertInt(n, op, to, flags)
	}

	switch from.Kind() {
	case cc.Ptr:
		if !force && from.Kind() == to.Kind() {
			return ""
		}

		if to.IsIntegerType() {
			p.w("%s(", p.typ(n, to))
			return ")"
		}

		panic(todo("%v: %q -> %q", pos(n), from, to))
	case cc.Function, cc.Struct, cc.Union:
		if !force && from.Kind() == to.Kind() {
			return ""
		}

		panic(todo("%q -> %q", from, to))
	case cc.Double, cc.Float:
		p.w("%s(", p.typ(n, to))
		return ")"
	case cc.Array:
		if !force && from.Kind() == to.Kind() {
			return ""
		}

		switch to.Kind() {
		case cc.Ptr:
			return ""
		}

		panic(todo("%q -> %q", from, to))
	}

	panic(todo("%q -> %q", from, to))
}

func (p *project) convertInt(n cc.Node, op cc.Operand, to cc.Type, flags flags) string {
	force := flags&fForceConv != 0
	value := op.Value()
	if value == nil || !to.IsIntegerType() {
		if to.IsScalarType() {
			p.w("%s(", p.typ(nil, to))
			return ")"
		}

		panic(todo("", op.Type(), to))
	}

	if flags&fForceRuntimeConv != 0 {
		p.w("%s(", p.helperType2(n, op.Type(), to))
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
						p.w("%s(", p.typ(nil, to))
						return ")"
					}

					p.w("%sUint8FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 2:
					if x >= 0 && x <= math.MaxUint16 {
						p.w("%s(", p.typ(nil, to))
						return ")"
					}

					p.w("%sUint16FromInt%d(", p.task.crt, from.Size()*8)
					return ")"
				case 4:
					if x >= 0 && x <= math.MaxUint32 {
						p.w("%s(", p.typ(nil, to))
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
			case cc.Uint64Value:
				switch to.Size() {
				case 1:
					if x <= math.MaxUint8 {
						p.w("%s(", p.typ(nil, to))
						return ")"
					}

					p.w("%sUint8FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 2:
					if x <= math.MaxUint16 {
						p.w("%s(", p.typ(nil, to))
						return ")"
					}

					p.w("%sUint16FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 4:
					if x <= math.MaxUint32 {
						p.w("%s(", p.typ(nil, to))
						return ")"
					}

					p.w("%sUint32FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 8:
					p.w("uint64(")
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
					if x <= math.MaxUint8 {
						switch {
						case !force && from.Size() == 1:
							return ""
						default:
							p.w("uint8(")
							return ")"
						}
					}

					p.w("%sUint8FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 2:
					if x <= math.MaxUint16 {
						switch {
						case !force && from.Size() == 2:
							return ""
						default:
							p.w("uint16(")
							return ")"
						}
					}

					p.w("%sUint16FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 4:
					if x <= math.MaxUint32 {
						switch {
						case !force && from.Size() == 4:
							return ""
						default:
							p.w("uint32(")
							return ")"
						}
					}

					p.w("%sUint32FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
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

func (p *project) tld(f *function, n *cc.InitDeclarator, sep string, staticLocal bool) {
	d := n.Declarator
	if _, ok := p.wanted[d]; !ok && !staticLocal {
		return
	}

	tld := p.tlds[d]
	if tld == nil { // Dead declaration.
		return
	}

	t := d.Type()
	if d.IsTypedefName {
		if _, ok := p.typedefsEmited[tld.name]; ok {
			return
		}

		p.typedefsEmited[tld.name] = struct{}{}
		if t.Kind() != cc.Void {
			p.w("%stype %s = %s; /* %v */", sep, tld.name, p.typ(n, t), pos(n))
		}
		return
	}

	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		p.w("%svar %s %s\t/* %v: */", sep, tld.name, p.typ(n, t), pos(n))
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
			p.w("%s ", p.typ(n, d.Type()))
		}
		p.w("= ")
		p.initializer(f, n.Initializer, d.Type(), tld, nil)
		p.w("; /* %v */", pos(d))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) functionDefinition(n *cc.FunctionDefinition) {
	// DeclarationSpecifiers Declarator DeclarationList CompoundStatement
	d := n.Declarator
	if d.Linkage == cc.Internal && d.Read == 0 && !d.AddressTaken && strings.HasPrefix(d.Name().String(), "__") {
		return
	}

	tld := p.tlds[d]
	if tld == nil {
		return
	}

	f := newFunction(p, n)
	p.functionDefinitionSignature(f, tld)
	p.w(" ")
	comment := fmt.Sprintf("/* %v: */", pos(d))
	if need := f.off; need != 0 {
		scope := f.blocks[n.CompoundStatement].scope
		f.bpName = scope.take("bp")
		p.w("{%s\n%s := %s.Alloc(%d)\n", comment, f.bpName, f.tlsName, need)
		p.w("defer %s.Free(%d)\n", f.tlsName, need)
		for _, v := range d.Type().Parameters() {
			if local := f.locals[v.Declarator()]; local != nil && local.isPinned { // Pin it.
				p.w("*(*%s)(unsafe.Pointer(%s%s)) = %s\n", p.typ(v.Declarator(), paramTypeDecay(v.Declarator())), f.bpName, nonZeroUintptr(local.off), local.name)
			}
		}
		comment = ""
	}
	p.compoundStatement(f, n.CompoundStatement, comment, false)
	p.w(";")
	p.flushLocalTaggesStructs()
	p.flushStaticTLDs()
}

func (p *project) flushLocalTaggesStructs() {
	for _, v := range p.localTaggedStructs {
		v()
	}
	p.localTaggedStructs = nil
}

func (p *project) flushStaticTLDs() {
	for _, v := range p.staticQueue {
		p.tld(nil, v, "\n", true)
	}
	p.staticQueue = nil
}

func (p *project) compoundStatement(f *function, n *cc.CompoundStatement, scomment string, forceNoBraces bool) {
	// '{' BlockItemList '}'
	brace := (!n.IsJumpTarget() || n.Parent() == nil) && !forceNoBraces
	if brace && (n.Parent() != nil || f.off == 0) {
		p.w("{%s", scomment)
	}
	sv := f.block
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
		p.zeroValue(f.rt)
	}
	if brace {
		p.w("%s}", tidyComment("\n", &n.Token2))
	}
	f.block = sv
}

func (p *project) zeroValue(t cc.Type) {
	if t.IsScalarType() {
		p.w("%s(0)", p.typ(nil, t))
		return
	}

	switch t.Kind() {
	case cc.Struct, cc.Union:
		p.w("%s{}", p.typ(nil, t))
	default:
		panic(todo("", t, t.Kind()))
	}
}

func (p *project) blockItem(f *function, n *cc.BlockItem) (r *cc.JumpStatement) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		p.declaration(f, n.Declaration, false)
	case cc.BlockItemStmt: // Statement
		r = p.statement(f, n.Statement, false, false)
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

func (p *project) statement(f *function, n *cc.Statement, forceCompoundStmtBrace, forceNoBraces bool) (r *cc.JumpStatement) {
	if forceCompoundStmtBrace {
		p.w(" {")
	}
	switch n.Case {
	case cc.StatementLabeled: // LabeledStatement
		r = p.labeledStatement(f, n.LabeledStatement)
	case cc.StatementCompound: // CompoundStatement
		if !forceCompoundStmtBrace {
			p.w("%s", n.CompoundStatement.Token.Sep)
		}
		if f.hasJumps {
			forceNoBraces = true
		}
		p.compoundStatement(f, n.CompoundStatement, "", forceCompoundStmtBrace || forceNoBraces)
	case cc.StatementExpr: // ExpressionStatement
		p.expressionStatement(f, n.ExpressionStatement)
	case cc.StatementSelection: // SelectionStatement
		p.selectionStatement(f, n.SelectionStatement)
	case cc.StatementIteration: // IterationStatement
		p.iterationStatement(f, n.IterationStatement)
	case cc.StatementJump: // JumpStatement
		r = p.jumpStatement(f, n.JumpStatement)
	case cc.StatementAsm: // AsmStatement
		// AsmStatement:
		//         Asm AttributeSpecifierList ';'
		// Asm:
		//         "__asm__" AsmQualifierList '(' STRINGLITERAL AsmArgList ')'
		if n.AsmStatement.Asm.Token3.Value == 0 && n.AsmStatement.Asm.AsmArgList == nil {
			break
		}

		p.err(n, "assembler statements not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	if forceCompoundStmtBrace {
		p.w("}")
	}
	return r
}

func (p *project) jumpStatement(f *function, n *cc.JumpStatement) (r *cc.JumpStatement) {
	p.w("%s", tidyComment("\n", n))
	if _, ok := n.Context().(*cc.SelectionStatement); ok && f.ifCtx == nil {
		switch f.switchCtx {
		case inSwitchCase:
			f.switchCtx = inSwitchSeenBreak
		case inSwitchSeenBreak:
			// nop but TODO
		case inSwitchFlat:
			// ok
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
		switch {
		case f.continueCtx != 0:
			p.w("goto __%d", f.continueCtx)
		default:
			p.w("continue")
		}
	case cc.JumpStatementBreak: // "break" ';'
		switch {
		case f.breakCtx != 0:
			p.w("goto __%d", f.breakCtx)
		default:
			p.w("break")
		}
	case cc.JumpStatementReturn: // "return" Expression ';'
		r = n
		switch {
		case f.rt != nil && f.rt.Kind() == cc.Void:
			if n.Expression != nil {
				p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, fOutermost)
				p.w(";")
			}
			p.w("return")
		case f.rt != nil && f.rt.Kind() != cc.Void:
			if n.Expression != nil {
				p.expression(f, n.Expression, f.rt, exprCondReturn, fOutermost)
				break
			}

			p.w("return ")
			p.zeroValue(f.rt)
		default:
			if n.Expression != nil {
				p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, fOutermost)
				p.w(";")
			}
			p.w("return")
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) expression(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoid:
		p.expressionVoid(f, n, t, mode, flags)
	case exprValue, exprCondReturn, exprCondInit:
		p.expressionValue(f, n, t, mode, flags)
	case exprBool:
		p.expressionBool(f, n, t, mode, flags)
	case exprAddrOf:
		p.expressionAddrOf(f, n, t, mode, flags)
	case exprPSelect:
		p.expressionPSelect(f, n, t, mode, flags)
	case exprLValue:
		p.expressionLValue(f, n, t, mode, flags)
	case exprFunc:
		p.expressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.expressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) expressionSelect(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionFunc(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionLValue(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionPSelect(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionAddrOf(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionBool(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		defer p.w("%s", p.convertType(n, n.Operand.Type(), t, flags))
		p.w("func() bool {")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, flags)
		p.w("; return ")
		p.assignmentExpression(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type(), mode, flags)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionValue(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		defer p.w("%s", p.convertType(n, n.Operand.Type(), t, flags))
		p.w("func() %v {", p.typ(n, n.AssignmentExpression.Operand.Type()))
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, flags)
		p.w("; return ")
		p.assignmentExpression(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type(), exprValue, flags)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionVoid(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		p.expression(f, n.Expression, t, mode, flags)
		p.w(";")
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) opKind(f *function, d declarator, t cc.Type) opKind {
	switch {
	case p.isArrayParameter(d, t):
		return opArrayParameter
	case p.isArray(f, d, t):
		return opArray
	case t.Kind() == cc.Union:
		return opUnion
	case t.Kind() == cc.Struct:
		return opStruct
	case t.IsBitFieldType():
		return opBitfield
	case t.Kind() == cc.Function:
		return opFunction
	default:
		return opNormal
	}
}

func (p *project) assignmentExpression(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoid:
		p.assignmentExpressionVoid(f, n, t, mode, flags)
	case exprValue, exprCondReturn, exprCondInit:
		p.assignmentExpressionValue(f, n, t, mode, flags)
	case exprAddrOf:
		p.assignmentExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.assignmentExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.assignmentExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.assignmentExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.assignmentExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.assignmentExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) assignmentExpressionSelect(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionFunc(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionLsh: // UnaryExpremode, ssion "<<=
		panic(todo(""))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionPSelect(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, n.AssignmentExpression.Operand.Type().Elem()))
		p.assignmentExpression(f, n, t, exprValue, flags)
		p.w("))")
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionLValue(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionBool(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.assignmentExpression(f, n, t, exprValue, flags|fOutermost)
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionAddrOf(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionValue(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		p.assignmentExpressionValueAssign(f, n, t, mode, flags)
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		p.assignOp(f, n, t, mode, "*", "Mul", flags)
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		p.assignOp(f, n, t, mode, "/", "Div", flags)
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		p.assignOp(f, n, t, mode, "%", "Rem", flags)
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		p.assignOp(f, n, t, mode, "+", "Add", flags)
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		p.assignOp(f, n, t, mode, "-", "Sub", flags)
	case cc.AssignmentExpressionLsh: // UnaryExpremode, ssion "<<=
		p.assignOp(f, n, t, mode, "<<", "Shl", flags)
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		p.assignOp(f, n, t, mode, ">>", "Shr", flags)
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		p.assignOp(f, n, t, mode, "&", "And", flags)
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		p.assignOp(f, n, t, mode, "^", "Xor", flags)
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		p.assignOp(f, n, t, mode, "|", "Or", flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionValueAssign(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	// UnaryExpression '=' AssignmentExpression
	if mode == exprCondReturn {
		p.w("return ")
	}
	lhs := n.UnaryExpression
	switch k := p.opKind(f, lhs, lhs.Operand.Type()); k {
	case opNormal:
		p.assignmentExpressionValueAssignNormal(f, n, t, mode, flags)
	case opBitfield:
		p.assignmentExpressionValueAssignBitfield(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignmentExpressionValueAssignBitfield(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	if d := n.UnaryExpression.Declarator(); d != nil {
		panic(todo(""))
	}

	lhs := n.UnaryExpression
	lt := lhs.Operand.Type()
	bf := lt.BitField()
	defer p.w("%s", p.convertType(n, lt, t, flags))
	switch {
	case bf.Type().IsSignedType():
		p.w("%sAssignBitFieldPtr%d%s(", p.task.crt, bf.BitFieldBlockWidth(), p.bfHelperType(lt))
		p.unaryExpression(f, lhs, lt, exprAddrOf, flags)
		p.w(", ")
		p.assignmentExpression(f, n.AssignmentExpression, lt, exprValue, flags|fOutermost)
		p.w(", %d, %#x)", bf.BitFieldOffset(), bf.Mask())
	default:
		p.w("%sAssignBitFieldPtr%d%s(", p.task.crt, bf.BitFieldBlockWidth(), p.bfHelperType(lt))
		p.unaryExpression(f, lhs, lt, exprAddrOf, flags)
		p.w(", ")
		p.assignmentExpression(f, n.AssignmentExpression, lt, exprValue, flags|fOutermost)
		p.w(", %d, %#x)", bf.BitFieldOffset(), bf.Mask())
	}
}

func (p *project) assignmentExpressionValueAssignNormal(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	if d := n.UnaryExpression.Declarator(); d != nil {
		if !d.Type().IsScalarType() {
			panic(todo(""))
		}

		if local := f.locals[d]; local != nil {
			if local.isPinned {
				defer p.w(")%s", p.convertType(n, d.Type(), t, flags))
				p.w("%sAssignPtr%s(", p.task.crt, p.helperType(d, d.Type()))
				p.w("%s%s /* %s */", f.bpName, nonZeroUintptr(local.off), local.name)
				p.w(", ")
				p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), exprValue, flags|fOutermost)
				return
			}

			defer p.w(")%s", p.convertType(n, d.Type(), t, flags))
			p.w("%sAssign%s(&%s, ", p.task.crt, p.helperType(n, d.Type()), local.name)
			p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), exprValue, flags|fOutermost)
			return
		}

		panic(todo("", pos(n)))
	}

	defer p.w(")%s", p.convertType(n, n.UnaryExpression.Operand.Type(), t, flags))
	p.w("%sAssignPtr%s(", p.task.crt, p.helperType(n, n.UnaryExpression.Operand.Type()))
	p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), exprAddrOf, flags)
	p.w(", ")
	p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), exprValue, flags|fOutermost)
}

func (p *project) assignmentExpressionVoid(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		lhs := n.UnaryExpression
		lt := lhs.Operand.Type()
		sv := f.condInitPrefix
		switch k := p.opKind(f, lhs, lt); k {
		case opArrayParameter:
			lt = lt.Decay()
			fallthrough
		case opNormal, opStruct:
			mode = exprValue
			if p.isArrayOrPinnedArray(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type()) {
				mode = exprAddrOf
			}
			switch {
			case flags&fNoCondAssignment == 0 && mode == exprValue && n.UnaryExpression.Declarator() != nil && p.isConditionalAssignmentExpr(n.AssignmentExpression):
				f.condInitPrefix = func() {
					p.unaryExpression(f, lhs, lt, exprLValue, flags)
					p.w(" = ")
				}
				p.assignmentExpression(f, n.AssignmentExpression, lt, exprCondInit, flags|fOutermost)
				p.w(";")
			default:
				p.unaryExpression(f, lhs, lt, exprLValue, flags)
				p.w(" = ")
				p.assignmentExpression(f, n.AssignmentExpression, lt, mode, flags|fOutermost)
			}
		case opBitfield:
			bf := lt.BitField()
			p.w("%sSetBitFieldPtr%d%s(", p.task.crt, bf.BitFieldBlockWidth(), p.bfHelperType(lt))
			p.unaryExpression(f, lhs, lt, exprAddrOf, flags)
			p.w(", ")
			p.assignmentExpression(f, n.AssignmentExpression, lt, exprValue, flags|fOutermost)
			p.w(", %d, %#x)", bf.BitFieldOffset(), bf.Mask())
		case opUnion:
			p.unaryExpression(f, lhs, lt, exprLValue, flags)
			p.w(" = ")
			p.assignmentExpression(f, n.AssignmentExpression, lt, exprValue, flags|fOutermost)
		default:
			panic(todo("", n.Position(), k))
		}
		f.condInitPrefix = sv
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		p.assignOp(f, n, t, mode, "*", "Mul", flags)
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		p.assignOp(f, n, t, mode, "/", "Div", flags)
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		p.assignOp(f, n, t, mode, "%", "Mod", flags)
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		p.assignOp(f, n, t, mode, "+", "Add", flags)
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		p.assignOp(f, n, t, mode, "-", "Sub", flags)
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		p.assignShiftOp(f, n, t, mode, "<<", "Shl", flags)
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		p.assignShiftOp(f, n, t, mode, ">>", "Shr", flags)
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		p.assignOp(f, n, t, mode, "&", "And", flags)
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		p.assignOp(f, n, t, mode, "^", "Xor", flags)
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		p.assignOp(f, n, t, mode, "|", "Or", flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func isRealType(op cc.Operand) bool {
	switch op.Type().Kind() {
	case cc.Float, cc.Double:
		return true
	default:
		return false
	}
}

func (p *project) bfHelperType(t cc.Type) string {
	switch {
	case t.IsSignedType():
		return fmt.Sprintf("Int%d", t.Size()*8)
	default:
		return fmt.Sprintf("Uint%d", t.Size()*8)
	}
}

func (p *project) helperType(n cc.Node, t cc.Type) string {
	for t.IsAliasType() {
		t = t.Alias()
	}
	s := p.typ(n, t)
	return strings.ToUpper(s[:1]) + s[1:]
}

func (p *project) helperType2(n cc.Node, from, to cc.Type) string {
	if from.Kind() == to.Kind() {
		return fmt.Sprintf("%s%s", p.task.crt, p.helperType(n, from))
	}

	return fmt.Sprintf("%s%sFrom%s", p.task.crt, p.helperType(n, to), p.helperType(n, from))
}

func (p *project) conditionalExpression(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.conditionalExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.conditionalExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.conditionalExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.conditionalExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.conditionalExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.conditionalExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.conditionalExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.conditionalExpressionSelect(f, n, t, mode, flags)
	case exprCondReturn:
		p.conditionalExpressionReturn(f, n, t, mode, flags)
	case exprCondInit:
		p.conditionalExpressionInit(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) conditionalExpressionInit(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		f.condInitPrefix()
		p.logicalOrExpression(f, n.LogicalOrExpression, t, exprValue, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		p.w("if ")
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags|fOutermost)
		p.w(" {")
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
		p.w("} else { ")
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags|fOutermost)
		p.w("}")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionReturn(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.w("return ")
		p.logicalOrExpression(f, n.LogicalOrExpression, t, exprValue, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		p.w("if ")
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags|fOutermost)
		p.w(" {")
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
		p.w("}; ")
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags|fOutermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionSelect(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionFunc(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionPSelect(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		p.conditionalExpression(f, n, t, exprValue, flags)
		p.w("))")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionLValue(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionBool(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.conditionalExpression(f, n, t, exprValue, flags|fOutermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionAddrOf(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		p.w(" func() %s { if ", p.typ(n, t))
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags|fOutermost)
		p.w(" { return ")
		p.expression(f, n.Expression, t, exprValue, flags|fOutermost)
		p.w("}; return ")
		p.conditionalExpression(f, n.ConditionalExpression, t, exprValue, flags|fOutermost)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionVoid(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		switch {
		case n.Expression.IsSideEffectsFree:
			p.w("if !(")
			p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags|fOutermost)
			p.w(") {")
			p.conditionalExpression(f, n.ConditionalExpression, n.ConditionalExpression.Operand.Type(), mode, flags|fOutermost)
			p.w("}")
		default:
			p.w("if ")
			p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags|fOutermost)
			p.w(" {")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), mode, flags|fOutermost)
			p.w("} else {")
			p.conditionalExpression(f, n.ConditionalExpression, n.ConditionalExpression.Operand.Type(), mode, flags|fOutermost)
			p.w("}")
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionValue(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, exprValue, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		p.w(" func() %s { if ", p.typ(n, t))
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags|fOutermost)
		p.w(" { return ")
		p.expression(f, n.Expression, t, exprValue, flags|fOutermost)
		p.w("}; return ")
		p.conditionalExpression(f, n.ConditionalExpression, t, exprValue, flags|fOutermost)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpression(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.logicalOrExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.logicalOrExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.logicalOrExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.logicalOrExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.logicalOrExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.logicalOrExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.logicalOrExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.logicalOrExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) logicalOrExpressionSelect(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionFunc(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionPSelect(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionLValue(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionBool(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		p.binaryLogicalOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionAddrOf(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionVoid(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionValue(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		p.binaryLogicalOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryLogicalOrExpression(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.binaryLogicalOrExpressionValue(f, n, t, mode, flags)
	case exprBool:
		p.binaryLogicalOrExpressionBool(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryLogicalOrExpressionBool(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n, n.Operand, n.Operand.Type(), &mode, flags))
	p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags)
	p.w(" ||%s", tidyComment(" ", &n.Token))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), exprBool, flags)
}

func (p *project) binaryLogicalOrExpressionValue(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n, n.Operand, t, &mode, flags))
	p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags)
	p.w(" ||%s", tidyComment(" ", &n.Token))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), exprBool, flags)
}

func (p *project) booleanBinaryExpression(n cc.Node, from cc.Operand, to cc.Type, mode *exprMode, flags flags) (r string) {
	if flags&fOutermost == 0 {
		p.w("(")
		r = ")"
	}
	switch *mode {
	case exprBool:
		*mode = exprValue
	default:
		r = p.convert(n, from, to, flags) + r
		p.w("%sBool32(", p.task.crt)
		r = ")" + r
	}
	return r
}

func (p *project) logicalAndExpression(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.logicalAndExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.logicalAndExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.logicalAndExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.logicalAndExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.logicalAndExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.logicalAndExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.logicalAndExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.logicalAndExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) logicalAndExpressionSelect(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionFunc(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionPSelect(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionLValue(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionBool(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		p.binaryLogicalAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionAddrOf(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionVoid(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		p.binaryLogicalAndExpressionValue(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionValue(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		p.binaryLogicalAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryLogicalAndExpression(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprBool:
		p.binaryLogicalAndExpressionBool(f, n, t, mode, flags)
	case exprValue:
		p.binaryLogicalAndExpressionValue(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryLogicalAndExpressionValue(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n, n.Operand, t, &mode, flags))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), exprBool, flags)
	p.w(" &&%s", tidyComment(" ", &n.Token))
	p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.InclusiveOrExpression.Operand.Type(), exprBool, flags)
}

func (p *project) binaryLogicalAndExpressionBool(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n, n.Operand, t, &mode, flags))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), exprBool, flags)
	p.w(" &&%s", tidyComment(" ", &n.Token))
	p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.InclusiveOrExpression.Operand.Type(), exprBool, flags)
}

func (p *project) inclusiveOrExpression(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.inclusiveOrExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.inclusiveOrExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.inclusiveOrExpressionAddrof(f, n, t, mode, flags)
	case exprBool:
		p.inclusiveOrExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.inclusiveOrExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.inclusiveOrExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.inclusiveOrExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.inclusiveOrExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) inclusiveOrExpressionSelect(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionFunc(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionPSelect(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionLValue(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionBool(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		p.binaryInclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionAddrof(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionVoid(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionValue(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		p.binaryInclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryInclusiveOrExpression(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprBool:
		p.binaryInclusiveOrExpressionBool(f, n, t, mode, flags)
	case exprValue:
		p.binaryInclusiveOrExpressionValue(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryInclusiveOrExpressionValue(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, t, &mode, flags))
	switch {
	case orOverflows(n.InclusiveOrExpression.Operand, n.ExclusiveOrExpression.Operand, n.Promote()):
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), exprValue, flags)
		p.w(" |%s", tidyComment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
	default:
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), exprValue, flags)
		p.w(" |%s", tidyComment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, flags)
	}
}

func (p *project) binaryInclusiveOrExpressionBool(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, t, &mode, flags))
	switch {
	case orOverflows(n.InclusiveOrExpression.Operand, n.ExclusiveOrExpression.Operand, n.Promote()):
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), exprValue, flags)
		p.w(" |%s", tidyComment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
	default:
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), exprValue, flags)
		p.w(" |%s", tidyComment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, flags)
	}
}

func orOverflows(lo, ro cc.Operand, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	return overflows(a.Or(a, b), promote)
}

func (p *project) artithmeticBinaryExpression(n cc.Node, from cc.Operand, to cc.Type, mode *exprMode, flags flags) (r string) {
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
		r = p.convert(n, from, to, flags) + r
	}
	return r
}

func (p *project) exclusiveOrExpression(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.exclusiveOrExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.exclusiveOrExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.exclusiveOrExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.exclusiveOrExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.exclusiveOrExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.exclusiveOrExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.exclusiveOrExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.exclusiveOrExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) exclusiveOrExpressionSelect(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionFunc(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionPSelect(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionLValue(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionBool(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		p.binaryExclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionAddrOf(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionVoid(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionValue(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		p.binaryExclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryExclusiveOrExpression(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue, exprBool:
		p.binaryExclusiveOrExpressionValue(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryExclusiveOrExpressionValue(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, t, &mode, flags))
	switch {
	case xorOverflows(n.ExclusiveOrExpression.Operand, n.AndExpression.Operand, n.Promote()):
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, flags)
		p.w(" ^%s", tidyComment(" ", &n.Token))
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
	default:
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), exprValue, flags)
		p.w(" ^%s", tidyComment(" ", &n.Token))
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, flags)
	}
}

func xorOverflows(lo, ro cc.Operand, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	return overflows(a.Xor(a, b), promote)
}

func (p *project) andExpression(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.andExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.andExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.andExpressionAddrof(f, n, t, mode, flags)
	case exprBool:
		p.andExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.andExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.andExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.andExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.andExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) andExpressionSelect(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionFunc(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionPSelect(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionLValue(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionBool(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		p.binaryAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionAddrof(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionVoid(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionValue(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		p.binaryAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryAndExpression(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.binaryAndExpressionValue(f, n, t, mode, flags)
	case exprBool:
		p.binaryAndExpressionBool(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryAndExpressionBool(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, n.Operand.Type(), &mode, flags))
	switch {
	case andOverflows(n.AndExpression.Operand, n.EqualityExpression.Operand, n.Promote()):
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, flags)
		p.w(" &%s", tidyComment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
	default:
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, flags)
		p.w(" &%s", tidyComment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, flags)
	}
}

func (p *project) binaryAndExpressionValue(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, t, &mode, flags))
	switch {
	case andOverflows(n.AndExpression.Operand, n.EqualityExpression.Operand, n.Promote()):
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, flags)
		p.w(" &%s", tidyComment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
	default:
		p.andExpression(f, n.AndExpression, n.Promote(), exprValue, flags)
		p.w(" &%s", tidyComment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, flags)
	}
}

func andOverflows(lo, ro cc.Operand, promote cc.Type) bool {
	a, b, ok := getIntOperands(lo, ro)
	if !ok {
		return false
	}

	return overflows(a.And(a, b), promote)
}

func (p *project) equalityExpression(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.equalityExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.equalityExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.equalityExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.equalityExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.equalityExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.equalityExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.equalityExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.equalityExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) equalityExpressionSelect(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionFunc(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionPSelect(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionLValue(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionBool(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		p.binaryEqualityExpression(f, n, " == ", t, mode, flags)
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		p.binaryEqualityExpression(f, n, " != ", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionAddrOf(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionVoid(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionValue(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		p.binaryEqualityExpression(f, n, " == ", t, mode, flags)
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		p.binaryEqualityExpression(f, n, " != ", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryEqualityExpression(f *function, n *cc.EqualityExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.binaryEqualityExpressionValue(f, n, oper, t, mode, flags)
	case exprBool:
		p.binaryEqualityExpressionBool(f, n, oper, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryEqualityExpressionBool(f *function, n *cc.EqualityExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n, n.Operand, t, &mode, flags))
	p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, flags)
	p.w(" %s%s", oper, tidyComment(" ", &n.Token))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), exprValue, flags)
}

func (p *project) binaryEqualityExpressionValue(f *function, n *cc.EqualityExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n, n.Operand, t, &mode, flags))
	p.equalityExpression(f, n.EqualityExpression, n.Promote(), exprValue, flags)
	p.w(" %s%s", oper, tidyComment(" ", &n.Token))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), exprValue, flags)
}

func (p *project) relationalExpression(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.relationalExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.relationalExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.relationalExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.relationalExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.relationalExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.relationalExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.relationalExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.relationalExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) relationalExpressionSelect(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionFunc(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionPSelect(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionLValue(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionBool(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
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

func (p *project) relationalExpressionAddrOf(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionVoid(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo(""))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionValue(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
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
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n, n.Operand, t, &mode, flags))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), exprValue, flags)
	p.w(" %s%s", oper, tidyComment(" ", &n.Token))
	p.shiftExpression(f, n.ShiftExpression, n.Promote(), exprValue, flags)
}

func (p *project) shiftExpression(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.shiftExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.shiftExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.shiftExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.shiftExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.shiftExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.shiftExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.shiftExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.shiftExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) shiftExpressionSelect(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionFunc(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionPSelect(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionLValue(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionBool(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		p.binaryShiftExpression(f, n, "<<", t, mode, flags)
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		p.binaryShiftExpression(f, n, ">>", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionAddrOf(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionVoid(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionValue(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		p.binaryShiftExpression(f, n, "<<", t, mode, flags)
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		p.binaryShiftExpression(f, n, ">>", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryShiftExpression(f *function, n *cc.ShiftExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.binaryShiftExpressionValue(f, n, oper, t, mode, flags)
	case exprBool:
		p.binaryShiftExpressionBool(f, n, oper, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryShiftExpressionBool(f *function, n *cc.ShiftExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, n.Operand.Type(), &mode, flags))
	switch {
	case n.ShiftExpression.Operand.Type().IsBitFieldType():
		panic(todo(""))
		p.w("(")
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
		p.w(")&%#x", n.ShiftExpression.Operand.Type().BitField().Mask())
	case shiftOverflows(n.ShiftExpression.Operand, n.AdditiveExpression.Operand, oper, n.Operand.Type()):
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags|fForceRuntimeConv)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
	case isConstInteger(n.ShiftExpression.Operand):
		s := p.convert(n, nil, n.Operand.Type(), 0)
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags)
		p.w("%s %s%s", s, oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
	default:
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
	}
}

func (p *project) binaryShiftExpressionValue(f *function, n *cc.ShiftExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, t, &mode, flags))
	switch {
	case n.ShiftExpression.Operand.Type().IsBitFieldType():
		p.w("(")
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
		p.w(")&%#x", n.ShiftExpression.Operand.Type().BitField().Mask())
	case shiftOverflows(n.ShiftExpression.Operand, n.AdditiveExpression.Operand, oper, n.Operand.Type()):
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags|fForceRuntimeConv)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
	case isConstInteger(n.ShiftExpression.Operand):
		s := p.convert(n, nil, n.Operand.Type(), 0)
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags)
		p.w("%s %s%s", s, oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
	default:
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
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

func (p *project) additiveExpression(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.additiveExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.additiveExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.additiveExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.additiveExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.additiveExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.additiveExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.additiveExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.additiveExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) additiveExpressionSelect(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionFunc(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionPSelect(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, t.Elem()))
		p.additiveExpression(f, n, t, exprValue, flags)
		p.w("))")
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, t.Elem()))
		p.additiveExpression(f, n, t, exprValue, flags)

	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionLValue(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionBool(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "+", t, mode, flags)
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "-", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionAddrOf(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionVoid(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionValue(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "+", t, mode, flags)
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		p.binaryAdditiveExpression(f, n, "-", t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) binaryAdditiveExpression(f *function, n *cc.AdditiveExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.binaryAdditiveExpressionValue(f, n, oper, t, mode, flags)
	case exprBool:
		p.binaryAdditiveExpressionBool(f, n, oper, t, mode, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) binaryAdditiveExpressionBool(f *function, n *cc.AdditiveExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, n.Operand.Type(), &mode, flags))
	lo := n.AdditiveExpression.Operand
	ro := n.MultiplicativeExpression.Operand
	lt := lo.Type()
	rt := ro.Type()
	switch {
	case lt.IsArithmeticType() && rt.IsArithmeticType(): // x +- y
		defer p.w("%s", p.bitFieldPatch2(lo, ro, n.Promote()))
		switch {
		case intAddOverflows(lo, ro, oper, n.Promote()): // i +- j
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
			p.w(" %s%s", oper, tidyComment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
		default:
			var s string
			if isRealType(n.Operand) && n.Operand.Value() != nil {
				s = p.convert(n, nil, n.Promote(), flags)
			}
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
			p.w("%s %s%s", s, oper, tidyComment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, flags)
		}
	default:
		panic(todo("", n.Position(), lt, rt, oper))
	}
}

func (p *project) binaryAdditiveExpressionValue(f *function, n *cc.AdditiveExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, t, &mode, flags))
	lo := n.AdditiveExpression.Operand
	ro := n.MultiplicativeExpression.Operand
	lt := lo.Type()
	rt := ro.Type()
	switch {
	case lt.IsArithmeticType() && rt.IsArithmeticType(): // x +- y
		defer p.w("%s", p.bitFieldPatch2(lo, ro, n.Promote()))
		switch {
		case intAddOverflows(lo, ro, oper, n.Promote()): // i +- j
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
			p.w(" %s%s", oper, tidyComment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
		default:
			var s string
			if isRealType(n.Operand) && n.Operand.Value() != nil {
				s = p.convert(n, nil, n.Promote(), flags)
			}
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
			p.w("%s %s%s", s, oper, tidyComment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, flags)
		}
	case lt.Kind() == cc.Ptr && rt.IsIntegerType(): // p +- i
		p.additiveExpression(f, n.AdditiveExpression, lt, exprValue, flags)
		p.w(" %s%s uintptr(", oper, tidyComment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, exprValue, flags)
		p.w(")")
		if sz := lt.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	case lt.Kind() == cc.Array && rt.IsIntegerType(): // p +- i
		p.additiveExpression(f, n.AdditiveExpression, lt, exprAddrOf, flags)
		p.w(" %s%s uintptr(", oper, tidyComment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, exprValue, flags)
		p.w(")")
		if sz := lt.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	case lt.IsIntegerType() && rt.Kind() == cc.Ptr: // i +- p
		p.w("uintptr(")
		p.additiveExpression(f, n.AdditiveExpression, lt, exprValue, flags)
		p.w(")")
		if sz := rt.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w(" %s%s ", oper, tidyComment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, exprValue, flags)
	case lt.IsIntegerType() && rt.Kind() == cc.Array: // i +- p
		panic(todo(""))
	case lt.Kind() == cc.Ptr && rt.Kind() == cc.Ptr && oper == "-": // p - q
		p.w("(")
		p.additiveExpression(f, n.AdditiveExpression, n.Operand.Type(), exprValue, flags)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Operand.Type(), exprValue, flags)
		p.w(")/%d", lt.Elem().Size())
	case lt.Kind() == cc.Ptr && rt.Kind() == cc.Array && oper == "-": // p - q
		defer p.w("%s", p.convertType(n, nil, n.Operand.Type(), 0))
		p.w("(")
		p.additiveExpression(f, n.AdditiveExpression, lt, exprValue, flags)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, rt.Decay(), exprAddrOf, flags&^fOutermost)
		p.w(")/%d", lt.Elem().Size())
	case lt.Kind() == cc.Array && rt.Kind() == cc.Ptr && oper == "-": // p - q
		panic(todo("", pos(n)))
	case lt.Kind() == cc.Array && rt.Kind() == cc.Array && oper == "-": // p - q
		panic(todo("", pos(n)))
	default:
		panic(todo("", n.Position(), lt, rt, oper))
	}
}

func (p *project) bitFieldPatch2(a, b cc.Operand, promote cc.Type) string {
	var m uint64
	var w int
	switch {
	case a.Type().IsBitFieldType():
		bf := a.Type().BitField()
		w = bf.BitFieldWidth()
		m = bf.Mask() >> bf.BitFieldOffset()
		if b.Type().IsBitFieldType() {
			bf = b.Type().BitField()
			w2 := bf.BitFieldWidth()
			if w2 != w {
				panic(todo(""))
			}
		}
	case b.Type().IsBitFieldType():
		bf := b.Type().BitField()
		w = bf.BitFieldWidth()
		m = bf.Mask() >> bf.BitFieldOffset()
	default:
		return ""
	}

	switch {
	case promote.IsSignedType():
		n := int(promote.Size())*8 - w
		p.w("(")
		return fmt.Sprintf(")&%#x<<%d>>%[2]d", m, n)
	default:
		p.w("(")
		return fmt.Sprintf(")&%#x", m)
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

func (p *project) multiplicativeExpression(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.multiplicativeExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.multiplicativeExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.multiplicativeExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.multiplicativeExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.multiplicativeExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.multiplicativeExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.multiplicativeExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.multiplicativeExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) multiplicativeExpressionSelect(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionFunc(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionPSelect(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionLValue(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionBool(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case
		cc.MultiplicativeExpressionMul, // MultiplicativeExpression '*' CastExpression
		cc.MultiplicativeExpressionDiv, // MultiplicativeExpression '/' CastExpression
		cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression

		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.multiplicativeExpression(f, n, t, exprValue, flags|fOutermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionAddrOf(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionVoid(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo(""))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionValue(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
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
	switch mode {
	case exprValue:
		p.binaryMultiplicativeExpressionValue(f, n, oper, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryMultiplicativeExpressionValue(f *function, n *cc.MultiplicativeExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n, n.Operand, t, &mode, flags))
	switch {
	case intMulOverflows(n.Operand, n.MultiplicativeExpression.Operand, n.CastExpression.Operand, oper, n.Promote()):
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.castExpression(f, n.CastExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
	default:
		defer p.w("%s", p.bitFieldPatch2(n.MultiplicativeExpression.Operand, n.CastExpression.Operand, n.Promote()))
		var s string
		if isRealType(n.Operand) && n.Operand.Value() != nil {
			s = p.convert(n, nil, n.Promote(), flags)
		}
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, flags)
		p.w("%s %s%s", s, oper, tidyComment(" ", &n.Token))
		if (oper == "/" || oper == "%") && (isZeroReal(n.MultiplicativeExpression.Operand) || isZeroReal(n.CastExpression.Operand)) {
			p.w("%s%sFrom%[2]s(", p.task.crt, p.helperType(n, n.Promote()))
			defer p.w(")")
		}
		p.castExpression(f, n.CastExpression, n.Promote(), exprValue, flags)
	}
}

func isZeroReal(op cc.Operand) bool {
	switch x := op.Value().(type) {
	case cc.Float32Value:
		return x == 0
	case cc.Float64Value:
		return x == 0
	default:
		return false
	}
}

func intMulOverflows(r, lo, ro cc.Operand, oper string, promote cc.Type) bool {
	if (isReal(lo) && !isInf(lo) || isReal(ro) && !isInf(ro)) && isInf(r) {
		return true
	}

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

func isReal(op cc.Operand) bool {
	switch op.Value().(type) {
	case cc.Float32Value, cc.Float64Value:
		return true
	default:
		return false
	}
}

func isInf(op cc.Operand) bool {
	switch x := op.Value().(type) {
	case cc.Float32Value:
		return math.IsInf(float64(x), 0)
	case cc.Float64Value:
		return math.IsInf(float64(x), 0)
	default:
		return false
	}
}

func (p *project) castExpression(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	if n.Case == cc.CastExpressionCast {
		if f != nil && n.CastExpression.Operand.Type().Kind() == cc.Ptr { // void *__ccgo_va_arg(__builtin_va_list ap);
			sv := f.vaType
			f.vaType = n.TypeName.Type()
			defer func() { f.vaType = sv }()
		}
	}
	switch mode {
	case exprValue:
		p.castExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.castExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.castExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.castExpressionBool(f, n, t, mode, flags)
	case exprLValue:
		p.castExpressionLValue(f, n, t, mode, flags)
	case exprPSelect:
		p.castExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.castExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.castExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) castExpressionSelect(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		p.castExpression(f, n.CastExpression, n.TypeName.Type(), mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionFunc(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		ot := n.CastExpression.Operand.Type()
		tn := n.TypeName.Type()
		var ft cc.Type
		switch tn.Kind() {
		case cc.Ptr:
			switch et := ot.Elem(); et.Kind() {
			case cc.Function:
				// ok
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("%v: %v, %v -> %v, %v -> %v, %v", pos(n), ot, ot.Kind(), tn, tn.Kind(), t, t.Kind()))
		}
		switch t.Kind() {
		case cc.Ptr:
			switch et := t.Elem(); et.Kind() {
			case cc.Function:
				ft = et
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("%v: %v, %v -> %v, %v -> %v, %v", pos(n), ot, ot.Kind(), tn, tn.Kind(), t, t.Kind()))
		}
		switch ot.Kind() {
		//TODO- case op.Type().Kind() == cc.Ptr:
		//TODO- 	switch {
		//TODO- 	case tn.Kind() == cc.Ptr && tn.Elem().Kind() == cc.Function && t.Kind() == cc.Ptr:
		//TODO- 		ft := tn.Elem().Alias()
		//TODO- 		switch ft.Kind() {
		//TODO- 		default:
		//TODO- 			panic(todo("", pos(n), n.CastExpression.Operand.Type(), n.CastExpression.Operand.Type().Kind()))
		//TODO- 		}
		//TODO- 		if ft.Kind() == cc.Ptr { //TODO probably wrong
		//TODO- 			ft = ft.Elem()
		//TODO- 		}
		//TODO- 		p.w("(*(*")
		//TODO- 		p.functionSignature(f, ft, "")
		//TODO- 		p.w(")(unsafe.Pointer(")
		//TODO- 		p.castExpression(f, n.CastExpression, op.Type(), exprAddrOf, flags)
		//TODO- 		p.w(")))")
		//TODO- 	default:
		//TODO- 		panic(todo(""))
		//TODO- 	}
		case cc.Ptr:
			switch et := ot.Elem(); et.Kind() {
			case cc.Function:
				p.w("(*(*")
				p.functionSignature(f, ft, "")
				p.w(")(unsafe.Pointer(")
				p.castExpression(f, n.CastExpression, ot, exprAddrOf, flags)
				p.w(")))")
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("%v: %v, %v -> %v, %v -> %v, %v", pos(n), ot, ot.Kind(), tn, tn.Kind(), t, t.Kind()))
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionPSelect(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		p.castExpression(f, n.CastExpression, n.TypeName.Type(), mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionLValue(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionBool(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.castExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionAddrOf(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		p.castExpressionAddrOf(f, n.CastExpression, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionVoid(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionValue(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		switch k := p.opKind(f, n.CastExpression, n.CastExpression.Operand.Type()); k {
		case opNormal, opBitfield:
			p.castExpressionValueNormal(f, n, t, mode, flags)
		case opArray:
			p.castExpressionValueArray(f, n, t, mode, flags)
		case opFunction:
			p.castExpressionValueFunction(f, n, t, mode, flags)
		case opArrayParameter:
			p.castExpressionValueNormal(f, n, t, mode, flags)
		default:
			panic(todo("", n.Position(), k))
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionValueArrayParameter(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	tn := n.TypeName.Type()
	defer p.w("%s", p.convertType(n, tn, t, flags))
	p.castExpression(f, n.CastExpression, tn, mode, flags)
}

func (p *project) castExpressionValueFunction(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	op := n.CastExpression.Operand
	tn := n.TypeName.Type()
	switch {
	case op.Type().Kind() == cc.Function:
		switch {
		case tn.Kind() == cc.Ptr && t.Kind() == cc.Ptr:
			p.castExpression(f, n.CastExpression, op.Type(), exprValue, flags)
		default:
			panic(todo(""))
		}
	default:
		panic(todo("%v: %v -> %v -> %v", pos(n), op.Type(), tn, t))
	}
}

func (p *project) castExpressionValueArray(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	tn := n.TypeName.Type()
	switch {
	case tn.IsScalarType():
		defer p.w("%s", p.convertType(n, nil, t, flags))
		p.castExpression(f, n.CastExpression, tn, exprAddrOf, flags)
	default:
		panic(todo("", pos(n)))
	}
}

func (p *project) castExpressionValueNormal(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	op := n.CastExpression.Operand
	tn := n.TypeName.Type()
	switch {
	case op.Type().Kind() == cc.Ptr && tn.IsArithmeticType():
		defer p.w("%s", p.convertType(n, nil, t, flags|fForceConv))
		p.castExpression(f, n.CastExpression, op.Type(), mode, flags)
	case tn.IsArithmeticType():
		switch {
		case (tn.Kind() == cc.Float || tn.Kind() == cc.Double) && op.Type().IsIntegerType() && op.Value() != nil && t.IsIntegerType():
			panic(todo(""))
		case isNegativeInt(op) && isUnsigned(t):
			defer p.w("%s", p.convertType(n, tn, t, flags|fForceConv))
			p.castExpression(f, n.CastExpression, tn, exprValue, flags|fOutermost)
		default:
			defer p.w("%s", p.convertType(n, tn, t, flags))
			p.castExpression(f, n.CastExpression, tn, exprValue, flags)
		}
	default:
		switch tn.Kind() {
		case cc.Ptr:
			switch {
			case t.Kind() == cc.Ptr && isNegativeInt(op):
				p.w("%s(", p.helperType2(n, op.Type(), tn))
				defer p.w(")")
				p.castExpression(f, n.CastExpression, op.Type(), mode, flags)
			default:
				defer p.w("%s", p.convertType(n, tn, t, flags))
				p.castExpression(f, n.CastExpression, tn, mode, flags)
			}
		case cc.Void:
			p.castExpression(f, n.CastExpression, tn, exprVoid, flags)
		default:
			panic(todo("", n.Position(), t, t.Kind()))
		}
	}
}

func (p *project) unaryExpression(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprLValue:
		p.unaryExpressionLValue(f, n, t, mode, flags)
	case exprValue:
		p.unaryExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.unaryExpressionVoid(f, n, t, mode, flags)
	case exprAddrOf:
		p.unaryExpressionAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.unaryExpressionBool(f, n, t, mode, flags)
	case exprPSelect:
		p.unaryExpressionPSelect(f, n, t, mode, flags)
	case exprFunc:
		p.unaryExpressionFunc(f, n, t, mode, flags)
	case exprSelect:
		p.unaryExpressionSelect(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) unaryExpressionSelect(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		ot := n.CastExpression.Operand.Type()
		switch ot.Kind() {
		case cc.Ptr:
			switch et := ot.Elem(); et.Kind() {
			case
				cc.Struct,
				cc.Union:

				p.w("(*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
				p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags)
				p.w(")))")
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", pos(n), ot, ot.Kind()))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo(""))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo(""))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo(""))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpressionFunc(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		ot := n.CastExpression.Operand.Type()
		switch ot.Kind() {
		case cc.Ptr:
			switch et := ot.Elem(); et.Kind() {
			case cc.Function:
				p.castExpression(f, n.CastExpression, ot, mode, flags|fAddrOfFuncPtrOk)
			case cc.Ptr:
				switch et2 := et.Elem(); et2.Kind() {
				case cc.Function:
					p.w("(**(**")
					p.functionSignature(f, et2, "")
					p.w(")(unsafe.Pointer(")
					p.castExpression(f, n.CastExpression, ot, exprAddrOf, flags|fAddrOfFuncPtrOk)
					p.w(")))")
				default:
					panic(todo("", pos(n), et2, et2.Kind()))
				}
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		case cc.Function:
			p.castExpression(f, n.CastExpression, ot, mode, flags|fAddrOfFuncPtrOk)
		default:
			panic(todo("", pos(n), ot, ot.Kind(), mode))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo(""))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo(""))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo(""))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpressionPSelect(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		p.unaryExpression(f, n, t, exprValue, flags)
		p.w("))")
	case cc.UnaryExpressionDeref: // '*' CastExpression
		ot := n.CastExpression.Operand.Type()
		switch ot.Kind() {
		case cc.Ptr:
			switch et := ot.Elem(); {
			case et.Kind() == cc.Ptr:
				switch et2 := et.Elem(); et2.Kind() {
				case cc.Struct:
					p.w("(*(**%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
					p.castExpression(f, n.CastExpression, t, exprValue, flags)
					p.w(")))")
				default:
					panic(todo("", pos(n), et2, et2.Kind()))
				}
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", pos(n), ot, ot.Kind()))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo(""))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo(""))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo(""))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpressionBool(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionNot: // '!' CastExpression
		p.w("!(")
		p.castExpression(f, n.CastExpression, t, mode, flags|fOutermost)
		p.w(")")
	default:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.unaryExpression(f, n, t, exprValue, flags|fOutermost)
	}
}

func (p *project) unaryExpressionAddrOf(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		ot := n.CastExpression.Operand.Type()
		switch ot.Kind() {
		case cc.Ptr:
			switch et := ot.Elem(); {
			case
				et.IsScalarType(),
				et.Kind() == cc.Struct,
				et.Kind() == cc.Union,
				et.Kind() == cc.Array:

				p.unaryExpressionDeref(f, n, t, mode, flags)
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", pos(n), ot, ot.Kind()))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo("", n.Position()))
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

func (p *project) unaryExpressionVoid(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "++", "+=", t, mode, flags)
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "--", "-=", t, mode, flags)
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		ot := n.CastExpression.Operand.Type()
		switch ot.Kind() {
		case cc.Ptr:
			switch et := ot.Elem(); {
			case et.IsScalarType():
				p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprVoid, flags)
			default:
				panic(todo("", pos(n), ot, ot.Kind()))
			}
		default:
			panic(todo("", pos(n), ot, ot.Kind()))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo("", n.Position()))
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

func (p *project) unaryExpressionValue(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "++", "+=", t, mode, flags)
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "--", "-=", t, mode, flags)
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		if t.Kind() != cc.Ptr {
			defer p.w("%s", p.convert(n, n.Operand, t, flags))
		}
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprAddrOf, flags&^fOutermost)
	case cc.UnaryExpressionDeref: // '*' CastExpression
		ot := n.CastExpression.Operand.Type()
		switch ot.Kind() {
		case cc.Ptr, cc.Array:
			switch et := ot.Elem(); {
			case
				et.IsScalarType(),
				et.Kind() == cc.Array,
				et.Kind() == cc.Struct,
				et.Kind() == cc.Union:

				p.unaryExpressionDeref(f, n, t, mode, flags)
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", pos(n), ot, ot.Kind()))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		p.w(" +")
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.UnaryExpressionMinus: // '-' CastExpression
		switch {
		case isNonNegativeInt(n.CastExpression.Operand) && t.Kind() == cc.Ptr:
			p.w(" -%sUintptr(", p.task.crt)
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags)
			p.w(")")
		case isZeroReal(n.CastExpression.Operand):
			p.w(" -")
			defer p.w("%s", p.convert(n, n.CastExpression.Operand, t, flags))
			p.w("%s%sFrom%[2]s(", p.task.crt, p.helperType(n, n.CastExpression.Operand.Type()))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags)
			p.w(")")
		case isNonNegativeInt(n.CastExpression.Operand) && isUnsigned(n.Operand.Type()):
			p.w(" -")
			defer p.w("%s", p.convert(n, n.CastExpression.Operand, t, flags|fForceRuntimeConv))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags)
		default:
			defer p.w("%s", p.convert(n, n.Operand, t, flags))
			p.w(" -")
			p.castExpression(f, n.CastExpression, n.Operand.Type(), exprValue, flags)
		}
	case cc.UnaryExpressionCpl: // '~' CastExpression
		switch {
		case n.CastExpression.Operand.Value() != nil:
			switch {
			case !t.IsIntegerType():
				defer p.w("%s", p.convert(n, n.Operand, t, flags))
				p.w(" ^")
				p.castExpression(f, n.CastExpression, n.Operand.Type(), exprValue, flags|fForceRuntimeConv)
			default:
				p.w("^")
				defer p.w("%s", p.convert(n, n.CastExpression.Operand, t, flags|fForceConv))
				p.w("%s%sFrom%[2]s(", p.task.crt, p.helperType(n, n.Operand.Type()))
				p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags|fOutermost)
				p.w(")")
			}
		default:
			defer p.w("%s", p.convert(n, n.Operand, t, flags))
			p.w(" ^")
			p.castExpression(f, n.CastExpression, n.Operand.Type(), exprValue, flags)
		}
	case cc.UnaryExpressionNot: // '!' CastExpression
		p.w("%sBool%s(!(", p.task.crt, p.helperType(n, t))
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprBool, flags|fOutermost)
		p.w("))")
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		p.intConst(n, "", n.Operand, t, flags)
		//TODO defer p.w("%s", p.convert(n, nil, t, flags))
		//TODO if d := n.UnaryExpression.Declarator(); d != nil {
		//TODO 	if f != nil {
		//TODO 		if local := f.locals[d]; local != nil && !local.isPinned {
		//TODO 			p.w("unsafe.Sizeof(%s)", local.name)
		//TODO 			break
		//TODO 		}
		//TODO 	}

		//TODO 	if tld := p.tlds[d]; tld != nil {
		//TODO 		p.w("unsafe.Sizeof(%s)", tld.name)
		//TODO 		break
		//TODO 	}

		//TODO 	nm := d.Name().String()
		//TODO 	if imp := p.imports[nm]; imp != nil {
		//TODO		imp.used = true
		//TODO 		p.w("unsafe.Sizeof(%sX%s)", imp.qualifier, nm)
		//TODO 		break
		//TODO 	}
		//TODO }

		//TODO t := n.UnaryExpression.Operand.Type()
		//TODO if p.isArray(f, n.UnaryExpression, t) {
		//TODO 	p.w("%d", t.Len()*t.Elem().Size())
		//TODO 	break
		//TODO }

		//TODO s := "(0)"
		//TODO if !t.IsArithmeticType() {
		//TODO 	switch t.Kind() {
		//TODO 	case cc.Ptr:
		//TODO 		// ok
		//TODO 	case cc.Struct, cc.Union, cc.Array:
		//TODO 		s = "{}"
		//TODO 	default:
		//TODO 		panic(todo("", t.Kind()))
		//TODO 	}
		//TODO }
		//TODO p.w("unsafe.Sizeof(%s%s)", p.typ(n, t), s)
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		p.intConst(n, "", n.Operand, t, flags)
		//TODO defer p.w("%s", p.convert(n, nil, t, flags))
		//TODO t := n.TypeName.Type()
		//TODO if t.Kind() == cc.Array {
		//TODO 	p.w("%d", t.Len()*t.Elem().Size())
		//TODO 	break
		//TODO }

		//TODO s := "(0)"
		//TODO if !t.IsArithmeticType() {
		//TODO 	switch t.Kind() {
		//TODO 	case cc.Ptr:
		//TODO 		// ok
		//TODO 	case cc.Struct, cc.Union:
		//TODO 		s = "{}"
		//TODO 	default:
		//TODO 		panic(todo("", t.Kind()))
		//TODO 	}
		//TODO }
		//TODO p.w("unsafe.Sizeof(%s%s)", p.typ(n, t), s)
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		p.intConst(n, "", n.Operand, t, flags)
		//TODO if n.TypeName.Type().Kind() == cc.Void {
		//TODO 	p.intConst(n, "", n.Operand, t, flags)
		//TODO 	break
		//TODO }

		//TODO defer p.w("%s", p.convert(n, nil, t, flags))
		//TODO t := n.UnaryExpression.Operand.Type()
		//TODO if p.isArray(f, n.UnaryExpression, t) {
		//TODO 	p.w("%d", t.Len()*t.Elem().Size())
		//TODO 	break
		//TODO }

		//TODO s := "(0)"
		//TODO if !t.IsArithmeticType() {
		//TODO 	switch t.Kind() {
		//TODO 	case cc.Ptr:
		//TODO 		// ok
		//TODO 	case cc.Struct, cc.Union:
		//TODO 		s = "{}"
		//TODO 	default:
		//TODO 		panic(todo("", t.Kind()))
		//TODO 	}
		//TODO }
		//TODO p.w("unsafe.Alignof(%s%s)", p.typ(n, t), s)
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		p.intConst(n, "", n.Operand, t, flags)
		//TODO if n.TypeName.Type().Kind() == cc.Void {
		//TODO 	p.intConst(n, "", n.Operand, t, flags)
		//TODO 	break
		//TODO }

		//TODO defer p.w("%s", p.convert(n, nil, t, flags))
		//TODO t := n.TypeName.Type()
		//TODO if t.Kind() == cc.Array {
		//TODO 	p.w("%d", t.Len()*t.Elem().Size())
		//TODO 	break
		//TODO }

		//TODO s := "(0)"
		//TODO if !t.IsArithmeticType() {
		//TODO 	switch t.Kind() {
		//TODO 	case cc.Ptr:
		//TODO 		// ok
		//TODO 	case cc.Struct, cc.Union:
		//TODO 		s = "{}"
		//TODO 	default:
		//TODO 		panic(todo("", t.Kind()))
		//TODO 	}
		//TODO }
		//TODO p.w("unsafe.Alignof(%s%s)", p.typ(n, t), s)
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpressionLValue(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		ot := n.CastExpression.Operand.Type()
		switch ot.Kind() {
		case cc.Ptr, cc.Array:
			switch et := ot.Elem(); {
			case
				et.IsScalarType(),
				et.Kind() == cc.Struct,
				et.Kind() == cc.Union:

				p.unaryExpressionDeref(f, n, t, mode, flags)
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", pos(n), ot, ot.Kind()))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo("", n.Position()))
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

func (p *project) unaryExpressionPreIncDec(f *function, n *cc.UnaryExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	switch mode {
	case exprValue:
		p.unaryExpressionPreIncDecValue(f, n, oper, oper2, t, mode, flags)
	case exprVoid:
		p.unaryExpressionPreIncDecVoid(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", pos(n), mode))
	}
}

func (p *project) unaryExpressionPreIncDecVoid(f *function, n *cc.UnaryExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionPreIncDecVoidNormal(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionPreIncDecVoidNormal(f *function, n *cc.UnaryExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	ut := n.UnaryExpression.Operand.Type()
	p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), exprLValue, flags)
	if ut.IsIntegerType() || ut.Kind() == cc.Ptr && p.incDelta(n, ut) == 1 {
		p.w("%s", oper)
		return
	}

	switch ut.Kind() {
	case cc.Ptr, cc.Double, cc.Float:
		p.w("%s %d", oper2, p.incDelta(n, ut))
		return
	}

	panic(todo(""))
}

func (p *project) unaryExpressionPreIncDecValue(f *function, n *cc.UnaryExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionPreIncDecValueNormal(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionPreIncDecValueNormal(f *function, n *cc.UnaryExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	defer p.w("%s", p.convert(n, n.UnaryExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	ut := n.UnaryExpression.Operand.Type()
	p.w("%sPre%s%s(&", p.task.crt, x, p.helperType(n, ut))
	p.unaryExpression(f, n.UnaryExpression, ut, exprLValue, flags)
	p.w(", %d)", p.incDelta(n.PostfixExpression, ut))
}

func (p *project) unaryExpressionDeref(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch mode {
	case exprValue:
		p.unaryExpressionDerefValue(f, n, t, mode, flags)
	case exprLValue:
		p.unaryExpressionDerefLValue(f, n, t, mode, flags)
	case exprAddrOf:
		p.unaryExpressionDerefAddrOf(f, n, t, mode, flags)
	case exprBool:
		p.unaryExpressionDerefBool(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) unaryExpressionDerefBool(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	if flags&fOutermost == 0 {
		p.w("(")
		defer p.w(")")
	}
	p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags|fOutermost)
	p.w(")) != 0")
}

func (p *project) unaryExpressionDerefAddrOf(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags)
}

func (p *project) unaryExpressionDerefLValue(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch k := p.opKind(f, n.CastExpression, n.CastExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionDerefLValueNormal(f, n, t, mode, flags)
	case opArray:
		panic(todo(""))
		p.unaryExpressionDerefLValueArray(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionDerefLValueArray(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("))%s", p.convertType(n, n.CastExpression.Operand.Type().Elem(), t, flags))
	p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags|fOutermost)
}

func (p *project) unaryExpressionDerefLValueNormal(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("))%s", p.convertType(n, n.CastExpression.Operand.Type().Elem(), t, flags))
	p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags|fOutermost)
}

func (p *project) unaryExpressionDerefValue(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch k := p.opKind(f, n.CastExpression, n.CastExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionDerefValueNormal(f, n, t, mode, flags)
	case opArray:
		p.unaryExpressionDerefValueArray(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionDerefValueArray(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.convertType(n, n.CastExpression.Operand.Type().Elem(), t, flags))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags|fOutermost)
	p.w("[0]")
}

func (p *project) unaryExpressionDerefValueNormal(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch op := n.Operand.Type(); {
	case op.Kind() == cc.Array:
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), mode, flags)
	default:
		defer p.w("))%s", p.convertType(n, n.CastExpression.Operand.Type().Elem(), t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), mode, flags|fOutermost)
	}
}

func (p *project) postfixExpression(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprLValue:
		p.postfixExpressionLValue(f, n, t, mode, flags)
	case exprValue:
		p.postfixExpressionValue(f, n, t, mode, flags)
	case exprVoid:
		p.postfixExpressionVoid(f, n, t, mode, flags)
	case exprFunc:
		p.postfixExpressionFunc(f, n, t, mode, flags)
	case exprAddrOf:
		p.postfixExpressionAddrOf(f, n, t, mode, flags)
	case exprSelect:
		p.postfixExpressionSelect(f, n, t, mode, flags)
	case exprPSelect:
		p.postfixExpressionPSelect(f, n, t, mode, flags)
	case exprBool:
		p.postfixExpressionBool(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) postfixExpressionBool(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, exprValue, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, t, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, exprValue, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, exprValue, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, exprValue, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, exprValue, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionPSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionPSelectIndex(f, n, t, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionPSelectCall(f, n, t, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionPSelectSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionPSelectPSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo(""))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo(""))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionPSelectSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opStruct:
		p.postfixExpressionPSelectSelectStruct(f, n, t, mode, flags)
	case opUnion:
		p.postfixExpressionPSelectSelectUnion(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionPSelectSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*(**%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
		p.w("/* .%s */", p.fieldName(n.Token2.Value))
		p.w(")))")
	}
}

func (p *project) postfixExpressionPSelectSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, t.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
		p.w("))")

	}
}

func (p *project) postfixExpressionPSelectCall(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	p.w("(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
	p.postfixExpressionCall(f, n, t, exprValue, flags)
	p.w("))")
}

func (p *project) postfixExpressionPSelectIndex(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	// case opArray:
	// 	p.postfixExpressionSelectIndexArray(f, n, t, mode, flags)
	case opNormal:
		p.postfixExpressionPSelectIndexNormal(f, n, t, mode, flags)
	case opArrayParameter:
		p.postfixExpressionSelectIndexArrayParamater(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionPSelectIndexNormal(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	case pe.Kind() == cc.Array:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.w("(*(**%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w(")))")
	default:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.w("(*(**%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags|fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w(")))")
	}
}

func (p *project) postfixExpressionSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionSelectIndex(f, n, t, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionSelectSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionSelectPSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo(""))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo(""))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionPSelectPSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opStruct:
		p.postfixExpressionPSelectPSelectStruct(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionPSelectPSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		//TODO- p.w("(*%s/*6655 %v -> %v -> %v */)(unsafe.Pointer(", p.typ(n, t.Elem()), pe, n.Operand.Type(), t)
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, t.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectPSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opStruct:
		p.postfixExpressionSelectPSelectStruct(f, n, t, mode, flags)
	case opUnion:
		p.postfixExpressionSelectPSelectUnion(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionSelectPSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectPSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionSelectSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opUnion:
		p.postfixExpressionSelectSelectUnion(f, n, t, mode, flags)
	case opStruct:
		p.postfixExpressionSelectSelectStruct(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionSelectSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionSelectSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectIndex(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opArray:
		p.postfixExpressionSelectIndexArray(f, n, t, mode, flags)
	case opNormal:
		p.postfixExpressionSelectIndexNormal(f, n, t, mode, flags)
	case opArrayParameter:
		p.postfixExpressionSelectIndexArrayParamater(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionSelectIndexArrayParamater(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectIndexNormal(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	case pe.Kind() != cc.Ptr:
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	default:
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectIndexArray(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, mode, flags&^fOutermost)
		p.w("[")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost)
		p.w("]")
	}
}

func (p *project) postfixExpressionAddrOf(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionAddrOfIndex(f, n, t, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionAddrOfSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionAddrOfPSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo(""))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo(""))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionAddrOfPSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	if flags&fOutermost == 0 {
		p.w("(")
		defer p.w(")")
	}
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case pe.Kind() == cc.Array:
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
		p.fldOff(pe.Elem(), n.Token2)
	default:
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags|fOutermost)
		p.fldOff(pe.Elem(), n.Token2)
	}
}

func (p *project) postfixExpressionAddrOfIndex(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	if flags&fOutermost == 0 {
		p.w("(")
		defer p.w(")")
	}
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		fallthrough
	default:
		flags &^= fOutermost
		pe := n.PostfixExpression.Operand.Type()
		d := n.PostfixExpression.Declarator()
		switch {
		case pe.Kind() == cc.Ptr:
			p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		case pe.Kind() == cc.Array && d != nil && d.IsParameter:
			p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		default:
			p.postfixExpression(f, n.PostfixExpression, pe, mode, flags)
		}
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	}
}

func (p *project) postfixExpressionAddrOfSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	if flags&fOutermost == 0 {
		p.w("(")
		defer p.w(")")
	}
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		fallthrough
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, nil, mode, flags|fOutermost)
		p.fldOff(pe, n.Token2)
	}
}

func (p *project) postfixExpressionFunc(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		switch n.Operand.Type().Kind() {
		case cc.Ptr:
			switch et := n.Operand.Type().Elem(); et.Kind() {
			case cc.Function:
				p.w("(*(*")
				p.functionSignature(f, n.Operand.Type().Elem(), "")
				p.w(")(unsafe.Pointer(")
				p.postfixExpression(f, n, n.Operand.Type(), exprAddrOf, flags)
				p.w(")))")
			default:
				panic(todo("", pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		switch n.Operand.Type().Kind() {
		case cc.Ptr:
			switch n.Operand.Type().Kind() {
			case cc.Ptr:
				switch et := n.Operand.Type().Elem(); et.Kind() {
				case cc.Function:
					p.w("(*(*")
					p.functionSignature(f, n.Operand.Type().Elem(), "")
					p.w(")(unsafe.Pointer(")
					p.postfixExpression(f, n, n.Operand.Type(), exprAddrOf, flags)
					p.w(")))")
				default:
					panic(todo("", pos(n), et, et.Kind()))
				}
			default:
				panic(todo("", pos(n), n.Operand.Type(), n.Operand.Type().Kind()))
			}
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		switch n.Operand.Type().Kind() {
		case cc.Ptr:
			switch n.Operand.Type().Kind() {
			case cc.Ptr:
				switch et := n.Operand.Type().Elem(); et.Kind() {
				case cc.Function:
					p.w("(*(*")
					p.functionSignature(f, n.Operand.Type().Elem(), "")
					p.w(")(unsafe.Pointer(")
					p.postfixExpression(f, n, n.Operand.Type(), exprAddrOf, flags)
					p.w(")))")
				default:
					panic(todo("", pos(n), et, et.Kind()))
				}
			default:
				panic(todo("", pos(n), n.Operand.Type(), n.Operand.Type().Kind()))
			}
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo(""))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo(""))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionVoid(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		panic(todo(""))
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, t, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		if n.IsSideEffectsFree {
			break
		}
		p.w("_ = ")
		p.postfixExpression(f, n, n.Operand.Type(), exprValue, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionValue(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		if p.isArray(f, n.PrimaryExpression, n.Operand.Type()) && t.Kind() == cc.Ptr {
			mode = exprAddrOf
		}
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionValueIndex(f, n, t, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, t, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionValueSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionValuePSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionValuePSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opStruct:
		p.postfixExpressionValuePSelectStruct(f, n, t, mode, flags)
	case opUnion:
		p.postfixExpressionValuePSelectUnion(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionValuePSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w("/* .%s */", p.fieldName(n.Token2.Value))
		p.w("))")
	}
}

func (p *project) postfixExpressionValuePSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().IsBitFieldType():
		fld := n.Field
		defer p.w("%s", p.convertType(n, fld.Promote(), t, flags))
		switch pe.Kind() {
		case cc.Array:
			x := p.convertType(n, nil, fld.Promote(), flags)
			p.w("*(*uint%d)(unsafe.Pointer(", fld.BitFieldBlockWidth())
			p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
			p.fldOff(pe.Elem(), n.Token2)
			p.w("))")
			p.w("&%#x>>%d%s", fld.Mask(), fld.BitFieldOffset(), x)
			if fld.Type().IsSignedType() {
				p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
			}
		default:
			x := p.convertType(n, nil, fld.Promote(), flags)
			p.w("*(*uint%d)(unsafe.Pointer(", fld.BitFieldBlockWidth())
			p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
			p.fldOff(pe.Elem(), n.Token2)
			p.w("))&%#x>>%d%s", fld.Mask(), fld.BitFieldOffset(), x)
			if fld.Type().IsSignedType() {
				p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
			}
		}
	case n.Operand.Type().Kind() == cc.Array:
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.fldOff(n.PostfixExpression.Operand.Type().Elem(), n.Token2)
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionValueIndex(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opArray:
		p.postfixExpressionValueIndexArray(f, n, t, mode, flags)
	case opNormal:
		p.postfixExpressionValueIndexNormal(f, n, t, mode, flags)
	case opArrayParameter:
		p.postfixExpressionValueIndexArrayParameter(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}
func (p *project) postfixExpressionValueIndexArrayParameter(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags|fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	}
}

func (p *project) postfixExpressionValueIndexNormal(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags|fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	default:
		switch pe := n.PostfixExpression.Operand.Type(); pe.Kind() {
		case cc.Ptr:
			defer p.w("%s", p.convert(n, n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		case cc.Array:
			defer p.w("%s", p.convert(n, n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		default:
			panic(todo("", pos(n), pe))
		}
	}
}

func (p *project) postfixExpressionValueIndexArray(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	pe := n.PostfixExpression.Operand.Type()
	switch n.Operand.Type().Kind() {
	case cc.Array:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, mode, flags&^fOutermost)
		p.w("[")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost)
		p.w("]")
	}
}

func (p *project) postfixExpressionValueSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opStruct:
		p.postfixExpressionValueSelectStruct(f, n, t, mode, flags)
	case opUnion:
		p.postfixExpressionValueSelectUnion(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionValueSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionValueSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	fld := n.Field
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		p.postfixExpression(f, n, t, exprAddrOf, flags)
	case n.Operand.Type().IsBitFieldType():
		defer p.w("%s", p.convertType(n, fld.Promote(), t, flags))
		x := p.convertType(n, nil, fld.Promote(), flags)
		p.w("*(*uint%d)(unsafe.Pointer(", fld.BitFieldBlockWidth())
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
		p.fldOff(pe, n.Token2)
		p.w("))&%#x>>%d%s", fld.Mask(), fld.BitFieldOffset(), x)
		if fld.Type().IsSignedType() {
			p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
		}
	case isUnionField(pe, fld):
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, fld.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
		p.fldOff(pe, n.Token2)
		p.w("))")
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func isUnionField(t cc.Type, fld cc.Field) (r bool) {
	if t.Kind() == cc.Union {
		return true
	}

	isUnionField2(t, fld, 0, false, &r)
	return r
}

func isUnionField2(t cc.Type, fld cc.Field, off uintptr, inUnion bool, r *bool) {
	if t.Kind() == cc.Union {
		inUnion = true
	}
	for idx := []int{0}; idx[0] < t.NumField(); idx[0]++ {
		f := t.FieldByIndex(idx)
		if f.IsBitField() {
			continue
		}

		if f.Offset()+off > fld.Offset() {
			return
		}

		if f.Name() != 0 && f.Offset()+off == fld.Offset() {
			if inUnion {
				*r = true
			}
			return
		}
		switch ft := f.Type(); ft.Kind() {
		case cc.Struct, cc.Union:
			isUnionField2(f.Type(), fld, f.Offset(), inUnion, r)
		}
	}
}

func (p *project) postfixExpressionLValue(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionLValueIndex(f, n, t, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionLValueSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionLValuePSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionLValuePSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opStruct:
		p.postfixExpressionLValuePSelectStruct(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionLValuePSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionLValueIndex(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opArray:
		p.postfixExpressionLValueIndexArray(f, n, t, mode, flags)
	case opNormal:
		p.postfixExpressionLValueIndexNormal(f, n, t, mode, flags)
	case opArrayParameter:
		p.postfixExpressionLValueIndexArrayParameter(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionLValueIndexArrayParameter(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	defer p.w("%s", p.convert(n, n.Operand, t, flags))
	pe := n.PostfixExpression.Operand.Type()
	p.w("*(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
	p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags&^fOutermost)
	p.w(" + ")
	p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
	if sz := pe.Elem().Size(); sz != 1 {
		p.w("*%d", sz)
	}
	p.w("))")
}

func (p *project) postfixExpressionLValueIndexNormal(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		switch pe := n.PostfixExpression.Operand.Type(); pe.Kind() {
		case cc.Ptr:
			defer p.w("%s", p.convert(n, n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		case cc.Array:
			defer p.w("%s", p.convert(n, n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		default:
			panic(todo("", pos(n), pe))
		}
	}
}

func (p *project) postfixExpressionLValueIndexArray(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	pe := n.PostfixExpression.Operand.Type()
	p.postfixExpression(f, n.PostfixExpression, pe, mode, flags&^fOutermost)
	p.w("[")
	p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost)
	p.w("]")
}

func (p *project) postfixExpressionLValueSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opStruct:
		p.postfixExpressionLValueSelectStruct(f, n, t, mode, flags)
	case opUnion:
		p.postfixExpressionLValueSelectUnion(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionLValueSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionLValueSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionIncDec(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoid:
		p.postfixExpressionIncDecVoid(f, n, oper, oper2, t, mode, flags)
	case exprLValue:
		p.postfixExpressionIncDecLValue(f, n, oper, oper2, t, mode, flags)
	case exprValue:
		p.postfixExpressionIncDecValue(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) postfixExpressionIncDecValue(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "++"
	pe := n.PostfixExpression.Operand.Type()
	switch k := p.opKind(f, n.PostfixExpression, pe); k {
	case opNormal:
		p.postfixExpressionIncDecValueNormal(f, n, oper, oper2, t, mode, flags)
	case opBitfield:
		p.postfixExpressionIncDecValueBitfield(f, n, oper, oper2, t, mode, flags)
	case opArrayParameter:
		p.postfixExpressionIncDecValueArrayParameter(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", n.Position(), pe, pe.Kind(), k))
	}
}

func (p *project) postfixExpressionIncDecValueArrayParameter(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "++"
	pe := n.PostfixExpression.Operand.Type()
	defer p.w("%s", p.convert(n, n.PostfixExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	p.w("%sPost%s%s(&", p.task.crt, x, p.helperType(n, pe.Decay()))
	p.postfixExpression(f, n.PostfixExpression, pe, exprLValue, flags)
	p.w(", %d)", p.incDelta(n.PostfixExpression, pe.Elem()))
}

func (p *project) postfixExpressionIncDecValueBitfield(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "++"
	pe := n.PostfixExpression.Operand.Type()
	defer p.w("%s", p.convert(n, n.PostfixExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	bf := pe.BitField()
	p.w("%sPost%sBitFieldPtr%d%s(", p.task.crt, x, bf.BitFieldBlockWidth(), p.bfHelperType(pe))
	p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
	p.w(", %d, %d, %#x)", p.incDelta(n.PostfixExpression, pe), bf.BitFieldOffset(), bf.Mask())
}

func (p *project) postfixExpressionIncDecValueNormal(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "++"
	pe := n.PostfixExpression.Operand.Type()
	defer p.w("%s", p.convert(n, n.PostfixExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	p.w("%sPost%s%s(&", p.task.crt, x, p.helperType(n, pe))
	p.postfixExpression(f, n.PostfixExpression, pe, exprLValue, flags)
	p.w(", %d)", p.incDelta(n.PostfixExpression, pe))
}

func (p *project) postfixExpressionIncDecLValue(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	switch k := p.opKind(f, n, n.Operand.Type()); k {
	case opNormal:
		p.postfixExpressionIncDecLValueNormal(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionIncDecLValueNormal(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	defer p.w("%s", p.convert(n, n.PostfixExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	p.w("%sPost%s%s(&", p.task.crt, x, p.helperType(n, pe))
	p.postfixExpression(f, n.PostfixExpression, pe, exprLValue, flags)
	p.w(", %d)", p.incDelta(n.PostfixExpression, pe))
}

func (p *project) postfixExpressionIncDecVoid(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	switch k := p.opKind(f, n, n.Operand.Type()); k {
	case opNormal:
		p.postfixExpressionIncDecVoidNormal(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionIncDecVoidNormal(f *function, n *cc.PostfixExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type().Decay()
	p.postfixExpression(f, n.PostfixExpression, pe, exprLValue, flags)
	if pe.IsIntegerType() || pe.Kind() == cc.Ptr && p.incDelta(n, pe) == 1 {
		p.w("%s", oper)
		return
	}

	switch pe.Kind() {
	case cc.Ptr, cc.Float, cc.Double:
		p.w("%s %d", oper2, p.incDelta(n, pe))
		return
	}

	panic(todo("", n.Position(), pe, pe.Kind()))
}

func (p *project) incDelta(n cc.Node, t cc.Type) uintptr {
	if t.IsArithmeticType() {
		return 1
	}

	if t.Kind() == cc.Ptr || t.Kind() == cc.Array {
		return t.Elem().Size()
	}

	panic(todo("", n.Position(), t.Kind()))
}

func (p *project) fldOff(t cc.Type, tok cc.Token) {
	var off uintptr
	fld, ok := t.FieldByName(tok.Value)
	if ok && fld.IsBitField() {
		fld = fld.BitFieldBlockFirst()
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

func (p *project) postfixExpressionCall(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '(' ArgumentExpressionList ')'
	switch mode {
	case exprVoid:
		p.postfixExpressionCallVoid(f, n, t, mode, flags)
	case exprValue:
		p.postfixExpressionCallValue(f, n, t, mode, flags)
	case exprBool:
		p.postfixExpressionCallBool(f, n, t, mode, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) postfixExpressionCallBool(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '(' ArgumentExpressionList ')'
	if flags&fOutermost == 0 {
		p.w("(")
		defer p.w(")")
	}
	defer p.w(" != 0")
	if d := n.PostfixExpression.Declarator(); d != nil {
		switch d.Name() {
		case idVaArg:
			if !f.vaType.IsScalarType() {
				panic(todo("", f.vaType))
			}

			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.w("%sVa%s(&", p.task.crt, p.helperType(n, f.vaType))
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), exprLValue, flags)
			p.w(")")
			return
		}
	}

	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprFunc, flags&^fOutermost)
	p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
}

func (p *project) postfixExpressionCallValue(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '(' ArgumentExpressionList ')'
	defer p.w("%s", p.convert(n, n.Operand, t, flags))
	if d := n.PostfixExpression.Declarator(); d != nil {
		switch d.Name() {
		case idVaEnd:
			p.w("_ = ")
			arg := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, arg, arg.Operand.Type(), exprValue, flags)
			return
		case idVaStart:
			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), exprLValue, flags)
			p.w(" = %s", f.vaName)
			return
		case idVaArg:
			if !f.vaType.IsScalarType() {
				panic(todo("", f.vaType))
			}

			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.w("%sVa%s(&", p.task.crt, p.helperType(n, f.vaType))
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), exprLValue, flags)
			p.w(")")
			return
		}
	}
	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprFunc, flags&^fOutermost)
	p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
}

func (p *project) postfixExpressionCallVoid(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '(' ArgumentExpressionList ')'
	if d := n.PostfixExpression.Declarator(); d != nil {
		switch d.Name() {
		case idVaEnd:
			p.w("_ = ")
			arg := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, arg, arg.Operand.Type(), exprValue, flags)
			return
		case idVaStart:
			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), exprLValue, flags)
			p.w(" = %s", f.vaName)
			return
		case idVaArg:
			if !f.vaType.IsScalarType() {
				panic(todo("", f.vaType))
			}

			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.w("%sVa%s(&", p.task.crt, p.helperType(n, f.vaType))
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), exprLValue, flags)
			p.w(")")
			return
		}
	}
	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprFunc, flags&^fOutermost)
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
		p.w(",%s", tidyComment(" ", arg))
		mode := exprValue
		if at := arg.Operand.Type(); at.Kind() == cc.Array && p.detectArray(f, arg, true, true, nil) {
			mode = exprAddrOf
		}
		switch {
		case i < len(params):
			p.assignmentExpression(f, arg, arg.Promote(), mode, fOutermost)
		case va && i == len(params):
			p.w("%sVaList(%s%s, ", p.task.crt, f.bpName, nonZeroUintptr(bpOff))
			paren = ")"
			fallthrough
		default:
			p.assignmentExpression(f, arg, arg.Promote(), mode, fOutermost)
		}
	}
	if isVariadic && len(args) == len(params) {
		p.w(", 0")
	}
	p.w("%s)", paren)
}

func (p *project) uintptr(n cc.Node, f func(), op cc.Operand) {
	if op.Type().IsIntegerType() {
		switch {
		case isNegativeInt(op):
			p.w(" %sUintptrFrom%s(", p.task.crt, p.helperType(n, op.Type()))
		default:
			p.w(" uintptr(")
		}
		f()
		p.w(")")
		return
	}

	panic(todo(""))
}

func (p *project) primaryExpression(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprLValue:
		p.primaryExpressionLValue(f, n, t, mode, flags)
	case exprValue:
		p.primaryExpressionValue(f, n, t, mode, flags)
	case exprFunc:
		p.primaryExpressionFunc(f, n, t, mode, flags)
	case exprAddrOf:
		p.primaryExpressionAddrOf(f, n, t, mode, flags)
	case exprSelect:
		p.primaryExpressionSelect(f, n, t, mode, flags)
	case exprPSelect:
		p.primaryExpressionPSelect(f, n, t, mode, flags)
	case exprBool:
		p.primaryExpressionBool(f, n, t, mode, flags)
	case exprVoid:
		p.primaryExpressionVoid(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) primaryExpressionVoid(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	if mode == exprVoid && n.IsSideEffectsFree {
		return
	}

	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		panic(todo(""))
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo(""))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo(""))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionBool(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	if flags&fOutermost == 0 && n.Case != cc.PrimaryExpressionExpr {
		p.w("(")
		defer p.w(")")
	}

	if n.Case != cc.PrimaryExpressionExpr {
		defer p.w(" != 0")
	}
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(n, f, d, d.Type(), exprValue, flags)
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n, n.Token.Src.String(), n.Operand, n.Operand.Type(), flags)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo(""))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo(""))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w(" 1 ")
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.w("(")
		defer p.w(")")
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionPSelect(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			switch k := p.declaratorKind(d); k {
			case opArray:
				panic(todo(""))
				p.primaryExpression(f, n, t, exprAddrOf, flags)
			default:
				p.declarator(n, f, d, t, mode, flags)
			}
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo(""))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo(""))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo(""))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.expression(f, n.Expression, t, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionSelect(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(n, f, d, t, mode, flags)
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo(""))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo(""))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo(""))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.expression(f, n.Expression, t, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionAddrOf(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(n, f, d, t, mode, flags)
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo(""))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo(""))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo(""))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.expression(f, n.Expression, t, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionFunc(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			switch d.Type().Kind() {
			case cc.Function:
				p.declarator(n, f, d, t, mode, flags)
			case cc.Ptr:
				switch et := d.Type().Elem(); et.Kind() {
				case cc.Function:
					p.w("(*(*")
					p.functionSignature(f, et, "")
					p.w(")(unsafe.Pointer(&")
					p.primaryExpression(f, n, n.Operand.Type(), exprValue, flags)
					p.w(")))")
				default:
					panic(todo("", pos(n), pos(d), d.Type(), d.Type().Kind()))
				}
			default:
				panic(todo("", pos(n), pos(d), d.Type(), d.Type().Kind()))
			}
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo(""))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo(""))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo(""))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.expression(f, n.Expression, t, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionValue(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(n, f, d, t, mode, flags)
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n, n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		p.floatConst(n, n.Token.Src.String(), n.Operand, t, flags)
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

		p.intConst(n, "", n.Operand, t, flags)
	case cc.PrimaryExpressionChar: // CHARCONST
		p.charConst(n, n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.w("(")
		defer p.w(")")
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionLValue(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(n, f, d, t, mode, flags)
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo(""))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo(""))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo(""))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.w("(")
		defer p.w(")")
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) stringLiteralString(s string) string {
	id := cc.String(s)
	off, ok := p.tsOffs[id]
	if !ok {
		off = uintptr(p.ts.Len())
		p.ts.WriteString(s)
		p.ts.WriteByte(0)
		p.tsOffs[id] = off
	}
	return fmt.Sprintf("%s%s%s", p.tsNameP, nonZeroUintptr(off), p.stringSnippet(s))
}

func (p *project) stringLiteral(v cc.Value) string {
	switch x := v.(type) {
	case cc.StringValue:
		id := cc.StringID(x)
		off, ok := p.tsOffs[id]
		s := id.String()
		if !ok {
			off = uintptr(p.ts.Len())
			p.ts.WriteString(s)
			p.ts.WriteByte(0)
			p.tsOffs[id] = off
		}
		return fmt.Sprintf("%s%s%s", p.tsNameP, nonZeroUintptr(off), p.stringSnippet(s))
	default:
		panic(todo("%T", x))
	}
}

func (p *project) stringSnippet(s string) string {
	s = strings.ReplaceAll(s, "*/", "*\\/")
	const max = 16
	switch {
	case len(s) <= max:
		return fmt.Sprintf("/* %q */", s)
	default:
		return fmt.Sprintf("/* %q */", s[:16]+"...")
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

func (p *project) charConst(n cc.Node, src string, op cc.Operand, to cc.Type, flags flags) {
	switch {
	case to.IsArithmeticType():
		defer p.w("%s", p.convert(n, op, to, flags))
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

func (p *project) floatConst(n cc.Node, src string, op cc.Operand, to cc.Type, flags flags) {
	if flags&fForceRuntimeConv != 0 {
		p.w("%s(", p.helperType2(n, op.Type(), to))
		defer p.w(")")
	}

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

			p.w("math.Float64frombits(%#x)", math.Float64bits(float64(x)))
		case cc.Float:
			if snValid && float32(sn) == float32(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			p.w("math.Float32frombits(%#x)", math.Float32bits(float32(x)))
		default:
			defer p.w("%s", p.convert(n, op, to, 0))
			if snValid && sn == float64(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			p.w("math.Float64frombits(%#x)", math.Float64bits(float64(x)))
		}
	case cc.Float32Value:
		switch to.Kind() {
		case cc.Double:
			if snValid && float32(sn) == float32(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			p.w("math.Float64frombits(%#x)", math.Float64bits(float64(x)))
		case cc.Float:
			if snValid && float32(sn) == float32(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			p.w("math.Float32frombits(%#x)", math.Float32bits(float32(x)))
		default:
			if to.IsIntegerType() {
				if s := p.float2Int(x, to); s != "" {
					defer p.w("%s%s", s, p.convertType(n, op.Type(), to, 0))
					break
				}
			}

			defer p.w("%s", p.convert(n, op, to, 0))
			if snValid && float32(sn) == float32(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			p.w("math.Float32frombits(%#x)", math.Float32bits(float32(x)))
		}
	default:
		panic(todo("%T(%v)", x, x))
	}
}

func (p *project) float2Int(x cc.Float32Value, to cc.Type) string {
	switch {
	case to.IsSignedType():
		limits := &signedSaturationLimits[to.Size()]
		v := float64(x)
		switch {
		case math.IsNaN(v):
			panic(todo(""))
		case math.IsInf(v, -1):
			panic(todo(""))
		case math.IsInf(v, 1):
			panic(todo(""))
		case v < limits.fmin:
			return fmt.Sprint(limits.min)
		case v > limits.fmax:
			return fmt.Sprint(limits.max)
		}
	default:
		limits := &unsignedSaturationLimits[to.Size()]
		v := float64(x)
		switch {
		case math.IsNaN(v):
			panic(todo(""))
		case math.IsInf(v, -1):
			panic(todo(""))
		case math.IsInf(v, 1):
			panic(todo(""))
		case v < 0:
			return "0"
		case v > limits.fmax:
			return fmt.Sprint(limits.max)
		}
	}
	return ""
}

type signedSaturationLimit struct {
	fmin, fmax float64
	min, max   int64
}

type unsignedSaturationLimit struct {
	fmax float64
	max  uint64
}

var (
	signedSaturationLimits = [...]signedSaturationLimit{
		1: {math.Nextafter(math.MinInt32, 0), math.Nextafter(math.MaxInt32, 0), math.MinInt32, math.MaxInt32},
		2: {math.Nextafter(math.MinInt32, 0), math.Nextafter(math.MaxInt32, 0), math.MinInt32, math.MaxInt32},
		4: {math.Nextafter(math.MinInt32, 0), math.Nextafter(math.MaxInt32, 0), math.MinInt32, math.MaxInt32},
		8: {math.Nextafter(math.MinInt64, 0), math.Nextafter(math.MaxInt64, 0), math.MinInt64, math.MaxInt64},
	}

	unsignedSaturationLimits = [...]unsignedSaturationLimit{
		1: {math.Nextafter(math.MaxUint32, 0), math.MaxUint32},
		2: {math.Nextafter(math.MaxUint32, 0), math.MaxUint32},
		4: {math.Nextafter(math.MaxUint32, 0), math.MaxUint32},
		8: {math.Nextafter(math.MaxUint64, 0), math.MaxUint64},
	}
)

func (p *project) intConst(n cc.Node, src string, op cc.Operand, to cc.Type, flags flags) {
	ptr := to.Kind() == cc.Ptr
	switch {
	case to.IsArithmeticType():
		// p.w("/*8159 %T(%#[1]x) %v -> %v */", op.Value(), op.Type(), to) //TODO-
		defer p.w("%s", p.convert(n, op, to, flags))
	case ptr:
		p.w(" uintptr(")
		defer p.w(")")
		// ok
	default:
		panic(todo("%v -> %v", op.Type(), to))
	}

	src = strings.TrimRight(src, "luLU")
	sn, err := strconv.ParseUint(src, 0, 64)
	snValid := err == nil
	var on uint64
	switch x := op.Value().(type) {
	case cc.Int64Value:
		if x < 0 {
			sn, err := strconv.ParseInt(src, 0, 64)
			snValid := err == nil
			if snValid && sn == int64(x) { // Prefer original form
				p.w("%s", src)
				return
			}

			p.w("%d", x)
			return
		}

		on = uint64(x)
	case cc.Uint64Value:
		on = uint64(x)
	default:
		panic(todo("%T(%v)", x, x))
	}

	if snValid && sn == on { // Prefer original form
		p.w("%s", src)
		return
	}

	p.w("%d", on)
}

func (p *project) assignShiftOp(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "<<=" AssignmentExpression etc.
	switch mode {
	case exprVoid:
		p.assignShiftOpVoid(f, n, t, mode, oper, oper2, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) assignShiftOpVoid(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "<<=" AssignmentExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.assignShiftOpVoidNormal(f, n, t, mode, oper, oper2, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignShiftOpVoidNormal(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		if d := n.UnaryExpression.Declarator(); d != nil {
			p.declarator(n, f, d, d.Type(), exprLValue, flags|fOutermost)
			p.w(" %s= ", oper)
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
			return
		}

		lhs := n.UnaryExpression
		switch {
		case lhs.Operand.Type().IsArithmeticType():
			p.w("%sAssign%sPtr%s(", p.task.crt, oper2, p.helperType(n, lhs.Operand.Type()))
			p.unaryExpression(f, lhs, lhs.Operand.Type(), exprAddrOf, flags|fOutermost)
			p.w(", int(")
			p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), exprValue, flags|fOutermost)
			p.w("))")
		default:
			panic(todo("", pos(n), lhs.Operand.Type()))
		}
	}
}

func (p *project) assignOp(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	switch mode {
	case exprVoid:
		p.assignOpVoid(f, n, t, mode, oper, oper2, flags)
	case exprValue:
		p.assignOpValue(f, n, t, mode, oper, oper2, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) assignOpValue(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.assignOpValueNormal(f, n, t, oper, oper2, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignOpValueNormal(f *function, n *cc.AssignmentExpression, t cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	asInt := oper2 == "Shl" || oper2 == "Shr"
	// UnaryExpression "*=" AssignmentExpression etc.
	if d := n.UnaryExpression.Declarator(); d != nil {
		if local := f.locals[d]; local != nil && local.isPinned {
			panic(todo("", pos(n), pos(d), d.Name()))
			//TODO- 	p.declarator(f, d, d.Type(), exprLValue, flags)
			//TODO- 	switch {
			//TODO- 	case d.Type().IsArithmeticType():
			//TODO- 		p.w(" %s= ", oper)
			//TODO- 		defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
			//TODO- 		p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
			//TODO- 	default:
			//TODO- 		panic(todo("", d.Type().Kind()))
			//TODO- 	}
			//TODO- 	return
		}

		switch {
		case d.Type().Kind() == cc.Ptr:
			defer p.w("%s", p.convertType(n, d.Type(), t, flags))
			p.w("%sAssign%s%s(&", p.task.crt, oper2, p.helperType(n, d.Type()))
			p.declarator(n, f, d, d.Type(), exprLValue, flags|fOutermost)
			p.w(", ")
			if dd := p.incDelta(d, d.Type()); dd != 1 {
				p.w("%d*(", dd)
				defer p.w(")")
			}
			p.assignmentExpression(f, n.AssignmentExpression, d.Type(), exprValue, flags|fOutermost)
			p.w(")")
		case d.Type().IsArithmeticType():
			defer p.w("%s", p.convertType(n, d.Type(), t, flags))
			p.w("%sAssign%s%s(&", p.task.crt, oper2, p.helperType(n, d.Type()))
			p.declarator(n, f, d, d.Type(), exprLValue, flags|fOutermost)
			p.w(", ")
			if asInt {
				p.w("int(")
			}
			p.assignmentExpression(f, n.AssignmentExpression, d.Type(), exprValue, flags|fOutermost)
			p.w(")")
			if asInt {
				p.w(")")
			}
		default:
			panic(todo("", pos(n), pos(d), d.Name()))
		}
		//TODO- p.declarator(f, d, d.Type(), exprLValue, flags|fOutermost)
		//TODO- switch d.Type().Kind() {
		//TODO- case cc.Ptr:
		//TODO- 	if oper != "+" && oper != "-" {
		//TODO- 		panic(todo(""))
		//TODO- 	}

		//TODO- 	p.w(" %s= ", oper)
		//TODO- 	if dd := p.incDelta(d, d.Type()); dd != 1 {
		//TODO- 		p.w("%d*(", dd)
		//TODO- 		defer p.w(")")
		//TODO- 	}
		//TODO- 	defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
		//TODO- 	p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
		//TODO- default:
		//TODO- 	p.w(" = ")
		//TODO- 	defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
		//TODO- 	p.declarator(f, d, n.Promote(), exprValue, flags|fOutermost)
		//TODO- 	p.w(" %s (", oper)
		//TODO- 	p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
		//TODO- 	p.w(")")
		//TODO- }
		return
	}

	lhs := n.UnaryExpression
	switch {
	case lhs.Operand.Type().IsArithmeticType():
		defer p.w("%s", p.convertType(n, lhs.Operand.Type(), n.Operand.Type(), flags))
		p.w("%sAssign%sPtr%s(", p.task.crt, oper2, p.helperType(n, lhs.Operand.Type()))
		p.unaryExpression(f, lhs, lhs.Operand.Type(), exprAddrOf, flags|fOutermost)
		p.w(", ")
		if asInt {
			p.w("int(")
		}
		p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), exprValue, flags|fOutermost)
		if asInt {
			p.w(")")
		}
		p.w(")")
	//TODO- case lhs.Operand.Type().Kind() == cc.Ptr:
	//TODO- 	p.w("*(*%s)(unsafe.Pointer(", p.typ(lhs.Operand.Type()))
	//TODO- 	p.unaryExpression(f, lhs, lhs.Operand.Type(), exprAddrOf, flags|fOutermost)
	//TODO- 	p.w(")) %s= (", oper)
	//TODO- 	p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), exprValue, flags|fOutermost)
	//TODO- 	p.w(")")
	//TODO- 	if dd := p.incDelta(n, lhs.Operand.Type()); dd != 1 {
	//TODO- 		p.w("*%d", dd)
	//TODO- 	}
	default:
		panic(todo("", lhs.Operand.Type()))
	}
}

func (p *project) assignOpVoid(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.assignOpVoidNormal(f, n, t, oper, oper2, mode, flags)
	case opBitfield:
		p.assignOpVoidBitfield(f, n, t, oper, oper2, mode, flags)
	case opArrayParameter:
		p.assignOpVoidArrayParameter(f, n, t, oper, oper2, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignOpVoidArrayParameter(f *function, n *cc.AssignmentExpression, t cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	d := n.UnaryExpression.Declarator()
	if local := f.locals[d]; local != nil && local.isPinned {
		panic(todo(""))
	}

	p.declarator(n, f, d, d.Type(), exprLValue, flags|fOutermost)
	if oper != "+" && oper != "-" {
		panic(todo(""))
	}

	p.w(" %s= ", oper)
	if dd := p.incDelta(d, d.Type()); dd != 1 {
		p.w("%d*", dd)
	}
	p.w("uintptr(")
	p.assignmentExpression(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type(), exprValue, flags|fOutermost)
	p.w(")")
}

func (p *project) assignOpVoidBitfield(f *function, n *cc.AssignmentExpression, t cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	lhs := n.UnaryExpression
	lt := lhs.Operand.Type()
	switch lhs.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		pe := n.UnaryExpression.PostfixExpression
		switch pe.Case {
		case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
			switch d := pe.PostfixExpression.Declarator(); {
			case d != nil:
				bf := lt.BitField()
				p.w("%sSetBitFieldPtr%d%s(", p.task.crt, bf.BitFieldBlockWidth(), p.bfHelperType(n.Promote()))
				p.unaryExpression(f, lhs, lt, exprAddrOf, flags)
				p.w(", (")
				s := p.convertType(n, lt, n.Promote(), flags)
				p.unaryExpression(f, lhs, lt, exprValue, flags)
				p.w(")%s %s ", s, oper)
				s = p.convertType(n, lt, n.Promote(), flags)
				p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
				p.w("%s", s)
				p.w(", %d, %#x)", bf.BitFieldOffset(), bf.Mask())
			default:
				panic(todo(""))
			}
		case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
			switch d := pe.PostfixExpression.Declarator(); {
			case d != nil:
				panic(todo(""))
			default:
				panic(todo(""))
			}
		default:
			panic(todo("", n.Position(), pe.Case))
		}
	default:
		panic(todo("", n.Position(), lhs.Case))
	}
}

func (p *project) assignOpVoidNormal(f *function, n *cc.AssignmentExpression, t cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	rop := n.AssignmentExpression.Operand
	if d := n.UnaryExpression.Declarator(); d != nil {
		if local := f.locals[d]; local != nil && local.isPinned {
			p.declarator(n, f, d, d.Type(), exprLValue, flags)
			switch {
			case d.Type().Kind() == cc.Ptr:
				p.w(" %s= ", oper)
				if dd := p.incDelta(d, d.Type()); dd != 1 {
					p.w("%d*(", dd)
					defer p.w(")")
				}
				defer p.w("%s", p.convert(n, rop.ConvertTo(n.Promote()), d.Type(), flags))
				p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
			case d.Type().IsArithmeticType():
				p.w(" %s= ", oper)
				defer p.w("%s", p.convert(n, rop.ConvertTo(n.Promote()), d.Type(), flags))
				p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
			default:
				panic(todo("", n.Position(), d.Type().Kind()))
			}
			return
		}

		p.declarator(n, f, d, d.Type(), exprLValue, flags|fOutermost)
		switch d.Type().Kind() {
		case cc.Ptr:
			if oper != "+" && oper != "-" {
				panic(todo(""))
			}

			p.w(" %s= ", oper)
			if dd := p.incDelta(d, d.Type()); dd != 1 {
				p.w("%d*(", dd)
				defer p.w(")")
			}
			defer p.w("%s", p.convert(n, rop.ConvertTo(n.Promote()), d.Type(), flags))
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
		default:
			p.w(" = ")
			defer p.w("%s", p.convert(n, rop.ConvertTo(n.Promote()), d.Type(), flags))
			p.declarator(n, f, d, n.Promote(), exprValue, flags|fOutermost)
			p.w(" %s (", oper)
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
			p.w(")")
		}
		return
	}

	lhs := n.UnaryExpression
	switch {
	case lhs.Operand.Type().IsArithmeticType():
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, lhs.Operand.Type()))
		p.unaryExpression(f, lhs, lhs.Operand.Type(), exprAddrOf, flags|fOutermost)
		p.w(")) %s= ", oper)
		defer p.w("%s", p.convert(n, rop.ConvertTo(n.Promote()), lhs.Operand.Type(), flags))
		p.w("(")
		p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), exprValue, flags|fOutermost)
		p.w(")")
	case lhs.Operand.Type().Kind() == cc.Ptr:
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, lhs.Operand.Type()))
		p.unaryExpression(f, lhs, lhs.Operand.Type(), exprAddrOf, flags|fOutermost)
		p.w(")) %s= (", oper)
		p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), exprValue, flags|fOutermost)
		p.w(")")
		if dd := p.incDelta(n, lhs.Operand.Type()); dd != 1 {
			p.w("*%d", dd)
		}
	default:
		panic(todo("", lhs.Operand.Type()))
	}
}

func opOverflows(op cc.Operand, t cc.Type) bool {
	a, ok := getIntOperand(op)
	if !ok {
		return false
	}

	return overflows(a, t)
}

func getIntOperand(a cc.Operand) (x *big.Int, ok bool) {
	switch n := a.Value().(type) {
	case cc.Int64Value:
		return big.NewInt(int64(n)), true
	case cc.Uint64Value:
		return big.NewInt(0).SetUint64(uint64(n)), true
	default:
		return nil, false
	}
}

func (p *project) iterationStatement(f *function, n *cc.IterationStatement) {
	sv := f.switchCtx
	sv2 := f.continueCtx
	sv3 := f.breakCtx
	f.switchCtx = 0
	f.continueCtx = 0
	f.breakCtx = 0
	defer func() {
		f.breakCtx = sv3
		f.continueCtx = sv2
		f.switchCtx = sv
	}()
	p.w("%s", tidyComment("\n", n))
	switch n.Case {
	case cc.IterationStatementWhile: // "while" '(' Expression ')' Statement
		if f.hasJumps {
			// a:	if !expr goto b
			//	stmt
			//	goto a
			// b:
			a := f.flatLabel()
			b := f.flatLabel()
			f.continueCtx = a
			f.breakCtx = b
			p.w("__%d: if !(", a)
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
			p.w(") { goto __%d };", b)
			p.statement(f, n.Statement, false, false)
			p.w("; goto __%d; __%d:", a, b)
			break
		}

		p.w("for ")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false)
	case cc.IterationStatementDo: // "do" Statement "while" '(' Expression ')' ';'
		if f.hasJumps {
			// a:	stmt
			// b:	if expr goto a // b is the continue label
			// c:
			a := f.flatLabel()
			b := f.flatLabel()
			c := f.flatLabel()
			f.continueCtx = b
			f.breakCtx = c
			p.w("__%d:", a)
			p.statement(f, n.Statement, false, false)
			p.w(";goto __%d; __%[1]d: if ", b)
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
			p.w("{goto __%d};goto __%d;__%[2]d:", a, c)
			break
		}

		v := f.scope.take("ok")
		p.w("for %v := true; %[1]v; %[1]v = ", v)
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false)
	case cc.IterationStatementFor: // "for" '(' Expression ';' Expression ';' Expression ')' Statement
		if f.hasJumps || n.Expression3 != nil && n.Expression3.Case == cc.ExpressionComma {
			//	expr
			// a:	if !expr2 goto c
			//	stmt
			// b: 	expr3 // label for continue
			//	goto a
			// c:
			a := f.flatLabel()
			b := f.flatLabel()
			f.continueCtx = b
			c := f.flatLabel()
			f.breakCtx = c
			if n.Expression != nil {
				p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, fOutermost|fNoCondAssignment)
			}
			semi := ""
			if n.Expression != nil || n.Expression2 != nil || n.Expression3 != nil {
				semi = ";"
			}
			p.w("%s__%d:", semi, a)
			if n.Expression2 != nil {
				p.w("if !(")
				p.expression(f, n.Expression2, n.Expression2.Operand.Type(), exprBool, fOutermost)
				p.w(") { goto __%d }", c)
			}
			p.w("%s", semi)
			p.statement(f, n.Statement, false, false)
			p.w(";goto __%d; __%[1]d:", b)
			if n.Expression3 != nil {
				p.expression(f, n.Expression3, n.Expression3.Operand.Type(), exprVoid, fOutermost|fNoCondAssignment)
			}
			p.w("%sgoto __%d; goto __%d;__%[3]d:", semi, a, c)
			break
		}

		expr := true
		if n.Expression != nil && n.Expression.Case == cc.ExpressionComma {
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, fOutermost)
			p.w(";")
			expr = false
		}
		p.w("for ")
		if expr && n.Expression != nil {
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, fOutermost|fNoCondAssignment)
		}
		p.w("; ")
		if n.Expression2 != nil {
			p.expression(f, n.Expression2, n.Expression2.Operand.Type(), exprBool, fOutermost)
		}
		p.w("; ")
		if n.Expression3 != nil {
			p.expression(f, n.Expression3, n.Expression3.Operand.Type(), exprVoid, fOutermost|fNoCondAssignment)
		}
		p.statement(f, n.Statement, true, false)
	case cc.IterationStatementForDecl: // "for" '(' Declaration Expression ';' Expression ')' Statement
		if true || f.block.isFlat() {
			panic(todo(""))
		}

		panic(todo("", pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) selectionStatement(f *function, n *cc.SelectionStatement) {
	p.w("%s", tidyComment("\n", n))
	switch n.Case {
	case cc.SelectionStatementIf: // "if" '(' Expression ')' Statement
		sv := f.ifCtx
		f.ifCtx = n
		defer func() { f.ifCtx = sv }()
		if f.hasJumps {
			// if !expr goto a
			// stmt
			// a:
			f.ifCtx = n
			a := f.flatLabel()
			p.w("if !(")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
			p.w(") { goto __%d };", a)
			p.statement(f, n.Statement, false, false)
			p.w(";__%d: ", a)
			break
		}

		p.w("if ")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false)
	case cc.SelectionStatementIfElse: // "if" '(' Expression ')' Statement "else" Statement
		sv := f.ifCtx
		f.ifCtx = n
		defer func() { f.ifCtx = sv }()
		if f.hasJumps {
			// if !expr goto a
			// stmt
			// goto b
			// a:
			// stmt2
			// b:
			a := f.flatLabel()
			b := f.flatLabel()
			p.w("if !(")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
			p.w(") { goto __%d };", a)
			p.statement(f, n.Statement, false, false)
			p.w(";goto __%d; __%d:", b, a)
			p.statement(f, n.Statement2, false, false)
			p.w(";__%d:", b)
			break
		}

		p.w("if ")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false)
		p.w(" else ")
		switch {
		case p.isIfStmt(n.Statement2):
			p.statement(f, n.Statement2, false, true)
		default:
			p.statement(f, n.Statement2, true, false)
		}
	case cc.SelectionStatementSwitch: // "switch" '(' Expression ')' Statement
		sv := f.switchCtx
		svBreakCtx := f.breakCtx
		f.breakCtx = 0
		defer func() {
			f.switchCtx = sv
			f.breakCtx = svBreakCtx
		}()
		if f.hasJumps {
			f.switchCtx = inSwitchFlat
			p.flatSwitch(f, n)
			break
		}

		f.switchCtx = inSwitchFirst
		p.w("switch ")
		p.expression(f, n.Expression, n.Promote(), exprValue, fOutermost)
		p.statement(f, n.Statement, true, false)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) isIfStmt(n *cc.Statement) bool {
	if n.Case != cc.StatementSelection {
		return false
	}

	switch n.SelectionStatement.Case {
	case cc.SelectionStatementIf, cc.SelectionStatementIfElse:
		return true
	}

	return false
}

func (p *project) flatSwitch(f *function, n *cc.SelectionStatement) {
	if n.Statement.Case != cc.StatementCompound {
		panic(todo(""))
	}

	sv := f.block
	f.block = f.blocks[n.Statement.CompoundStatement]
	defer func() { f.block = sv }()
	// "switch" '(' Expression ')' Statement
	cases := n.Cases()
	labels := map[*cc.LabeledStatement]int{}
	svBreakCtx := f.breakCtx
	f.breakCtx = f.flatLabel()
	p.w("switch ")
	p.expression(f, n.Expression, n.Promote(), exprValue, fOutermost)
	p.w("{")
	for _, ls := range cases {
		switch ls.Case {
		case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
			continue
		case cc.LabeledStatementCaseLabel: // "case" ConstantExpression ':' Statement
			p.w("%scase ", tidyComment("\n", ls))
			p.constantExpression(f, ls.ConstantExpression, ls.ConstantExpression.Operand.Type(), exprValue, fOutermost)
			p.w(":")
		case cc.LabeledStatementDefault: // "default" ':' Statement
			p.w("%sdefault:", tidyComment("\n", ls))
		case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
			panic(todo(""))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
		label := f.flatLabel()
		labels[ls] = label
		p.w("goto __%d;", label)
	}
	p.w("}; goto __%d;", f.breakCtx)
	svLabels := f.flatSwitchLabels
	f.flatSwitchLabels = labels
	p.statement(f, n.Statement, false, true)
	f.flatSwitchLabels = svLabels
	p.w("__%d:", f.breakCtx)
	f.breakCtx = svBreakCtx
}

func isJumpTarget(n *cc.Statement) bool {
	return n.Case == cc.StatementLabeled && n.LabeledStatement.Case == cc.LabeledStatementLabel ||
		n.Case == cc.StatementCompound && n.CompoundStatement.IsJumpTarget()
}

func (p *project) expressionStatement(f *function, n *cc.ExpressionStatement) {
	p.w("%s", tidyComment("\n", n))
	// Expression AttributeSpecifierList ';'
	if n.Expression == nil || n.Expression.IsSideEffectsFree {
		return
	}

	p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, fOutermost)
}

func (p *project) labeledStatement(f *function, n *cc.LabeledStatement) (r *cc.JumpStatement) {
	if f.hasJumps { //TODO merge with ...Flat below
		return p.labeledStatementFlat(f, n)
	}

	switch n.Case {
	case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
		if _, ok := f.unusedLabels[n.Token.Value]; ok {
			p.w("goto %s;", f.labelNames[n.Token.Value])
		}
		p.w("%s%s:", comment("\n", n), f.labelNames[n.Token.Value])
		r = p.statement(f, n.Statement, false, false)
	case
		cc.LabeledStatementCaseLabel, // "case" ConstantExpression ':' Statement
		cc.LabeledStatementDefault:   // "default" ':' Statement

		p.labeledStatementCase(f, n)
	case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
		panic(todo("", n.Position(), n.Case))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) labeledStatementFlat(f *function, n *cc.LabeledStatement) (r *cc.JumpStatement) {
	switch n.Case {
	case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
		if _, ok := f.unusedLabels[n.Token.Value]; ok {
			p.w("goto %s;", f.labelNames[n.Token.Value])
		}
		p.w("%s%s:", tidyComment("\n", n), f.labelNames[n.Token.Value])
		r = p.statement(f, n.Statement, false, false)
	case
		cc.LabeledStatementCaseLabel, // "case" ConstantExpression ':' Statement
		cc.LabeledStatementDefault:   // "default" ':' Statement

		p.labeledStatementCase(f, n)
	case cc.LabeledStatementRange: // "case" ConstantExpression "..." ConstantExpression ':' Statement
		panic(todo("", n.Position(), n.Case))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) labeledStatementCase(f *function, n *cc.LabeledStatement) {
	switch f.switchCtx {
	case inSwitchFirst:
		f.switchCtx = inSwitchCase
	case inSwitchCase:
		p.w("\nfallthrough;")
	case inSwitchSeenBreak:
		f.switchCtx = inSwitchCase
	case inSwitchFlat:
		// ok
	default:
		panic(todo("", n.Position(), f.switchCtx))
	}
	switch n.Case {
	case cc.LabeledStatementCaseLabel: // "case" ConstantExpression ':' Statement
		switch {
		case f.switchCtx == inSwitchFlat:
			p.w("%s__%d:", tidyComment("\n", n), f.flatSwitchLabels[n])
		default:
			p.w("%scase ", tidyComment("\n", n))
			p.constantExpression(f, n.ConstantExpression, n.ConstantExpression.Operand.Type(), exprValue, fOutermost)
			p.w(":")
		}
	case cc.LabeledStatementDefault: // "default" ':' Statement
		switch {
		case f.switchCtx == inSwitchFlat:
			p.w("%s__%d:", tidyComment("\n", n), f.flatSwitchLabels[n])
		default:
			p.w("%sdefault:", tidyComment("\n", n))
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	p.statement(f, n.Statement, false, false)
}

func (p *project) constantExpression(f *function, n *cc.ConstantExpression, t cc.Type, mode exprMode, flags flags) {
	// ConditionalExpression
	p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
}

func (p *project) functionDefinitionSignature(f *function, tld *tld) {
	switch {
	case f.mainSignatureForced:
		p.w("%sfunc %s(%s *%sTLS, _ int32, _ uintptr) int32", tidyComment("\n", f.fndef), tld.name, f.tlsName, p.task.crt)
	default:
		p.w("%s", tidyComment("\n", f.fndef))
		p.functionSignature(f, f.fndef.Declarator.Type(), tld.name)
	}
}

func (p *project) functionSignature(f *function, t cc.Type, nm string) {
	p.w("func")
	if nm != "" {
		p.w(" %s", nm)
	}
	switch {
	case f == nil || nm == "":
		p.w("(*%sTLS", p.task.crt)
	default:
		p.w("(%s *%sTLS", f.tlsName, p.task.crt)
	}
	for _, v := range t.Parameters() {
		if v.Type().Kind() == cc.Void {
			break
		}

		var pn string
		if f != nil && nm != "" {
			pn = "_"
			if d := v.Declarator(); d != nil {
				if local := f.locals[d]; local != nil {
					pn = local.name
				}
			}
		}
		p.w(", %s %s", pn, p.paramTyp(v.Type()))
	}
	if t.IsVariadic() {
		switch {
		case f == nil || nm == "":
			p.w(", uintptr")
		default:
			p.w(", %s uintptr", f.vaName)
		}
	}
	p.w(")")
	if rt := t.Result(); rt != nil && rt.Kind() != cc.Void {
		p.w(" %s", p.typ(nil, rt))
	}
}

func (p *project) paramTyp(t cc.Type) string {
	if t.Kind() == cc.Array {
		return "uintptr"
	}

	return p.typ(nil, t)
}
