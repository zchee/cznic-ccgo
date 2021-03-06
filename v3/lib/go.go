// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v3/lib"

import (
	"bytes"
	"fmt"
	"go/scanner"
	"go/token"
	"hash/maphash"
	"io/ioutil"
	"math"
	"math/big"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"modernc.org/cc/v3"
	"modernc.org/mathutil"
)

var (
	idAddOverflow          = cc.String("__builtin_add_overflow") // bool __builtin_add_overflow (type1 a, type2 b, type3 *res)
	idAligned              = cc.String("aligned")                // int __attribute__ ((aligned (8))) foo;
	idAtomicLoadN          = cc.String("__atomic_load_n")        // type __atomic_load_n (type *ptr, int memorder)
	idAtomicStoreN         = cc.String("__atomic_store_n")       // void __atomic_store_n (type *ptr, type val, int memorder)
	idBp                   = cc.String("bp")
	idBuiltinConstantPImpl = cc.String("__builtin_constant_p_impl")
	idCAPI                 = cc.String("CAPI")
	idChooseExpr           = cc.String("__builtin_choose_expr")
	idEnviron              = cc.String("environ")
	idMain                 = cc.String("main")
	idMulOverflow          = cc.String("__builtin_mul_overflow") // bool __builtin_mul_overflow (type1 a, type2 b, type3 *res)
	idPacked               = cc.String("packed")                 // __attribute__((packed))
	idSubOverflow          = cc.String("__builtin_sub_overflow") // bool __builtin_sub_overflow (type1 a, type2 b, type3 *res)
	idTls                  = cc.String("tls")
	idTs                   = cc.String("ts")
	idVa                   = cc.String("va")
	idVaArg                = cc.String("__ccgo_va_arg")
	idVaEnd                = cc.String("__ccgo_va_end")
	idVaList               = cc.String("va_list")
	idVaStart              = cc.String("__ccgo_va_start")
	idWtext                = cc.String("wtext")

	bytesBufferPool = sync.Pool{New: func() interface{} { return &bytes.Buffer{} }}

	oTraceG bool
	oTraceW bool
)

type exprMode int

const (
	doNotExport = iota
	exportCapitalize
	exportPrefix
)

const (
	_              exprMode = iota
	exprAddrOf              // &foo as uinptr (must be static/pinned)
	exprBool                // foo in foo != 0
	exprCondInit            // foo or bar in int i = x ? foo : bar;
	exprCondReturn          // foo or bar in return x ? foo : bar;
	exprDecay               // &foo[0] in foo for array foo.
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
	fForceNoConv
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
	node  cc.Node

	conflicts bool
	emitted   bool
}

func (s *taggedStruct) emit(p *project, ds *cc.DeclarationSpecifiers) {
	if s == nil || s.emitted {
		return
	}

	s.emitted = true
	p.w("%stype %s = %s; /* %v */\n\n", tidyComment("\n", ds), s.name, s.gotyp, p.pos(s.node))
}

// Return first non empty token separator within n or dflt otherwise.
func comment(dflt string, n cc.Node) string {
	if s := tokenSeparator(n); s != "" {
		return s
	}

	return dflt
}

// tidyComment is like comment but makes comment more Go-like.
func tidyComment(dflt string, n cc.Node) (r string) { return tidyCommentString(comment(dflt, n)) }

func tidyCommentString(s string) (r string) {
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

	s = strings.ReplaceAll(s, "\f", "")
	b := bytesBufferPool.Get().(*bytes.Buffer)
	defer func() { b.Reset(); bytesBufferPool.Put(b) }()
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
			b2 := bytesBufferPool.Get().(*bytes.Buffer)
			defer func() { b2.Reset(); bytesBufferPool.Put(b2) }()
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
					fmt.Fprintf(b, "//%s\n", s2)
					break
				}

				fmt.Fprintf(b, "/*%s*/", s2)
				break
			}

			if !nl {
				fmt.Fprintf(b, "/*%s*/", s2)
				break
			}

			// Block comment followed by a newline can be safely replaced by a sequence of
			// line comments.  Try to enhance the comment.
			if commentForm1(b, a) ||
				commentForm2(b, a) ||
				commentForm3(b, a) {
				break
			}

			// No enhancement posibilities detected, use the default form.
			if a[len(a)-1] == "" {
				a = a[:len(a)-1]
			}
			fmt.Fprintf(b, "//%s", a[0])
			for _, v := range a[1:] {
				fmt.Fprintf(b, "\n// %s", v)
			}
		default:
			b.WriteByte(c)
			b.WriteByte(c2)
		}
	}
	return b.String()
}

func commentForm1(b *bytes.Buffer, a []string) bool {
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

func commentForm2(b *bytes.Buffer, a []string) bool {
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

func commentForm3(b *bytes.Buffer, a []string) bool {
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
	return tok.Sep.String()
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
	block  *cc.CompoundStatement
	decls  []*cc.Declaration // What to declare in this block.
	params []*cc.Parameter
	parent *block
	scope  scope

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
	f.tlsName = f.scope.take(idTls)
	if t.IsVariadic() {
		f.vaName = f.scope.take(idVa)
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
				block.scope.take(cc.String(tld.name))
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
	if f.project.pass1 {
		return 1
	}

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
		f.labelNames[id] = f.labels.take(id)
	}
}

func (f *function) staticAllocsAndPinned(n *cc.CompoundStatement) {
	for _, v := range f.params {
		switch {
		case v.Type().Kind() == cc.Array && v.Type().IsVLA():
			f.project.err(f.fndef, "variable length arrays not supported")
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
		}

		x, ok := n.(*cc.PostfixExpression)
		if !ok || x.Case != cc.PostfixExpressionCall {
			return true
		}

		if x.PostfixExpression == nil || x.PostfixExpression.Operand == nil || x.PostfixExpression.Operand.Type() == nil {
			return true
		}

		ft := funcType(x.PostfixExpression.Operand.Type())
		if ft.Kind() != cc.Function {
			return true
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
				panic(todo("", f.project.pos(x), t, t.Kind()))
			}
		}
		if need != 0 {
			if f.project.task.mingw {
				need += 8 // On windows the va list is prefixed with its length
			}
			va := roundup(f.off, 8)
			f.vaLists[x] = va
			f.off = va + need
		}
		return true
	})
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
			panic(todo("", p.pos(x), x.Case))
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
			panic(todo("", p.pos(x), x.Case))
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
			panic(todo("", p.pos(x), x.Case))
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
			panic(todo("", p.pos(x), x.Case))
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
			panic(todo("", p.pos(x), x.Case))
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
			panic(todo("", p.pos(x), x.Case))
		}
	case *cc.Expression:
		switch x.Case {
		case cc.ExpressionAssign: // AssignmentExpression
			return p.detectArray(f, x.AssignmentExpression, pinnedOk, recursiveOk, out)
		case cc.ExpressionComma: // Expression ',' AssignmentExpression
			return p.detectArray(f, x.Expression, pinnedOk, recursiveOk, out) || p.detectArray(f, x.AssignmentExpression, pinnedOk, recursiveOk, out)
		default:
			panic(todo("", p.pos(x), x.Case))
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

// Return n's position with path reduced to baseName(path) unless
// p.task.fullPathComments is true.
func (p *project) pos(n cc.Node) (r token.Position) {
	if n == nil {
		return r
	}

	r = token.Position(n.Position())
	if r.IsValid() && !p.task.fullPathComments {
		r.Filename = filepath.Base(r.Filename)
	}
	return r
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

func roundup(n, to uintptr) uintptr {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
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
	for _, decl := range block.decls {
		ds := decl.DeclarationSpecifiers
		for list := decl.InitDeclaratorList; list != nil; list = list.InitDeclaratorList {
			work = append(work, item{ds, list.InitDeclarator.Declarator})
		}
	}
	block.scope.take(cc.String(f.tlsName))
	if f.vaName != "" {
		block.scope.take(cc.String(f.vaName))
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
		local.name = block.scope.take(d.Name())
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

func (n *enumSpec) emit(p *project) {
	if n == nil || p.pass1 || n.emitted {
		return
	}

	n.emitted = true
	ok := false
	for list := n.spec.EnumeratorList; list != nil; list = list.EnumeratorList {
		nm := list.Enumerator.Token.Value
		if _, ok2 := p.emitedEnums[nm]; !ok2 && p.enumConsts[nm] != "" {
			ok = true
			break
		}
	}
	if !ok {
		return
	}

	p.w("%s", tidyComment("\n", n.decl))
	p.w("const ( /* %v: */", p.pos(n.decl))
	for list := n.spec.EnumeratorList; list != nil; list = list.EnumeratorList {
		en := list.Enumerator
		nm := en.Token.Value
		if _, ok := p.emitedEnums[nm]; ok || p.enumConsts[nm] == "" {
			continue
		}

		p.emitedEnums[nm] = struct{}{}
		p.w("%s%s = ", tidyComment("\n", en), p.enumConsts[nm])
		p.intConst(en, "", en.Operand, en.Operand.Type(), fOutermost|fForceNoConv)
		p.w(";")
	}
	p.w(");")
}

type typedef struct {
	sig uint64
	tld *tld
}

type project struct {
	ast                *cc.AST
	buf                bytes.Buffer
	capi               []string
	defines            []string
	emitedEnums        map[cc.StringID]struct{}
	enumConsts         map[cc.StringID]string
	enumSpecs          map[*cc.EnumSpecifier]*enumSpec
	errors             scanner.ErrorList
	externs            map[cc.StringID]*tld
	fn                 string
	imports            map[string]*imported // C name: import info
	intType            cc.Type
	localTaggedStructs []func()
	mainName           string
	scope              scope
	sharedFns          map[*cc.FunctionDefinition]struct{}
	sharedFnsEmitted   map[*cc.FunctionDefinition]struct{}
	staticQueue        []*cc.InitDeclarator
	structs            map[cc.StringID]*taggedStruct // key: C tag
	symtab             map[string]interface{}        // *tld or *imported
	task               *Task
	tlds               map[*cc.Declarator]*tld
	ts                 bytes.Buffer // Text segment
	tsW                []rune       // Text segment, wcha_t
	tsWName            string
	tsWNameP           string
	tsWOffs            map[cc.StringID]uintptr
	tsName             string
	tsNameP            string
	tsOffs             map[cc.StringID]uintptr
	typeSigHash        maphash.Hash
	typedefTypes       map[cc.StringID]*typedef
	typedefsEmited     map[string]struct{}
	verifyStructs      map[string]cc.Type
	wanted             map[*cc.Declarator]struct{}
	wcharSize          uintptr

	isMain bool
	pass1  bool
}

func newProject(t *Task) (*project, error) {
	intType := t.cfg.ABI.Type(cc.Int)
	if intType.Size() != 4 { // We're assuming wchar_t is int32.
		return nil, fmt.Errorf("unsupported C int size: %d", intType.Size())
	}

	if n := t.cfg.ABI.Types[cc.UChar].Size; n != 1 {
		return nil, fmt.Errorf("unsupported C unsigned char size: %d", n)
	}

	if n := t.cfg.ABI.Types[cc.UShort].Size; n != 2 {
		return nil, fmt.Errorf("unsupported C unsigned short size: %d", n)
	}

	if n := t.cfg.ABI.Types[cc.UInt].Size; n != 4 {
		return nil, fmt.Errorf("unsupported C unsigned int size: %d", n)
	}

	if n := t.cfg.ABI.Types[cc.ULongLong].Size; n != 8 {
		return nil, fmt.Errorf("unsupported C unsigned long long size: %d", n)
	}

	p := &project{
		emitedEnums:      map[cc.StringID]struct{}{},
		enumConsts:       map[cc.StringID]string{},
		enumSpecs:        map[*cc.EnumSpecifier]*enumSpec{},
		externs:          map[cc.StringID]*tld{},
		imports:          map[string]*imported{},
		intType:          intType,
		scope:            newScope(),
		sharedFns:        t.cfg.SharedFunctionDefinitions.M,
		sharedFnsEmitted: map[*cc.FunctionDefinition]struct{}{},
		symtab:           map[string]interface{}{},
		task:             t,
		tlds:             map[*cc.Declarator]*tld{},
		tsWOffs:          map[cc.StringID]uintptr{},
		tsOffs:           map[cc.StringID]uintptr{},
		typedefTypes:     map[cc.StringID]*typedef{},
		typedefsEmited:   map[string]struct{}{},
		verifyStructs:    map[string]cc.Type{},
		wanted:           map[*cc.Declarator]struct{}{},
		wcharSize:        t.asts[0].WideCharType.Size(),
	}
	p.scope.take(idCAPI)
	for _, v := range t.imported {
		var err error
		if v.name, v.exports, err = t.capi(v.path); err != nil {
			return nil, err
		}

		v.qualifier = p.scope.take(cc.String(v.name)) + "."
		for k := range v.exports {
			if p.imports[k] == nil {
				p.imports[k] = v
			}
		}
	}
	p.tsNameP = p.scope.take(idTs)
	p.tsName = p.scope.take(idTs)
	p.tsWNameP = p.scope.take(idWtext)
	p.tsWName = p.scope.take(idWtext)
	if err := p.layout(); err != nil {
		return nil, err
	}

	return p, nil
}

func (p *project) newScope() scope {
	s := newScope()
	var a []cc.StringID
	for k := range p.structs {
		a = append(a, k)
	}
	sort.Slice(a, func(i, j int) bool { return a[i].String() < a[j].String() })
	for _, k := range a {
		s.take(cc.String(p.structs[k].name))
	}
	return s
}

func (p *project) err(n cc.Node, s string, args ...interface{}) {
	if p.task.errTrace {
		s = s + "(" + origin(2) + ")"
	}
	if p.task.traceTranslationUnits {
		trc("%v: %s (%v)", n.Position(), fmt.Sprintf(s, args...), origin(2))
	}
	if !p.task.allErrors && len(p.errors) >= 10 {
		return
	}

	switch {
	case n == nil:
		p.errors.Add(token.Position{}, fmt.Sprintf(s, args...))
	default:
		p.errors.Add(token.Position(n.Position()), fmt.Sprintf(s, args...))
		if !p.task.allErrors && len(p.errors) == 10 {
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
	if p.pass1 {
		return
	}

	if oTraceW {
		fmt.Printf(s, args...)
	}
	//fmt.Fprintf(&p.buf, "/* %s */", origin(2)) //TODO-
	fmt.Fprintf(&p.buf, s, args...)
}

func (p *project) layout() error {
	if err := p.layoutTLDs(); err != nil {
		return err
	}

	if err := p.layoutSymtab(); err != nil {
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

func (p *project) layoutSymtab() error {
	var t0 time.Time
	if p.task.traceTranslationUnits {
		fmt.Printf("processing symbol table ... ")
		t0 = time.Now()
		defer func() { fmt.Println(time.Since(t0)) }()
	}

	for _, i := range p.task.symSearchOrder {
		switch {
		case i < 0:
			imported := p.task.imported[-i-1]
			for nm := range imported.exports {
				if _, ok := p.symtab[nm]; !ok {
					p.symtab[nm] = imported
				}
			}
		default:
			ast := p.task.asts[i]
			for d := range ast.TLD {
				if d.IsFunctionPrototype() || d.Linkage != cc.External {
					continue
				}

				nm := d.Name()
				name := nm.String()
				if _, ok := p.symtab[name]; !ok {
					tld := p.externs[nm]
					if tld == nil {
						if d.Type().Kind() != cc.Function && !p.task.header {
							p.err(d, "back-end: undefined: %s %v %v", d.Name(), d.Type(), d.Type().Kind())
						}
						continue
					}

					p.symtab[name] = tld
				}
			}
		}
	}
	return nil
}

func (p *project) layoutDefines() error {
	if !p.task.exportDefinesValid {
		return nil
	}

	var t0 time.Time
	if p.task.traceTranslationUnits {
		fmt.Printf("processing #defines ... ")
		t0 = time.Now()
		defer func() { fmt.Println(time.Since(t0)) }()
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
			name = p.scope.take(cc.String(name))
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
		src = strings.TrimRight(src, "lLuU")
		if _, err := strconv.ParseUint(src, 0, 64); err == nil {
			return neg + src
		}

		src = strings.TrimRight(src, "fF")
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
		return fmt.Sprintf("%d", int64(x))
	case cc.Uint64Value:
		return fmt.Sprintf("%d", uint64(x))
	default:
		panic(todo("", pos(m)))
	}
}

func (p *project) layoutEnums() error {
	var t0 time.Time
	if p.task.traceTranslationUnits {
		fmt.Printf("processing enum values ... ")
		t0 = time.Now()
		defer func() { fmt.Println(time.Since(t0)) }()
	}

	var export int
	if p.task.exportEnumsValid {
		switch {
		case p.task.exportEnums != "":
			export = exportPrefix
		default:
			export = exportCapitalize
		}
	}

	var enumList []*cc.EnumSpecifier
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

				if _, ok := p.enumSpecs[x]; !ok {
					enumList = append(enumList, x)
					p.enumSpecs[x] = &enumSpec{decl: decl.Declaration, spec: x}
				}
				return true
			})
		}
	}

	vals := map[cc.StringID]interface{}{}
	for _, v := range enumList {
		for list := v.EnumeratorList; list != nil; list = list.EnumeratorList {
			en := list.Enumerator
			nm := en.Token.Value
			var val int64
			switch x := en.Operand.Value().(type) {
			case cc.Int64Value:
				val = int64(x)
			case cc.Uint64Value:
				val = int64(x)
			default:
				panic(todo(""))
			}
			switch ex, ok := vals[nm]; {
			case ok:
				switch {
				case ex == nil: //
					continue
				case ex == val: // same name and same value
					continue
				default: // same name, different value
					vals[nm] = nil
				}
			default:
				vals[nm] = val
			}
			p.enumConsts[nm] = ""
		}
	}
	var a []cc.StringID
	for nm := range p.enumConsts {
		if val, ok := vals[nm]; ok && val == nil {
			delete(p.enumConsts, nm)
			continue
		}

		a = append(a, nm)
	}
	sort.Slice(a, func(i, j int) bool { return a[i].String() < a[j].String() })
	for _, nm := range a {
		name := nm.String()
		switch export {
		case doNotExport:
			// nop
		case exportCapitalize:
			name = capitalize(name)
		case exportPrefix:
			name = p.task.exportEnums + name
		}
		name = p.scope.take(cc.String(name))
		p.enumConsts[nm] = name
	}
	return nil
}

func (p *project) layoutStaticLocals() error {
	var t0 time.Time
	if p.task.traceTranslationUnits {
		fmt.Printf("processing static local declarations ... ")
		t0 = time.Now()
		defer func() { fmt.Println(time.Since(t0)) }()
	}
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

					p.tlds[x] = &tld{name: p.scope.take(x.Name())}
				}
				return true
			})
		}
	}
	return nil
}

func (p *project) layoutStructs() error {
	var t0 time.Time
	if p.task.traceTranslationUnits {
		fmt.Printf("processing struct/union types ... ")
		t0 = time.Now()
		defer func() { fmt.Println(time.Since(t0)) }()
	}

	var export int
	if p.task.exportStructsValid {
		switch {
		case p.task.exportStructs != "":
			export = exportPrefix
		default:
			export = exportCapitalize
		}
	}
	m := map[cc.StringID]*taggedStruct{}
	var tags []cc.StringID
	for _, v := range p.task.asts {
		cc.Inspect(v.TranslationUnit, func(n cc.Node, entry bool) bool {
			if entry {
				switch x := n.(type) {
				case *cc.Declarator:
					if nm := x.Name().String(); strings.HasPrefix(nm, "_") {
						break
					}

					p.captureStructTags(x, x.Type(), m, &tags)
				case *cc.Declaration:
					cc.Inspect(x.DeclarationSpecifiers, func(nn cc.Node, entry bool) bool {
						switch y := nn.(type) {
						case *cc.StructOrUnionSpecifier:
							if tag := y.Token.Value; tag != 0 {
								p.captureStructTags(y, y.Type(), m, &tags)
							}
						}
						return true
					})
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

		name := k.String()
		switch export {
		case exportCapitalize:
			name = capitalize(name)
		case exportPrefix:
			name = p.task.exportExterns + name
		}
		v.name = p.scope.take(cc.String(name))
	}
	for _, k := range tags {
		v := m[k]
		if v != nil {
			v.gotyp = p.structType(nil, v.ctyp)
		}
	}
	p.structs = m
	return nil
}

func (p *project) captureStructTags(n cc.Node, t cc.Type, m map[cc.StringID]*taggedStruct, tags *[]cc.StringID) {
	if t == nil {
		return
	}

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
			ts := p.typeSignature(n, t)
			exs := p.typeSignature(n, ex.ctyp)
			if ts != exs {
				ex.conflicts = true
			}
			return
		}

		nf := t.NumField()
		m[tag] = &taggedStruct{ctyp: t, node: n}
		for idx := []int{0}; idx[0] < nf; idx[0]++ {
			p.captureStructTags(n, t.FieldByIndex(idx).Type(), m, tags)
		}
		*tags = append(*tags, tag)
	case cc.Array:
		p.captureStructTags(n, t.Elem(), m, tags)
	}
}

func (p *project) typeSignature(n cc.Node, t cc.Type) (r uint64) {
	p.typeSigHash.Reset()
	p.typeSignature2(n, &p.typeSigHash, t)
	return p.typeSigHash.Sum64()
}

func (p *project) typeSignature2(n cc.Node, b *maphash.Hash, t cc.Type) {
	t = t.Alias()
	if t.IsIntegerType() {
		if !t.IsSignedType() {
			b.WriteByte('u')
		}
		fmt.Fprintf(b, "int%d", t.Size()*8)
		return
	}

	if t.IsArithmeticType() {
		b.WriteString(t.Kind().String())
		return
	}

	structOrUnion := "struct"
	switch t.Kind() {
	case cc.Ptr:
		fmt.Fprintf(b, "*%s", t.Elem())
	case cc.Array:
		if t.IsVLA() {
			p.err(n, "unsupported C type: %v", t)
		}

		fmt.Fprintf(b, "[%d]%s", t.Len(), t.Elem())
	case cc.Vector:
		fmt.Fprintf(b, "[%d]%s", t.Len(), t.Elem())
	case cc.Union:
		structOrUnion = "union"
		fallthrough
	case cc.Struct:
		b.WriteString(structOrUnion)
		nf := t.NumField()
		fmt.Fprintf(b, " %d{", nf)
		b.WriteByte('{')
		for idx := []int{0}; idx[0] < nf; idx[0]++ {
			f := t.FieldByIndex(idx)
			fmt.Fprintf(b, "%s:%d:%d:%v:%d:%d:",
				f.Name(), f.BitFieldOffset(), f.BitFieldWidth(), f.IsBitField(), f.Offset(), f.Padding(),
			)
			p.typeSignature2(f.Declarator(), b, f.Type())
			b.WriteByte(';')
		}
		b.WriteByte('}')
	case cc.Void:
		b.WriteString("void")
	case cc.Invalid:
		b.WriteString("invalid") //TODO fix cc/v3
	default:
		panic(todo("", p.pos(n), t, t.Kind()))
	}
}

func (p *project) structType(n cc.Node, t cc.Type) string {
	switch t.Kind() {
	case cc.Struct, cc.Union:
		tag := t.Tag()
		if tag != 0 && p.structs != nil {
			s := p.structs[tag]
			if s == nil {
				return p.structLiteral(n, t)
			}

			if s.gotyp == "" {
				s.gotyp = p.structLiteral(n, t)
			}
			return s.gotyp
		}

		return p.structLiteral(n, t)
	default:
		panic(todo("internal error: %v", t.Kind()))
	}
}

func (p *project) structLiteral(n cc.Node, t cc.Type) string {
	b := bytesBufferPool.Get().(*bytes.Buffer)
	defer func() { b.Reset(); bytesBufferPool.Put(b) }()
	switch t.Kind() {
	case cc.Struct:
		info := p.structLayout(n, t)
		// trc("%v:\n%s", p.pos(n), dumpLayout(t, info))
		b.WriteString("struct {")
		if info.forceAlign {
			fmt.Fprintf(b, "_[0]uint%d;", 8*t.Align())
		}
		var max uintptr
		for _, off := range info.offs {
			flds := info.flds[off]
			if off < max {
				var a []string
				var nmf cc.Field
				for _, f := range flds {
					if f.Name() != 0 && nmf == nil {
						nmf = f
					}
					if !f.IsBitField() {
						panic(todo("internal error %q, off %v max %v\n%s", f.Name(), off, max, dumpLayout(t, info)))
					}
					a = append(a, fmt.Sprintf("%s %s: %d", f.Type(), f.Name(), f.BitFieldWidth()))
				}
				fmt.Fprintf(b, "/* %s */", strings.Join(a, ", "))
				continue
			}

			f := flds[0]
			switch pad := info.padBefore[f]; {
			case pad < 0:
				continue
			case pad > 0:
				fmt.Fprintf(b, "_ [%d]byte;", pad)
			}
			switch {
			case f.IsBitField():
				max += uintptr(f.BitFieldBlockWidth()) >> 3
				var a []string
				var nmf cc.Field
				for _, f := range flds {
					if f.Name() != 0 && nmf == nil {
						nmf = f
					}
					if !f.IsBitField() {
						panic(todo("internal error %q\n%s", f.Name(), dumpLayout(t, info)))
					}
					a = append(a, fmt.Sprintf("%s %s: %d", f.Type(), f.Name(), f.BitFieldWidth()))
				}
				if nmf == nil {
					nmf = f
				}
				fmt.Fprintf(b, "%s uint%d /* %s */;", p.bitFieldName(n, nmf), f.BitFieldBlockWidth(), strings.Join(a, ", "))
			default:
				if t := f.Type(); t.Kind() == cc.Array && t.IsIncomplete() || t.Size() == 0 {
					break
				}

				max += f.Type().Size()
				fmt.Fprintf(b, "%s %s;", p.fieldName2(n, f), p.typ(nil, f.Type()))
			}
		}
		if info.padAfter != 0 {
			fmt.Fprintf(b, "_ [%d]byte;", info.padAfter)
		}
		b.WriteByte('}')
	case cc.Union:
		b.WriteString("struct {")
		al := uintptr(t.Align())
		sz := t.Size()
		if al > sz {
			panic(todo("", p.pos(n)))
		}

		f := t.FieldByIndex([]int{0})
		ft := f.Type()
		al0 := ft.Align()
		if f.IsBitField() {
			al0 = f.BitFieldBlockWidth() >> 3
		}
		if al != uintptr(al0) {
			fmt.Fprintf(b, "_ [0]uint%d;", 8*al)
		}
		fsz := ft.Size()
		switch {
		case f.IsBitField():
			fmt.Fprintf(b, "%s ", p.fieldName2(n, f))
			fmt.Fprintf(b, "uint%d;", f.BitFieldBlockWidth())
			fsz = uintptr(f.BitFieldBlockWidth()) >> 3
		default:
			fmt.Fprintf(b, "%s %s;", p.fieldName2(n, f), p.typ(nil, ft))
		}
		if pad := sz - fsz; pad != 0 {
			fmt.Fprintf(b, "_ [%d]byte;", pad)
		}
		b.WriteByte('}')
	default:
		panic(todo("internal error: %v", t.Kind()))
	}
	r := b.String()
	if p.task.verifyStructs {
		if _, ok := p.verifyStructs[r]; !ok {
			p.verifyStructs[r] = t
		}
	}
	return r
}

type structInfo struct {
	offs      []uintptr
	flds      map[uintptr][]cc.Field
	padBefore map[cc.Field]int
	padAfter  int

	forceAlign bool
}

func (p *project) structLayout(n cc.Node, t cc.Type) *structInfo {
	nf := t.NumField()
	flds := map[uintptr][]cc.Field{}
	var maxAlign int
	for idx := []int{0}; idx[0] < nf; idx[0]++ {
		f := t.FieldByIndex(idx)
		if f.IsBitField() && f.BitFieldWidth() == 0 {
			continue
		}

		if a := f.Type().Align(); !f.IsBitField() && a > maxAlign {
			maxAlign = a
		}
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
	var forceAlign bool
	for _, off := range offs {
		f := flds[off][0]
		ft := f.Type()
		//trc("%q off %d pos %d %v %v %v", f.Name(), off, pos, ft, ft.Kind(), ft.IsIncomplete())
		switch {
		case ft.IsBitFieldType():
			if f.BitFieldOffset() != 0 {
				break
			}

			if p := int(off - pos); p != 0 {
				if pads == nil {
					pads = map[cc.Field]int{}
				}
				pads[f] = p
				pos = off
			}
			pos += uintptr(f.BitFieldBlockWidth()) >> 3
		default:
			var sz uintptr
			switch {
			case ft.Kind() != cc.Array || ft.Len() != 0:
				sz = ft.Size()
			default:
				forceAlign = true
			}
			if p := int(off - pos); p != 0 {
				if pads == nil {
					pads = map[cc.Field]int{}
				}
				pads[f] = p
				pos = off
			}
			pos += sz
		}
	}
	return &structInfo{
		offs:       offs,
		flds:       flds,
		padBefore:  pads,
		padAfter:   int(t.Size() - pos),
		forceAlign: forceAlign || maxAlign < t.Align(),
	}
}

func (p *project) bitFieldName(n cc.Node, f cc.Field) string {
	if id := f.Name(); id != 0 {
		return p.fieldName(n, id)
	}

	return fmt.Sprintf("__%d", f.Offset())
}

func (p *project) fieldName2(n cc.Node, f cc.Field) string {
	if f.Name() != 0 {
		return p.fieldName(n, f.Name())
	}

	return p.fieldName(n, cc.String(fmt.Sprintf("__%d", f.Offset())))
}

func (p *project) fieldName(n cc.Node, id cc.StringID) string {
	if id == 0 {
		panic(todo("", p.pos(n)))
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

func (p *project) typ(nd cc.Node, t cc.Type) (r string) {
	if t.IsIncomplete() {
		panic(todo("", p.pos(nd), t))
	}

	if t.IsAliasType() {
		if tld := p.tlds[t.AliasDeclarator()]; tld != nil {
			return tld.name
		}
	}

	b := bytesBufferPool.Get().(*bytes.Buffer)
	defer func() { b.Reset(); bytesBufferPool.Put(b) }()
	if t.IsIntegerType() {
		if !t.IsSignedType() {
			b.WriteByte('u')
		}
		if t.Size() > 8 {
			p.err(nd, "unsupported C type: %v", t)
		}
		fmt.Fprintf(b, "int%d", 8*t.Size())
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
		if t.IsVLA() {
			p.err(nd, "unsupported C type: %v", t)
		}
		fmt.Fprintf(b, "[%d]%s", n, p.typ(nd, t.Elem()))
		return b.String()
	case cc.Vector:
		n := t.Len()
		fmt.Fprintf(b, "[%d]%s", n, p.typ(nd, t.Elem()))
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

		return p.structType(nd, t)
	}

	panic(todo("", p.pos(nd), t.Kind(), t))
}

func (p *project) layoutTLDs() error {
	var t0 time.Time
	if p.task.traceTranslationUnits {
		fmt.Printf("processing file scope declarations ... ")
		t0 = time.Now()
		defer func() { fmt.Println(time.Since(t0)) }()
	}

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
						p.scope.take(idMain)
						break out
					}
				}
			}
		}
	}
	sharedFns := map[*cc.FunctionDefinition]struct{}{}
	for _, ast := range p.task.asts {
		a = a[:0]
		for d := range ast.TLD {
			if d.IsFunctionPrototype() {
				continue
			}

			// https://gcc.gnu.org/onlinedocs/gcc/Inline.html
			//
			// If you specify both inline and extern in the function definition, then the
			// definition is used only for inlining. In no case is the function compiled on
			// its own, not even if you refer to its address explicitly. Such an address
			// becomes an external reference, as if you had only declared the function, and
			// had not defined it.
			//
			// This combination of inline and extern has almost the effect of a macro. The
			// way to use it is to put a function definition in a header file with these
			// keywords, and put another copy of the definition (lacking inline and extern)
			// in a library file. The definition in the header file causes most calls to
			// the function to be inlined. If any uses of the function remain, they refer
			// to the single copy in the library.
			if d.IsExtern() && d.Type().Inline() {
				continue
			}

			if fn := d.FunctionDefinition(); fn != nil {
				if _, ok := p.sharedFns[fn]; ok {
					if _, ok := sharedFns[fn]; ok {
						continue
					}

					sharedFns[fn] = struct{}{}
				}
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
				p.checkAttributes(d.Type())
			}
			nm := d.Name()
			name := nm.String()
			switch d.Linkage {
			case cc.External:
				if ex := p.externs[nm]; ex != nil {
					if _, ok := p.task.hide[name]; ok {
						break
					}

					if d.Type().Kind() != cc.Function {
						break
					}

					p.err(d, "redeclared: %s", d.Name())
					break
				}

				isMain := p.isMain && nm == idMain
				switch exportExtern {
				case doNotExport:
					// nop
				case exportCapitalize:
					name = capitalize(name)
				case exportPrefix:
					name = p.task.exportExterns + name
				}
				name = p.scope.take(cc.String(name))
				if isMain {
					p.mainName = name
					d.Read++
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
				if token.IsExported(name) && !p.isMain && p.task.exportExternsValid {
					name = "s" + name
				}
				tld := &tld{name: p.scope.take(cc.String(name))}
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
					if exportTypedef == doNotExport && strings.HasPrefix(name, "__") {
						break
					}

					ex, ok := p.typedefTypes[d.Name()]
					if ok {
						sig := p.typeSignature(d, d.Type())
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
						name = p.task.exportTypedefs + name
					}
					tld := &tld{name: p.scope.take(cc.String(name))}
					p.typedefTypes[d.Name()] = &typedef{p.typeSignature(d, d.Type()), tld}
					for _, v := range ast.Scope[nm] {
						if d, ok := v.(*cc.Declarator); ok {
							p.tlds[d] = tld
						}
					}
				}
			default:
				panic(todo("", p.pos(d), nm, d.Linkage))
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

func (p *project) checkAttributes(t cc.Type) (r bool) {
	//TODO typedefs seem to not have attributes set from cc/v3
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
			if !p.checkAttributes(f.Type()) {
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
					if x.Token.Value == idPacked {
						p.err(sd, "unsupported attribute: packed")
						r = false
						return false
					}

					if x.Token.Value != idAligned {
						break
					}

					switch v := x.ExpressionList.AssignmentExpression.Operand.Value().(type) {
					case cc.Int64Value:
						if int(v) != t.Align() {
							p.err(sd, "unsupported attribute: alignment")
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
	if strings.HasPrefix(s, "_") {
		s = "X" + s
	}
	a := []rune(s)
	return strings.ToUpper(string(a[0])) + string(a[1:])
}

func (p *project) main() error {
	targs := append([]string(nil), p.task.args...)
	for i, v := range targs {
		if v == "" {
			targs[i] = `""`
		}
	}
	p.o(`// Code generated by '%s %s', DO NOT EDIT.

package %s

`,
		filepath.Base(p.task.args[0]),
		strings.Join(targs[1:], " "),
		p.task.pkgName,
	)
	if len(p.defines) != 0 {
		p.w("\nconst (")
		p.w("%s", strings.Join(p.defines, "\n"))
		p.w("\n)\n\n")
	}
	var a []*enumSpec
	for _, es := range p.enumSpecs {
		if es.spec.LexicalScope().Parent() == nil && !es.emitted {
			a = append(a, es)
		}
	}
	sort.Slice(a, func(i, j int) bool {
		return a[i].decl.Position().String() < a[j].decl.Position().String()
	})
	for _, es := range a {
		es.emit(p)
	}
	for i, v := range p.task.asts {
		var t0 time.Time
		if p.task.traceTranslationUnits {
			fmt.Printf("Go back end %v/%v: %s ... ", i+1, len(p.task.asts), p.task.sources[i].Name)
			t0 = time.Now()
		}
		p.oneAST(v)
		if p.task.traceTranslationUnits {
			fmt.Println(time.Since(t0))
		}
		p.task.asts[i] = nil
		memGuard(i, p.task.isScripted)
	}
	sort.Slice(p.task.imported, func(i, j int) bool { return p.task.imported[i].path < p.task.imported[j].path })
	p.o(`import (
	"math"
	"reflect"
	"unsafe"
`)
	if len(p.verifyStructs) != 0 {
		p.o("\t\"fmt\"\n")
	}
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
	p.doVerifyStructs()
	if err := p.Err(); err != nil {
		return err
	}

	if _, err := p.buf.WriteTo(p.task.out); err != nil {
		return err
	}

	return p.Err()
}

func (p *project) doVerifyStructs() {
	if len(p.verifyStructs) == 0 {
		return
	}

	var a []string
	for k := range p.verifyStructs {
		a = append(a, k)
	}
	sort.Strings(a)
	p.w("\n\nfunc init() {")
	n := 0
	for _, k := range a {
		t := p.verifyStructs[k]
		p.w("\nvar v%d %s", n, k)
		p.w("\nif g, e := unsafe.Sizeof(v%d), uintptr(%d); g != e { panic(fmt.Sprintf(`invalid struct/union size, got %%v, expected %%v`, g, e))}", n, t.Size())
		nf := t.NumField()
		for idx := []int{0}; idx[0] < nf; idx[0]++ {
			f := t.FieldByIndex(idx)
			if f.IsFlexible() {
				break
			}

			if f.IsBitField() || f.Type().Size() == 0 {
				continue
			}

			nm := p.fieldName2(f.Declarator(), f)
			switch {
			case t.Kind() == cc.Union:
				if f.Offset() != 0 {
					panic(todo(""))
				}

				if idx[0] != 0 {
					break
				}

				fallthrough
			default:
				p.w("\nif g, e := unsafe.Offsetof(v%d.%s), uintptr(%d); g != e { panic(fmt.Sprintf(`invalid struct/union field offset, got %%v, expected %%v`, g, e))}", n, nm, f.Offset())
				p.w("\nif g, e := unsafe.Sizeof(v%d.%s), uintptr(%d); g != e { panic(fmt.Sprintf(`invalid struct/union field size, got %%v, expected %%v`, g, e))}", n, nm, f.Type().Size())
			}
		}
		n++
	}
	p.w("\n}\n")
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
			p.w("// %s:", p.pos(init))
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
		p.w("\n\n")
		//TODO add cmd line option for this
		//TODO s := strings.TrimSpace(hex.Dump(b))
		//TODO a := strings.Split(s, "\n")
		//TODO p.w("//  %s\n", strings.Join(a, "\n//  "))
		p.w("var %s = %q\n", p.tsName, b)
		p.w("var %s = (*reflect.StringHeader)(unsafe.Pointer(&%s)).Data\n", p.tsNameP, p.tsName)
	}
	if len(p.tsW) != 0 {
		p.w("var %s = [...]%s{", p.tsWName, p.typ(nil, p.ast.WideCharType))
		for _, v := range p.tsW {
			p.w("%d, ", v)
		}
		p.w("}\n")
		p.w("var %s = uintptr(unsafe.Pointer(&%s[0]))\n", p.tsWNameP, p.tsWName)
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
	p.w("%s", tidyCommentString(ast.TrailingSeperator.String()))
}

func (p *project) externalDeclaration(n *cc.ExternalDeclaration) {
	switch n.Case {
	case cc.ExternalDeclarationFuncDef: // FunctionDefinition
		p.functionDefinition(n.FunctionDefinition)
	case cc.ExternalDeclarationDecl: // Declaration
		p.declaration(nil, n.Declaration, false)
	case cc.ExternalDeclarationAsm: // AsmFunctionDefinition
		// nop
	case cc.ExternalDeclarationAsmStmt: // AsmStatement
		panic(todo("", p.pos(n)))
	case cc.ExternalDeclarationEmpty: // ';'
		// nop
	case cc.ExternalDeclarationPragma: // PragmaSTDC
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) declaration(f *function, n *cc.Declaration, topDecl bool) {
	cc.Inspect(n.DeclarationSpecifiers, func(m cc.Node, entry bool) bool {
		switch x := m.(type) {
		case *cc.EnumSpecifier:
			if f == nil {
				p.enumSpecs[x].emit(p)
			}
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
	sep := tidyComment("\n", n) //TODO repeats
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
		if !p.pass1 {
			p.staticQueue = append(p.staticQueue, n)
		}
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
	case exprDecay:
		p.declaratorDecay(n, f, d, t, mode, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) declaratorDecay(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	if d.Type().Kind() != cc.Array {
		panic(todo("", n.Position(), p.pos(d)))
	}

	if f != nil {
		if local := f.locals[d]; local != nil {
			if d.IsParameter {
				p.w("%s", local.name)
				return
			}

			if p.pass1 {
				f.pin(d)
				return
			}

			p.w("%s%s/* &%s[0] */", f.bpName, nonZeroUintptr(local.off), local.name)
			return
		}
	}

	if x := p.tlds[d]; x != nil && d.IsStatic() {
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
		return
	}

	switch x := p.symtab[d.Name().String()].(type) {
	case *tld:
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
	case *imported:
		x.used = true
		p.w("uintptr(unsafe.Pointer(&%sX%s))", x.qualifier, d.Name())
	default:
		panic(todo("%v: %v: %q %T", n.Position(), p.pos(d), d.Name(), x))
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

	p.declaratorDefault(n, d)
}

func (p *project) declaratorDefault(n cc.Node, d *cc.Declarator) {
	if x := p.tlds[d]; x != nil && d.IsStatic() {
		p.w("%s", x.name)
		return
	}

	switch x := p.symtab[d.Name().String()].(type) {
	case *tld:
		p.w("%s", x.name)
	case *imported:
		x.used = true
		p.w("%sX%s", x.qualifier, d.Name())
	default:
		if d.IsExtern() {
			switch d.Name() {
			case idEnviron:
				if d.Type().String() == "pointer to pointer to char" {
					p.w("%sEnviron()", p.task.crt)
					return
				}
			}
		}

		if d.Linkage == cc.External && p.task.nostdlib {
			p.w("X%s", d.Name())
			return
		}

		id := fmt.Sprintf("__builtin_%s", d.Name())
		switch x := p.symtab[id].(type) {
		case *imported:
			x.used = true
			p.w("%sX%s", x.qualifier, d.Name())
			return
		}

		p.err(n, "back-end: undefined: %s", d.Name())
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

	p.declaratorDefault(n, d)
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

	p.declaratorDefault(n, d)
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

				panic(todo("", p.pos(d)))
			}

			if x := p.tlds[d]; x != nil && d.IsStatic() {
				p.w("(*(*")
				p.functionSignature(f, u, "")
				p.w(")(unsafe.Pointer(&%s)))", x.name)
				return
			}

			switch x := p.symtab[d.Name().String()].(type) {
			case *tld:
				p.w("(*(*")
				p.functionSignature(f, u, "")
				p.w(")(unsafe.Pointer(&%s)))", x.name)
			case *imported:
				x.used = true
				p.w("uintptr(unsafe.Pointer(&%sX%s))", x.qualifier, d.Name())
			default:
				panic(todo("%v: %v: %q", n.Position(), p.pos(d), d.Name()))
			}
		}

		panic(todo("", p.pos(d), u))
	default:
		panic(todo("", p.pos(d), u))
	}
}

func (p *project) declaratorFuncFunc(n cc.Node, f *function, d *cc.Declarator, t cc.Type, mode exprMode, flags flags) {
	switch d.Type().Kind() {
	case cc.Function:
		// ok
	default:
		panic(todo("", p.pos(d), d.Type(), d.Type().Kind()))
	}

	if f != nil {
		if local := f.locals[d]; local != nil {
			panic(todo("", n.Position(), p.pos(d)))
		}
	}

	p.declaratorDefault(n, d)
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

	p.declaratorDefault(n, d)
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

	p.declaratorLValueDefault(n, d)
}

func (p *project) declaratorLValueDefault(n cc.Node, d *cc.Declarator) {
	if x := p.tlds[d]; x != nil && d.IsStatic() {
		p.w("%s", x.name)
		return
	}

	switch x := p.symtab[d.Name().String()].(type) {
	case *tld:
		p.w("%s", x.name)
	case *imported:
		x.used = true
		p.w("%sX%s", x.qualifier, d.Name())
	default:
		if d.IsExtern() {
			switch d.Name() {
			case idEnviron:
				if d.Type().String() == "pointer to pointer to char" {
					p.w("*(*uintptr)(unsafe.Pointer(%sEnvironP()))", p.task.crt)
					return
				}
			}
		}
		panic(todo("%v: %v: %q", n.Position(), p.pos(d), d.Name()))
	}
}

func (p *project) declaratorKind(d *cc.Declarator) opKind {
	switch {
	case p.isArrayParameterDeclarator(d):
		return opArrayParameter
	case !p.pass1 && p.isArrayDeclarator(d):
		return opArray
	case d.Type().Kind() == cc.Function:
		return opFunction
	case d.Type().Kind() == cc.Union:
		return opUnion
	default:
		return opNormal
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
			panic(todo("", p.pos(n)))
			//TODO type error
			p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(d, d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
			return
		}

		p.w("%s", local.name)
		return
	}

	p.declaratorDefault(n, d)
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

	p.declaratorDefault(n, d)
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
	if p.pass1 {
		f.pin(d)
		return
	}

	local := f.locals[d]
	p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
}

func (p *project) declaratorAddrOfFunction(n cc.Node, f *function, d *cc.Declarator) {
	if d.Type().Kind() != cc.Function {
		panic(todo("", p.pos(n)))
	}

	if x := p.tlds[d]; x != nil && d.IsStatic() {
		p.w("*(*uintptr)(unsafe.Pointer(&struct{f ")
		p.functionSignature(f, d.Type(), "")
		p.w("}{%s}))", x.name)
		return
	}

	switch x := p.symtab[d.Name().String()].(type) {
	case *tld:
		p.w("*(*uintptr)(unsafe.Pointer(&struct{f ")
		p.functionSignature(f, d.Type(), "")
		p.w("}{%s}))", x.name)
	case *imported:
		x.used = true
		p.w("*(*uintptr)(unsafe.Pointer(&struct{f ")
		p.functionSignature(f, d.Type(), "")
		p.w("}{%sX%s}))", x.qualifier, d.Name())
	default:
		p.err(d, "back-end: undefined: %s", d.Name())
	}
}

func (p *project) declaratorAddrOfUnion(n cc.Node, f *function, d *cc.Declarator) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if p.pass1 {
				f.pin(d)
				return
			}

			if local.isPinned {
				p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			panic(todo("", p.pos(n)))
		}
	}

	if x := p.tlds[d]; x != nil && d.IsStatic() {
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
		return
	}

	switch x := p.symtab[d.Name().String()].(type) {
	case *tld:
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
	case *imported:
		x.used = true
		p.w("uintptr(unsafe.Pointer(&%sX%s))", x.qualifier, d.Name())
	default:
		panic(todo("%v: %v: %q", n.Position(), p.pos(d), d.Name()))
	}
}

func (p *project) declaratorAddrOfNormal(n cc.Node, f *function, d *cc.Declarator, flags flags) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if p.pass1 && flags&fAddrOfFuncPtrOk == 0 {
				f.pin(d)
				return
			}

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

			panic(todo("", p.pos(n), p.pos(d), d.Name(), d.Type(), d.IsParameter, d.AddressTaken, flags&fAddrOfFuncPtrOk != 0))
		}
	}

	if x := p.tlds[d]; x != nil && d.IsStatic() {
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
		return
	}

	switch x := p.symtab[d.Name().String()].(type) {
	case *tld:
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
	case *imported:
		x.used = true
		p.w("uintptr(unsafe.Pointer(&%sX%s))", x.qualifier, d.Name())
	default:
		panic(todo("%v: %v: %q %T", n.Position(), p.pos(d), d.Name(), x))
	}
}

func (p *project) declaratorAddrOfArray(n cc.Node, f *function, d *cc.Declarator) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if p.pass1 {
				f.pin(d)
				return
			}

			if local.isPinned {
				p.w("%s%s/* &%s */", f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			panic(todo("", p.pos(d), d.Name(), d.Type(), d.IsParameter))
		}
	}

	if x := p.tlds[d]; x != nil && d.IsStatic() {
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
		return
	}

	switch x := p.symtab[d.Name().String()].(type) {
	case *tld:
		p.w("uintptr(unsafe.Pointer(&%s))", x.name)
	case *imported:
		x.used = true
		p.w("uintptr(unsafe.Pointer(&%sX%s))", x.qualifier, d.Name())
	default:
		panic(todo("%v: %v: %q", n.Position(), p.pos(d), d.Name()))
	}
}

func (p *project) convertType(n cc.Node, from, to cc.Type, flags flags) string {
	// trc("%v: %v -> %v\n%s", p.pos(n), from, to, debug.Stack()[:600]) //TODO-
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
	case cc.Function, cc.Struct, cc.Union, cc.Ptr, cc.Array:
		if from.Kind() == to.Kind() {
			return ""
		}

		panic(todo("", n.Position(), from, to, from.Alias(), to.Alias()))
	case cc.Double, cc.Float:
		p.w("%s(", p.typ(n, to))
		return ")"
	}

	panic(todo("", n.Position(), from, to, from.Alias(), to.Alias()))
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

		panic(todo("%v: %q -> %q", p.pos(n), from, to))
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
		p.checkAttributes(t)
		if _, ok := p.typedefsEmited[tld.name]; ok {
			return
		}

		p.typedefsEmited[tld.name] = struct{}{}
		if t.Kind() != cc.Void {
			p.w("%stype %s = %s; /* %v */", sep, tld.name, p.typ(n, t), p.pos(d))
		}
		return
	}

	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		p.w("%svar %s %s\t/* %v: */", sep, tld.name, p.typ(n, t), p.pos(n))
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
		p.w("; /* %v */", p.pos(d))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) functionDefinition(n *cc.FunctionDefinition) {
	// DeclarationSpecifiers Declarator DeclarationList CompoundStatement
	if p.task.header {
		return
	}

	if _, ok := p.sharedFns[n]; ok {
		if _, ok := p.sharedFnsEmitted[n]; ok {
			return
		}

		p.sharedFnsEmitted[n] = struct{}{}
	}

	d := n.Declarator
	if d.IsExtern() && d.Type().Inline() {
		// https://gcc.gnu.org/onlinedocs/gcc/Inline.html
		//
		// If you specify both inline and extern in the function definition, then the
		// definition is used only for inlining. In no case is the function compiled on
		// its own, not even if you refer to its address explicitly. Such an address
		// becomes an external reference, as if you had only declared the function, and
		// had not defined it.
		//
		// This combination of inline and extern has almost the effect of a macro. The
		// way to use it is to put a function definition in a header file with these
		// keywords, and put another copy of the definition (lacking inline and extern)
		// in a library file. The definition in the header file causes most calls to
		// the function to be inlined. If any uses of the function remain, they refer
		// to the single copy in the library.
		return
	}

	name := d.Name().String()
	if _, ok := p.task.hide[name]; ok {
		return
	}

	if p.isMain && d.Linkage == cc.External && d.Read == 0 && !d.AddressTaken && len(p.task.asts) == 1 {
		return
	}

	if d.Linkage == cc.Internal && d.Read == 0 && !d.AddressTaken /*TODO- && strings.HasPrefix(name, "__") */ {
		return
	}

	tld := p.tlds[d]
	if tld == nil {
		return
	}

	p.fn = name

	defer func() { p.fn = "" }()

	f := newFunction(p, n)
	p.pass1 = true
	p.compoundStatement(f, n.CompoundStatement, "", false, false, 0)
	p.pass1 = false
	p.functionDefinitionSignature(f, tld)
	p.w(" ")
	comment := fmt.Sprintf("/* %v: */", p.pos(d))
	if need := f.off; need != 0 {
		scope := f.blocks[n.CompoundStatement].scope
		f.bpName = scope.take(idBp)
		p.w("{%s\n%s := %s.Alloc(%d)\n", comment, f.bpName, f.tlsName, need)
		p.w("defer %s.Free(%d)\n", f.tlsName, need)
		for _, v := range d.Type().Parameters() {
			if local := f.locals[v.Declarator()]; local != nil && local.isPinned { // Pin it.
				p.w("*(*%s)(unsafe.Pointer(%s%s)) = %s\n", p.typ(v.Declarator(), paramTypeDecay(v.Declarator())), f.bpName, nonZeroUintptr(local.off), local.name)
			}
		}
		comment = ""
	}
	p.compoundStatement(f, n.CompoundStatement, comment, false, true, 0)
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

func (p *project) compoundStatement(f *function, n *cc.CompoundStatement, scomment string, forceNoBraces, fnBody bool, mode exprMode) {
	// '{' BlockItemList '}'
	brace := (!n.IsJumpTarget() || n.Parent() == nil) && !forceNoBraces
	if brace && (n.Parent() != nil || f.off == 0) {
		p.w("{%s", scomment)
	}
	if fnBody {
		p.instrument(n)
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
		m := mode
		if list.BlockItemList != nil {
			m = 0
		}
		r = p.blockItem(f, list.BlockItem, m)
	}
	if n.Parent() == nil && r == nil && f.rt.Kind() != cc.Void {
		p.w("\nreturn ")
		p.zeroValue(f.rt)
	}
	s := tidyComment("\n", &n.Token2)
	p.w("%s", s)
	if brace {
		if !strings.HasSuffix(s, "\n") {
			p.w("\n")
		}
		p.w("}")
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

func (p *project) blockItem(f *function, n *cc.BlockItem, mode exprMode) (r *cc.JumpStatement) {
	switch n.Case {
	case cc.BlockItemDecl: // Declaration
		p.declaration(f, n.Declaration, false)
	case cc.BlockItemStmt: // Statement
		r = p.statement(f, n.Statement, false, false, false, mode)
		p.w(";")
		if r == nil {
			p.instrument(n)
		}
	case cc.BlockItemLabel: // LabelDeclaration
		panic(todo("", p.pos(n)))
		p.w(";")
	case cc.BlockItemFuncDef: // DeclarationSpecifiers Declarator CompoundStatement
		p.err(n, "nested functions not supported")
		p.w(";")
	case cc.BlockItemPragma: // PragmaSTDC
		panic(todo("", p.pos(n)))
		p.w(";")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) instrument(n cc.Node) {
	if p.task.cover {
		p.w("%sCover();", p.task.crt)
	}
	if p.task.coverC {
		p.w("%sCoverC(%q);", p.task.crt, p.pos(n).String()+" "+p.fn)
	}
	if p.task.watch {
		p.w("%sWatch();", p.task.crt)
	}
}

func (p *project) statement(f *function, n *cc.Statement, forceCompoundStmtBrace, forceNoBraces, switchBlock bool, mode exprMode) (r *cc.JumpStatement) {
	if forceCompoundStmtBrace {
		p.w(" {")
		if !switchBlock {
			p.instrument(n)
		}
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
		p.compoundStatement(f, n.CompoundStatement, "", forceCompoundStmtBrace || forceNoBraces, false, 0)
	case cc.StatementExpr: // ExpressionStatement
		if mode != 0 {
			p.w("return ")
			e := n.ExpressionStatement.Expression
			p.expression(f, e, e.Operand.Type(), exprValue, fOutermost)
			break
		}

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

		p.w("panic(`%s: assembler statements not supported`)", n.Position())
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
		panic(todo("", p.pos(n)))
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
	case exprDecay:
		p.expressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) expressionDecay(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		p.w("func() uintptr {")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, flags)
		p.w("; return ")
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags|fOutermost)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionSelect(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionFunc(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionLValue(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionPSelect(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionAddrOf(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		p.w(" func() uintptr {")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, flags|fOutermost)
		p.w("; return ")
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags|fOutermost)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionBool(f *function, n *cc.Expression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
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
		switch {
		case n.AssignmentExpression.Operand.Type().Kind() == cc.Array:
			p.expressionDecay(f, n, t, exprDecay, flags)
		default:
			defer p.w("%s", p.convertType(n, n.Operand.Type(), t, flags))
			p.w("func() %v {", p.typ(n, n.AssignmentExpression.Operand.Type()))
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprVoid, flags)
			p.w("; return ")
			p.assignmentExpression(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type(), exprValue, flags)
			p.w("}()")
		}
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
	case !p.pass1 && p.isArray(f, d, t):
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
	case exprDecay:
		p.assignmentExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) assignmentExpressionDecay(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionLsh: // UnaryExpremode, ssion "<<=
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionSelect(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionFunc(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionLsh: // UnaryExpremode, ssion "<<=
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionLValue(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionBool(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	default:
		// case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		// case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		// case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		// case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		// case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		// case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		// case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		// case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		// case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		// case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		// case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.assignmentExpression(f, n, t, exprValue, flags|fOutermost)
	}
}

func (p *project) assignmentExpressionAddrOf(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		p.assignmentExpressionValueAddrOf(f, n, t, mode, flags)
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionLsh: // UnaryExpression "<<=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		panic(todo("", p.pos(n)))
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionValueAddrOf(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	// UnaryExpression '=' AssignmentExpression
	if mode == exprCondReturn {
		panic(todo("", p.pos(n)))
	}

	lhs := n.UnaryExpression
	switch k := p.opKind(f, lhs, lhs.Operand.Type()); k {
	case opStruct:
		p.assignmentExpressionValueAssignStructAddrof(f, n, n.Operand.Type(), mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignmentExpressionValueAssignStructAddrof(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	// UnaryExpression '=' AssignmentExpression
	lhs := n.UnaryExpression.Operand.Type()
	rhs := n.AssignmentExpression.Operand.Type()
	if lhs.Kind() == cc.Array || rhs.Kind() == cc.Array {
		panic(todo("", p.pos(n)))
	}

	if d := n.UnaryExpression.Declarator(); d != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				panic(todo("", p.pos(n)))
			}

			panic(todo("", p.pos(n)))
		}
	}

	p.w("%sXmemmove(tls, ", p.task.crt)
	p.unaryExpression(f, n.UnaryExpression, lhs, exprAddrOf, flags|fOutermost)
	p.w(", ")
	p.assignmentExpression(f, n.AssignmentExpression, rhs, exprAddrOf, flags|fOutermost)
	p.w(", %d)", lhs.Size())
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
	case opStruct, opUnion:
		p.assignmentExpressionValueAssignStruct(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignmentExpressionValueAssignStruct(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	// UnaryExpression '=' AssignmentExpression
	lhs := n.UnaryExpression.Operand.Type()
	rhs := n.AssignmentExpression.Operand.Type()
	if lhs.Kind() == cc.Array || rhs.Kind() == cc.Array {
		panic(todo("", p.pos(n)))
	}

	p.w(" func() %s { __v := ", p.typ(n, lhs))
	p.assignmentExpression(f, n.AssignmentExpression, rhs, exprValue, flags|fOutermost)
	p.w(";")
	p.unaryExpression(f, n.UnaryExpression, lhs, exprLValue, flags|fOutermost)
	p.w(" = __v; return __v}()")
}

func (p *project) assignmentExpressionValueAssignBitfield(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	if d := n.UnaryExpression.Declarator(); d != nil {
		panic(todo("", p.pos(n)))
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
		p.w(", %d, %d, %#x)", bf.BitFieldWidth(), bf.BitFieldOffset(), bf.Mask())
	default:
		p.w("%sAssignBitFieldPtr%d%s(", p.task.crt, bf.BitFieldBlockWidth(), p.bfHelperType(lt))
		p.unaryExpression(f, lhs, lt, exprAddrOf, flags)
		p.w(", ")
		p.assignmentExpression(f, n.AssignmentExpression, lt, exprValue, flags|fOutermost)
		p.w(", %d, %d, %#x)", bf.BitFieldWidth(), bf.BitFieldOffset(), bf.Mask())
	}
}

func (p *project) assignmentExpressionValueAssignNormal(f *function, n *cc.AssignmentExpression, t cc.Type, mode exprMode, flags flags) {
	if d := n.UnaryExpression.Declarator(); d != nil {
		if !d.Type().IsScalarType() {
			panic(todo("", p.pos(n)))
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
				mode = exprDecay
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
	case exprDecay:
		p.conditionalExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) conditionalExpressionDecay(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		p.w(" func() %s { if ", p.typ(n, t))
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), exprBool, flags|fOutermost)
		p.w(" { return ")
		switch n.Expression.Operand.Type().Kind() {
		case cc.Array:
			p.expression(f, n.Expression, t, exprDecay, flags|fOutermost)
		case cc.Ptr:
			panic(todo("", n.Expression.Position(), n.Expression.Operand.Type()))
		default:
			panic(todo("", n.Expression.Position(), n.Expression.Operand.Type()))
		}
		p.w("}; return ")
		switch n.ConditionalExpression.Operand.Type().Kind() {
		case cc.Array:
			p.conditionalExpression(f, n.ConditionalExpression, t, exprDecay, flags|fOutermost)
		default:
			p.conditionalExpression(f, n.ConditionalExpression, t, exprValue, flags|fOutermost)
		}
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
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
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionFunc(f *function, n *cc.ConditionalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
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
	case exprDecay:
		p.logicalOrExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) logicalOrExpressionDecay(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionSelect(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionFunc(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionPSelect(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionLValue(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionVoid(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		p.w("_ = ")
		p.logicalOrExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
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
	case exprDecay:
		p.logicalAndExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) logicalAndExpressionDecay(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionSelect(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionFunc(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionPSelect(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionLValue(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
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
	case exprDecay:
		p.inclusiveOrExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) inclusiveOrExpressionDecay(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionSelect(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionFunc(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionPSelect(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionLValue(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionVoid(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		p.w("_ = ")
		p.inclusiveOrExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
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
	case exprDecay:
		p.exclusiveOrExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) exclusiveOrExpressionDecay(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionSelect(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionFunc(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionPSelect(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionLValue(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionVoid(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		p.w("_ = ")
		p.exclusiveOrExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
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

	return !lo.Type().IsSignedType() && a.Sign() == 0 ||
		!ro.Type().IsSignedType() && b.Sign() == 0 ||
		overflows(a.Xor(a, b), promote)
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
	case exprDecay:
		p.andExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) andExpressionDecay(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionSelect(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionFunc(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionPSelect(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionLValue(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionVoid(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		p.w("_ = ")
		p.andExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
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
	case exprDecay:
		p.equalityExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) equalityExpressionDecay(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo("", p.pos(n)))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionSelect(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo("", p.pos(n)))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionFunc(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo("", p.pos(n)))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionPSelect(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo("", p.pos(n)))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionLValue(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo("", p.pos(n)))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionVoid(f *function, n *cc.EqualityExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, mode, flags)
	default:
		// case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		// case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		p.w("_ = ")
		p.equalityExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
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
	case exprDecay:
		p.relationalExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) relationalExpressionDecay(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionSelect(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionFunc(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionPSelect(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionLValue(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		panic(todo("", p.pos(n)))
	case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) relationalExpressionVoid(f *function, n *cc.RelationalExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, mode, flags)
	default:
		// case cc.RelationalExpressionLt: // RelationalExpression '<' ShiftExpression
		// case cc.RelationalExpressionGt: // RelationalExpression '>' ShiftExpression
		// case cc.RelationalExpressionLeq: // RelationalExpression "<=" ShiftExpression
		// case cc.RelationalExpressionGeq: // RelationalExpression ">=" ShiftExpression
		p.w("_ = ")
		p.relationalExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
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
	case exprDecay:
		p.shiftExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) shiftExpressionDecay(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo("", p.pos(n)))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionSelect(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo("", p.pos(n)))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionFunc(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo("", p.pos(n)))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionPSelect(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo("", p.pos(n)))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionLValue(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo("", p.pos(n)))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionVoid(f *function, n *cc.ShiftExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo("", p.pos(n)))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
		p.w("(")
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), exprValue, flags)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), exprValue, flags)
		p.w(")&%#x", n.ShiftExpression.Operand.Type().BitField().Mask())
	case shiftOverflows(n, n.ShiftExpression.Operand, n.AdditiveExpression.Operand, oper, n.Operand.Type()):
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
	case shiftOverflows(n, n.ShiftExpression.Operand, n.AdditiveExpression.Operand, oper, n.Operand.Type()):
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

func shiftOverflows(n cc.Node, lo, ro cc.Operand, oper string, result cc.Type) bool {
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
		panic(todo("", pos(n)))
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
	case exprDecay:
		p.additiveExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) additiveExpressionDecay(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo("", p.pos(n)))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionSelect(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo("", p.pos(n)))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionFunc(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo("", p.pos(n)))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionVoid(f *function, n *cc.AdditiveExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, mode, flags)
	case
		cc.AdditiveExpressionAdd, // AdditiveExpression '+' MultiplicativeExpression
		cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression

		p.w("_ = ")
		p.additiveExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
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
		defer p.w("%s", p.bitFieldPatch2(n, lo, ro, n.Promote()))
		switch {
		case intAddOverflows(n, lo, ro, oper, n.Promote()): // i +- j
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
		defer p.w("%s", p.bitFieldPatch2(n, lo, ro, n.Promote()))
		switch {
		case intAddOverflows(n, lo, ro, oper, n.Promote()): // i +- j
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
		p.additiveExpression(f, n.AdditiveExpression, lt, exprDecay, flags)
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
		panic(todo("", p.pos(n)))
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
		p.multiplicativeExpression(f, n.MultiplicativeExpression, rt.Decay(), exprDecay, flags&^fOutermost)
		p.w(")/%d", lt.Elem().Size())
	case lt.Kind() == cc.Array && rt.Kind() == cc.Ptr && oper == "-": // p - q
		panic(todo("", p.pos(n)))
	case lt.Kind() == cc.Array && rt.Kind() == cc.Array && oper == "-": // p - q
		panic(todo("", p.pos(n)))
	default:
		panic(todo("", n.Position(), lt, rt, oper))
	}
}

func (p *project) bitFieldPatch2(n cc.Node, a, b cc.Operand, promote cc.Type) string {
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
				panic(todo("", p.pos(n)))
			}
		}
	case b.Type().IsBitFieldType():
		bf := b.Type().BitField()
		w = bf.BitFieldWidth()
		m = bf.Mask() >> bf.BitFieldOffset()
	default:
		return ""
	}

	p.w("((")
	switch {
	case promote.IsSignedType():
		n := int(promote.Size())*8 - w
		var s string
		switch promote.Size() {
		case 4:
			s = fmt.Sprintf(")&%#x", int32(m))
		default:
			s = fmt.Sprintf(")&%#x", m)
		}
		if n != 0 {
			s += fmt.Sprintf("<<%d>>%[1]d", n)
		}
		return ")" + s
	default:
		return fmt.Sprintf(")&%#x)", m)
	}
}

func intAddOverflows(n cc.Node, lo, ro cc.Operand, oper string, promote cc.Type) bool {
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
		panic(todo("", pos(n)))
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
	case exprDecay:
		p.multiplicativeExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) multiplicativeExpressionDecay(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionSelect(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionFunc(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionPSelect(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionLValue(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionVoid(f *function, n *cc.MultiplicativeExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, mode, flags)
	case cc.MultiplicativeExpressionMul: // MultiplicativeExpression '*' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionDiv: // MultiplicativeExpression '/' CastExpression
		panic(todo("", p.pos(n)))
	case cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression
		panic(todo("", p.pos(n)))
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
	case intMulOverflows(n, n.Operand, n.MultiplicativeExpression.Operand, n.CastExpression.Operand, oper, n.Promote()):
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
		p.w(" %s%s", oper, tidyComment(" ", &n.Token))
		p.castExpression(f, n.CastExpression, n.Promote(), exprValue, flags|fForceRuntimeConv)
	default:
		defer p.w("%s", p.bitFieldPatch2(n, n.MultiplicativeExpression.Operand, n.CastExpression.Operand, n.Promote()))
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

func intMulOverflows(n cc.Node, r, lo, ro cc.Operand, oper string, promote cc.Type) bool {
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
		panic(todo("", pos(n)))
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
	case exprDecay:
		p.castExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) castExpressionDecay(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
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
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("%v: %v, %v -> %v, %v -> %v, %v", p.pos(n), ot, ot.Kind(), tn, tn.Kind(), t, t.Kind()))
		}
		switch t.Kind() {
		case cc.Ptr:
			switch et := t.Elem(); et.Kind() {
			case cc.Function:
				ft = et
			default:
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("%v: %v, %v -> %v, %v -> %v, %v", p.pos(n), ot, ot.Kind(), tn, tn.Kind(), t, t.Kind()))
		}
		switch ot.Kind() {
		//TODO- case op.Type().Kind() == cc.Ptr:
		//TODO- 	switch {
		//TODO- 	case tn.Kind() == cc.Ptr && tn.Elem().Kind() == cc.Function && t.Kind() == cc.Ptr:
		//TODO- 		ft := tn.Elem().Alias()
		//TODO- 		switch ft.Kind() {
		//TODO- 		default:
		//TODO- 			panic(todo("", p.pos(n), n.CastExpression.Operand.Type(), n.CastExpression.Operand.Type().Kind()))
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
		//TODO- 		panic(todo("", p.pos(n)))
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
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("%v: %v, %v -> %v, %v -> %v, %v", p.pos(n), ot, ot.Kind(), tn, tn.Kind(), t, t.Kind()))
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
		panic(todo("", p.pos(n)))
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
			panic(todo("", n.Position()))
		}
	default:
		panic(todo("%v: %v -> %v -> %v", p.pos(n), op.Type(), tn, t))
	}
}

func (p *project) castExpressionValueArray(f *function, n *cc.CastExpression, t cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	tn := n.TypeName.Type()
	switch {
	case tn.IsScalarType():
		defer p.w("%s", p.convertType(n, nil, t, flags))
		p.castExpression(f, n.CastExpression, tn, exprDecay, flags)
	default:
		panic(todo("", p.pos(n)))
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
			panic(todo("", p.pos(n)))
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
	case exprDecay:
		p.unaryExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) unaryExpressionDecay(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags)
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
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
		panic(todo("", p.pos(n)))
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
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", p.pos(n), ot, ot.Kind()))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpressionFunc(f *function, n *cc.UnaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", p.pos(n)))
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
					panic(todo("", p.pos(n), et2, et2.Kind()))
				}
			default:
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		case cc.Function:
			p.castExpression(f, n.CastExpression, ot, mode, flags|fAddrOfFuncPtrOk)
		default:
			panic(todo("", p.pos(n), ot, ot.Kind(), mode))
		}
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", p.pos(n)))
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
		panic(todo("", n.Position()))
		//TODO- p.w("(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		//TODO- p.unaryExpression(f, n, t, exprValue, flags)
		//TODO- p.w("))")
	case cc.UnaryExpressionDeref: // '*' CastExpression
		panic(todo("", n.Position()))
		//TODO- ot := n.CastExpression.Operand.Type()
		//TODO- switch ot.Kind() {
		//TODO- case cc.Ptr:
		//TODO- 	switch et := ot.Elem(); {
		//TODO- 	case et.Kind() == cc.Ptr:
		//TODO- 		switch et2 := et.Elem(); et2.Kind() {
		//TODO- 		case cc.Struct:
		//TODO- 			if et2.IsIncomplete() {
		//TODO- 				p.w("(*(**uintptr)(unsafe.Pointer(")
		//TODO- 				p.castExpression(f, n.CastExpression, t, exprValue, flags)
		//TODO- 				p.w(")))")
		//TODO- 				break
		//TODO- 			}

		//TODO- 			p.w("(*(**%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		//TODO- 			p.castExpression(f, n.CastExpression, t, exprValue, flags)
		//TODO- 			p.w(")))")
		//TODO- 		default:
		//TODO- 			panic(todo("", p.pos(n), et2, et2.Kind()))
		//TODO- 		}
		//TODO- 	default:
		//TODO- 		panic(todo("", p.pos(n), et, et.Kind()))
		//TODO- 	}
		//TODO- default:
		//TODO- 	panic(todo("", p.pos(n), ot, ot.Kind()))
		//TODO- }
	case cc.UnaryExpressionPlus: // '+' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionMinus: // '-' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionCpl: // '~' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionNot: // '!' CastExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", p.pos(n)))
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
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", p.pos(n), ot, ot.Kind()))
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
		p.w("_ = ")
		switch {
		case n.CastExpression.Operand.Type().Kind() == cc.Array:
			panic(todo("", p.pos(n)))
		default:
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprAddrOf, flags|fOutermost)
		}
	case cc.UnaryExpressionDeref: // '*' CastExpression
		p.w("_ = *(*byte)(unsafe.Pointer(")
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags|fOutermost)
		p.w("))")
	case
		cc.UnaryExpressionPlus,  // '+' CastExpression
		cc.UnaryExpressionMinus, // '-' CastExpression
		cc.UnaryExpressionNot,   // '!' CastExpression
		cc.UnaryExpressionCpl:   // '~' CastExpression

		p.w("_ = ")
		defer p.w("%s", p.convert(n, n.CastExpression.Operand, p.intType, flags|fOutermost))
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags|fOutermost)
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		// nop
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		// nop
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
		switch {
		case n.CastExpression.Operand.Type().Kind() == cc.Array:
			panic(todo("", p.pos(n)))
		default:
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprAddrOf, flags&^fOutermost)
		}
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
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", p.pos(n), ot, ot.Kind()))
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
			defer p.w("%s", p.convert(n, n.CastExpression.Operand, t, flags))
			p.w("%sNeg%s(", p.task.crt, p.helperType(n, n.CastExpression.Operand.Type()))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), exprValue, flags)
			p.w(")")
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
				if n.CastExpression.Operand.IsZero() {
					p.w("^%s(0)", p.typ(n, t))
					break
				}

				p.w("^%s(", p.helperType2(n, n.CastExpression.Operand.Type(), t))
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
		defer p.w("%s", p.convert(n, nil, t, flags))
		if d := n.UnaryExpression.Declarator(); d != nil {
			var isLocal bool
			if f != nil {
				if local := f.locals[d]; local != nil {
					isLocal = true
					if !local.isPinned {
						p.w("unsafe.Sizeof(%s)", local.name)
						return
					}
				}
			}

			if !isLocal {
				if tld := p.tlds[d]; tld != nil {
					p.w("unsafe.Sizeof(%s)", tld.name)
					break
				}

				nm := d.Name().String()
				if imp := p.imports[nm]; imp != nil {
					imp.used = true
					p.w("unsafe.Sizeof(%sX%s)", imp.qualifier, nm)
					break
				}
			}
		}

		t := n.UnaryExpression.Operand.Type()
		if p.isArray(f, n.UnaryExpression, t) {
			p.w("%d", t.Len()*t.Elem().Size())
			break
		}

		s := "(0)"
		if !t.IsArithmeticType() {
			switch t.Kind() {
			case cc.Ptr:
				// ok
			case cc.Struct, cc.Union, cc.Array:
				s = "{}"
			default:
				panic(todo("", t.Kind()))
			}
		}
		p.w("unsafe.Sizeof(%s%s)", p.typ(n, t), s)
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		defer p.w("%s", p.convert(n, nil, t, flags))
		t := n.TypeName.Type()
		if t.Kind() == cc.Array {
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
		p.w("unsafe.Sizeof(%s%s)", p.typ(n, t), s)
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		if n.TypeName.Type().Kind() == cc.Void {
			p.intConst(n, "", n.Operand, t, flags)
			break
		}

		defer p.w("%s", p.convert(n, nil, t, flags))
		t := n.UnaryExpression.Operand.Type()
		if p.isArray(f, n.UnaryExpression, t) {
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
		p.w("unsafe.Alignof(%s%s)", p.typ(n, t), s)
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		if n.TypeName.Type().Kind() == cc.Void {
			p.intConst(n, "", n.Operand, t, flags)
			break
		}

		defer p.w("%s", p.convert(n, nil, t, flags))
		t := n.TypeName.Type()
		if t.Kind() == cc.Array {
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
		p.w("unsafe.Alignof(%s%s)", p.typ(n, t), s)
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
		panic(todo("", p.pos(n)))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", p.pos(n)))
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
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", p.pos(n), ot, ot.Kind()))
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
		panic(todo("", p.pos(n), mode))
	}
}

func (p *project) unaryExpressionPreIncDecVoid(f *function, n *cc.UnaryExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionPreIncDecVoidNormal(f, n, oper, oper2, t, mode, flags)
	case opArrayParameter:
		p.unaryExpressionPreIncDecVoidArrayParameter(f, n, oper, oper2, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionPreIncDecVoidArrayParameter(f *function, n *cc.UnaryExpression, oper, oper2 string, t cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	ut := n.UnaryExpression.Operand.Type()
	p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), exprLValue, flags)
	switch d := p.incDelta(n, ut); d {
	case 1:
		p.w("%s", oper)
	default:
		p.w("%s %d", oper2, d)
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

	panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
		p.unaryExpressionDerefLValueArray(f, n, t, mode, flags)
	case opArrayParameter:
		p.unaryExpressionDerefLValueNormal(f, n, t, mode, flags)
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
	case opNormal, opArrayParameter:
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
	case exprDecay:
		p.postfixExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) postfixExpressionDecay(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		pe := n.PostfixExpression.Operand.Type()
		p.w("(")
		switch {
		case pe.Kind() == cc.Array:
			p.postfixExpression(f, n.PostfixExpression, pe, exprDecay, flags&^fOutermost)
		case pe.Kind() == cc.Ptr:
			p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags&^fOutermost)
		default:
			panic(todo("", p.pos(n)))
		}
		p.w(" + ")
		p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w(")")
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpression(f, n, t, exprAddrOf, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpression(f, n, t, exprAddrOf, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", p.pos(n)))
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
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
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, exprValue, flags)
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
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*(**%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type().Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
		p.w("/* .%s */", p.fieldName(n, n.Token2.Value))
		p.w(")))")
	}
}

func (p *project) postfixExpressionPSelectSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", p.pos(n)))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, t.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags)
		p.w(".%s", p.fieldName(n, n.Token2.Value))
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
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionSelectSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionSelectPSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, t.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprPSelect, flags)
		p.w(".%s", p.fieldName(n, n.Token2.Value))
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
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w(")).%s", p.fieldName(n, n.Token2.Value))
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
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n, n.Token2.Value))
	}
}

func (p *project) postfixExpressionSelectSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionAddrOfSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionAddrOfPSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		panic(todo("", p.pos(n)))
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
	case n.Operand.Type().IsBitFieldType():
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags|fOutermost)
		p.bitFldOff(pe.Elem(), n.Token2)
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
	case n.Operand.Type().IsBitFieldType():
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, nil, mode, flags|fOutermost)
		p.bitFldOff(pe, n.Token2)
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
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		switch n.Operand.Type().Kind() {
		case cc.Ptr:
			switch et := n.Operand.Type().Elem(); et.Kind() {
			case cc.Function:
				p.w("(*(*")
				p.functionSignature(f, n.Operand.Type().Elem(), "")
				p.w(")(unsafe.Pointer(")
				p.postfixExpressionCall(f, n, t, exprValue, flags)
				p.w(")))")
			default:
				panic(todo("", p.pos(n), et, et.Kind()))
			}
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
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
					panic(todo("", p.pos(n), et, et.Kind()))
				}
			default:
				panic(todo("", p.pos(n), n.Operand.Type(), n.Operand.Type().Kind()))
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
					panic(todo("", p.pos(n), et, et.Kind()))
				}
			default:
				panic(todo("", p.pos(n), n.Operand.Type(), n.Operand.Type().Kind()))
			}
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionVoid(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.w("_ = ")
		p.postfixExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, n.Operand.Type(), mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.w("_ = ")
		p.postfixExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.w("_ = ")
		p.postfixExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionValue(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		if p.isArray(f, n.PrimaryExpression, n.Operand.Type()) && t.Kind() == cc.Ptr {
			mode = exprDecay
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
		panic(todo("", p.pos(n), n.TypeName.Type(), n.TypeName.Type().Alias()))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		// Built-in Function: int __builtin_types_compatible_p (type1, type2) You can
		// use the built-in function __builtin_types_compatible_p to determine whether
		// two types are the same.
		//
		// This built-in function returns 1 if the unqualified versions of the types
		// type1 and type2 (which are types, not expressions) are compatible, 0
		// otherwise. The result of this built-in function can be used in integer
		// constant expressions.
		//
		// This built-in function ignores top level qualifiers (e.g., const, volatile).
		// For example, int is equivalent to const int.
		//
		// The type int[] and int[5] are compatible. On the other hand, int and char *
		// are not compatible, even if the size of their types, on the particular
		// architecture are the same. Also, the amount of pointer indirection is taken
		// into account when determining similarity. Consequently, short * is not
		// similar to short **. Furthermore, two types that are typedefed are
		// considered compatible if their underlying types are compatible.
		//
		// An enum type is not considered to be compatible with another enum type even
		// if both are compatible with the same integer type; this is what the C
		// standard specifies. For example, enum {foo, bar} is not similar to enum
		// {hot, dog}.
		//
		// You typically use this function in code whose execution varies depending on
		// the arguments’ types. For example:
		//
		//	#define foo(x)                                                  \
		//	  ({                                                           \
		//	    typeof (x) tmp = (x);                                       \
		//	    if (__builtin_types_compatible_p (typeof (x), long double)) \
		//	      tmp = foo_long_double (tmp);                              \
		//	    else if (__builtin_types_compatible_p (typeof (x), double)) \
		//	      tmp = foo_double (tmp);                                   \
		//	    else if (__builtin_types_compatible_p (typeof (x), float))  \
		//	      tmp = foo_float (tmp);                                    \
		//	    else                                                        \
		//	      abort ();                                                 \
		//	    tmp;                                                        \
		//	  })
		//
		// Note: This construct is only available for C.
		p.w(" %d ", n.Operand.Value())
	case cc.PostfixExpressionChooseExpr: // "__builtin_choose_expr" '(' AssignmentExpression ',' AssignmentExpression ',' AssignmentExpression ')'
		switch op := n.AssignmentExpression.Operand; {
		case op.IsNonZero():
			p.assignmentExpression(f, n.AssignmentExpression2, t, mode, flags)
		case op.IsZero():
			p.assignmentExpression(f, n.AssignmentExpression3, t, mode, flags)
		default:
			panic(todo(""))
		}
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
		panic(todo("", p.pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo("", p.pos(n)))
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w("/* .%s */", p.fieldName(n, n.Token2.Value))
		p.w("))")
	}
}

func (p *project) postfixExpressionValuePSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type())
	switch {
	case n.Operand.Type().IsBitFieldType():
		p.w("(")
		defer p.w(")")
		fld := n.Field
		defer p.w("%s", p.convertType(n, fld.Promote(), t, flags))
		switch pe.Kind() {
		case cc.Array:
			x := p.convertType(n, nil, fld.Promote(), flags)
			p.w("*(*uint%d)(unsafe.Pointer(", fld.BitFieldBlockWidth())
			p.postfixExpression(f, n.PostfixExpression, pe, exprDecay, flags)
			p.bitFldOff(pe.Elem(), n.Token2)
			p.w("))")
			p.w("&%#x>>%d%s", fld.Mask(), fld.BitFieldOffset(), x)
			if fld.Type().IsSignedType() {
				panic(todo(""))
				p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
			}
		default:
			x := p.convertType(n, nil, fld.Promote(), flags)
			p.w("*(*uint%d)(unsafe.Pointer(", fld.BitFieldBlockWidth())
			p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
			p.bitFldOff(pe.Elem(), n.Token2)
			p.w("))&%#x>>%d%s", fld.Mask(), fld.BitFieldOffset(), x)
			if fld.Type().IsSignedType() {
				p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
			}
		}
	case n.Operand.Type().Kind() == cc.Array:
		defer p.w("%s", p.convertType(n, n.Operand.Type().Decay(), t.Decay(), flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.fldOff(n.PostfixExpression.Operand.Type().Elem(), n.Token2)
	case k == opArray:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w("[0].%s", p.fieldName(n, n.Token2.Value))
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w(")).%s", p.fieldName(n, n.Token2.Value))
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
			p.postfixExpression(f, n.PostfixExpression, pe, exprDecay, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		default:
			panic(todo("", p.pos(n), pe))
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
		p.postfixExpression(f, n.PostfixExpression, pe, exprDecay, flags|fOutermost)
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
	fld := n.Field
	switch {
	case n.Operand.Type().IsBitFieldType():
		p.w("(")
		defer p.w("%s)", p.convertType(n, fld.Promote(), t, flags))
		x := p.convertType(n, nil, fld.Promote(), flags)
		p.w("*(*uint%d)(unsafe.Pointer(", fld.BitFieldBlockWidth())
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
		p.bitFldOff(pe, n.Token2)
		p.w("))&%#x>>%d%s", fld.Mask(), fld.BitFieldOffset(), x)
		if fld.Type().IsSignedType() {
			p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
		}
	case n.Operand.Type().Kind() == cc.Array:
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags|fOutermost)
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
	case n.Operand.Type().IsBitFieldType():
		p.w("(")
		defer p.w("%s)", p.convertType(n, fld.Promote(), t, flags))
		x := p.convertType(n, nil, fld.Promote(), flags)
		p.w("*(*uint%d)(unsafe.Pointer(", fld.BitFieldBlockWidth())
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
		p.bitFldOff(pe, n.Token2)
		p.w("))&%#x>>%d%s", fld.Mask(), fld.BitFieldOffset(), x)
		if fld.Type().IsSignedType() {
			p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
		}
	case n.Operand.Type().Kind() == cc.Array:
		p.postfixExpression(f, n, t, exprDecay, flags)
	case fld.InUnion():
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, fld.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprAddrOf, flags)
		p.fldOff(pe, n.Token2)
		p.w("))")
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n, n.Token2.Value))
	}
}

func (p *project) postfixExpressionLValue(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionLValueIndex(f, n, t, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionLValueSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionLValuePSelect(f, n, t, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo("", p.pos(n)))
	case cc.PostfixExpressionChooseExpr:
		panic(todo("", p.pos(n)))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionLValuePSelect(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	pe := n.PostfixExpression
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opStruct:
		if !p.inUnion(n, pe.Operand.Type().Elem(), n.Token2.Value) {
			p.postfixExpressionLValuePSelectStruct(f, n, t, mode, flags)
			break
		}

		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, pe, pe.Operand.Type(), exprValue, flags|fOutermost)
		p.fldOff(pe.Operand.Type().Elem(), n.Token2)
		p.w("))")
	case opUnion:
		p.postfixExpressionLValuePSelectUnion(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionLValuePSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", p.pos(n)))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w("/* .%s */", p.fieldName(n, n.Token2.Value))
		p.w(")))")
	}
}

func (p *project) postfixExpressionLValuePSelectStruct(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type())
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", p.pos(n)))
	case k == opArray:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w("[0].%s", p.fieldName(n, n.Token2.Value))
	default:
		defer p.w("%s", p.convert(n, n.Operand, t, flags))
		p.w("(*%s)(unsafe.Pointer(", p.typ(n, pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, exprValue, flags)
		p.w(")).%s", p.fieldName(n, n.Token2.Value))
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
		panic(todo("", p.pos(n)))
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
			p.postfixExpression(f, n.PostfixExpression, pe, exprDecay, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(n, func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		default:
			panic(todo("", p.pos(n), pe))
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
	pe := n.PostfixExpression
	switch k := p.opKind(f, pe, pe.Operand.Type()); k {
	case opStruct:
		if !p.inUnion(n, pe.Operand.Type(), n.Token2.Value) {
			p.postfixExpressionLValueSelectStruct(f, n, t, mode, flags)
			break
		}

		p.w("*(*%s)(unsafe.Pointer(", p.typ(n, n.Operand.Type()))
		p.postfixExpression(f, pe, pe.Operand.Type(), exprAddrOf, flags|fOutermost)
		p.fldOff(pe.Operand.Type(), n.Token2)
		p.w("))")
	case opUnion:
		p.postfixExpressionLValueSelectUnion(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) inUnion(n cc.Node, t cc.Type, fname cc.StringID) bool {
	f, ok := t.FieldByName(fname)
	if !ok {
		p.err(n, "unknown field: %s", fname)
		return false
	}

	return f.InUnion()
}

func (p *project) postfixExpressionLValueSelectUnion(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case pe.Kind() == cc.Array:
		panic(todo("", p.pos(n)))
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", p.pos(n)))
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
		panic(todo("", p.pos(n)))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n, n.Token2.Value))
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
	p.w(", %d)", p.incDelta(n.PostfixExpression, pe))
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
	p.w(", %d, %d, %d, %#x)", p.incDelta(n.PostfixExpression, pe), bf.BitFieldBlockWidth(), bf.BitFieldOffset(), bf.Mask())
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

func (p *project) bitFldOff(t cc.Type, tok cc.Token) {
	var off uintptr
	fld, ok := t.FieldByName(tok.Value)
	switch {
	case ok && !fld.IsBitField():
		panic(todo("%v: ICE: bitFdlOff must not be used with non bit fields", origin(2)))
	case !ok:
		p.err(&tok, "uknown field: %s", tok.Value)
	default:
		off = fld.BitFieldBlockFirst().Offset()
	}
	if off != 0 {
		p.w("+%d", off)
	}
	p.w("/* &.%s */", tok.Value)
}

func (p *project) fldOff(t cc.Type, tok cc.Token) {
	var off uintptr
	fld, ok := t.FieldByName(tok.Value)
	switch {
	case ok && fld.IsBitField():
		panic(todo("%v: ICE: fdlOff must not be used with bit fields", origin(2)))
	case !ok:
		p.err(&tok, "uknown field: %s", tok.Value)
	default:
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
		case idAtomicLoadN:
			p.atomicLoadN(f, n, t, mode, flags)
			return
		case idBuiltinConstantPImpl:
			p.w("%v", n.Operand.Value())
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
		case idAtomicLoadN:
			p.atomicLoadN(f, n, t, mode, flags)
			return
		case idAddOverflow:
			p.addOverflow(f, n, t, mode, flags)
			return
		case idSubOverflow:
			p.subOverflow(f, n, t, mode, flags)
			return
		case idMulOverflow:
			p.mulOverflow(f, n, t, mode, flags)
			return
		case idBuiltinConstantPImpl:
			p.w("%v", n.Operand.Value())
			return
		}
	}
	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprFunc, flags&^fOutermost)
	p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
}

// bool __builtin_mul_overflow (type1 a, type2 b, type3 *res)
func (p *project) mulOverflow(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	args := p.argList(n.ArgumentExpressionList)
	if len(args) != 3 {
		p.err(n, "expected 3 arguments in call to __builtin_mul_overflow")
		return
	}

	pt := args[2].Operand.Type()
	if pt.Kind() != cc.Ptr {
		p.err(n, "invalid argument of __builtin_mul_overflow (expected pointer): %s", pt)
		return
	}

	vt := pt.Elem()
	switch {
	case vt.IsIntegerType():
		switch vt.Size() {
		case 1, 2, 4, 8:
			p.w("%sX__builtin_mul_overflow%s", p.task.crt, p.helperType(n, vt))
		default:
			p.err(n, "invalid argument of __builtin_mul_overflow: %v, elem kind %v", pt, vt.Kind())
			return
		}
		p.w("(%s", f.tlsName)
		types := []cc.Type{vt, vt, pt}
		for i, v := range args[:3] {
			p.w(", ")
			p.assignmentExpression(f, v, types[i], exprValue, flags|fOutermost)
		}
		p.w(")")
		return
	}

	p.err(n, "invalid arguments of __builtin_mul_overflow: (%v, %v, %v)", args[0].Operand.Type(), args[1].Operand.Type(), args[2].Operand.Type())
	return
}

// bool __builtin_sub_overflow (type1 a, type2 b, type3 *res)
func (p *project) subOverflow(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	args := p.argList(n.ArgumentExpressionList)
	if len(args) != 3 {
		p.err(n, "expected 3 arguments in call to __builtin_sub_overflow")
		return
	}

	pt := args[2].Operand.Type()
	if pt.Kind() != cc.Ptr {
		p.err(n, "invalid argument of __builtin_sub_overflow (expected pointer): %s", pt)
		return
	}

	vt := pt.Elem()
	switch {
	case vt.IsIntegerType():
		switch vt.Size() {
		case 1, 2, 4, 8:
			p.w("%sX__builtin_sub_overflow%s", p.task.crt, p.helperType(n, vt))
		default:
			p.err(n, "invalid argument of __builtin_sub_overflow: %v, elem kind %v", pt, vt.Kind())
			return
		}
		p.w("(%s", f.tlsName)
		types := []cc.Type{vt, vt, pt}
		for i, v := range args[:3] {
			p.w(", ")
			p.assignmentExpression(f, v, types[i], exprValue, flags|fOutermost)
		}
		p.w(")")
		return
	}

	p.err(n, "invalid arguments of __builtin_sub_overflow: (%v, %v, %v)", args[0].Operand.Type(), args[1].Operand.Type(), args[2].Operand.Type())
	return
}

// bool __builtin_add_overflow (type1 a, type2 b, type3 *res)
func (p *project) addOverflow(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	args := p.argList(n.ArgumentExpressionList)
	if len(args) != 3 {
		p.err(n, "expected 3 arguments in call to __builtin_add_overflow")
		return
	}

	pt := args[2].Operand.Type()
	if pt.Kind() != cc.Ptr {
		p.err(n, "invalid argument of __builtin_add_overflow (expected pointer): %s", pt)
		return
	}

	vt := pt.Elem()
	switch {
	case vt.IsIntegerType():
		switch vt.Size() {
		case 1, 2, 4, 8:
			p.w("%sX__builtin_add_overflow%s", p.task.crt, p.helperType(n, vt))
		default:
			p.err(n, "invalid argument of __builtin_add_overflow: %v, elem kind %v", pt, vt.Kind())
			return
		}
		p.w("(%s", f.tlsName)
		types := []cc.Type{vt, vt, pt}
		for i, v := range args[:3] {
			p.w(", ")
			p.assignmentExpression(f, v, types[i], exprValue, flags|fOutermost)
		}
		p.w(")")
		return
	}

	p.err(n, "invalid arguments of __builtin_add_overflow: (%v, %v, %v)", args[0].Operand.Type(), args[1].Operand.Type(), args[2].Operand.Type())
	return
}

// type __atomic_load_n (type *ptr, int memorder)
func (p *project) atomicLoadN(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	args := p.argList(n.ArgumentExpressionList)
	if len(args) != 2 {
		p.err(n, "expected 2 arguments in call to __atomic_load_n")
		return
	}

	pt := args[0].Operand.Type()
	if pt.Kind() != cc.Ptr {
		p.err(n, "invalid argument of __atomic_load_n (expected pointer): %s", pt)
		return
	}

	vt := pt.Elem()
	switch {
	case vt.IsIntegerType():
		var s string
		switch {
		case vt.IsSignedType():
			s = "Int"
		default:
			s = "Uint"
		}
		switch vt.Size() {
		case 2, 4, 8:
			p.w("%sAtomicLoadN%s%d", p.task.crt, s, 8*vt.Size())
		default:
			p.err(n, "invalid argument of __atomic_load_n: %v, elem kind %v", pt, vt.Kind())
			return
		}
		types := []cc.Type{pt, p.intType}
		p.w("(")
		for i, v := range args[:2] {
			if i != 0 {
				p.w(", ")
			}
			p.assignmentExpression(f, v, types[i], exprValue, flags|fOutermost)
		}
		p.w(")")
		return
	case vt.Kind() == cc.Ptr:
		panic(todo("", pt, vt))
	}

	p.err(n, "invalid first argument of __atomic_load_n: %v, elem kind %v", pt, vt.Kind())
	return
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
		case idAtomicStoreN:
			p.atomicStoreN(f, n, t, mode, flags)
			return
		}
	}
	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), exprFunc, flags&^fOutermost)
	p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
}

// void __atomic_store_n (type *ptr, type val, int memorder)
func (p *project) atomicStoreN(f *function, n *cc.PostfixExpression, t cc.Type, mode exprMode, flags flags) {
	args := p.argList(n.ArgumentExpressionList)
	if len(args) != 3 {
		p.err(n, "expected 3 arguments in call to __atomic_store_n")
		return
	}

	pt := args[0].Operand.Type()
	if pt.Kind() != cc.Ptr {
		p.err(n, "invalid first argument of __atomic_store_n (expected pointer): %s", pt)
		return
	}

	vt := args[1].Operand.Type()
	switch {
	case vt.IsIntegerType():
		var s string
		switch {
		case vt.IsSignedType():
			s = "Int"
		default:
			s = "Uint"
		}
		switch vt.Size() {
		case 2, 4, 8:
			p.w("%sAtomicStoreN%s%d", p.task.crt, s, 8*vt.Size())
		default:
			p.err(n, "invalid arguments of __atomic_store_n: (%v, %v), element kind %v", pt, vt, vt.Kind())
			return
		}
		p.w("(")
		types := []cc.Type{pt, vt, p.intType}
		for i, v := range args[:3] {
			if i != 0 {
				p.w(", ")
			}
			if i == 1 {
				p.w("%s(", strings.ToLower(p.helperType(n, vt)))
			}
			p.assignmentExpression(f, v, types[i], exprValue, flags|fOutermost)
			if i == 1 {
				p.w(")")
			}
		}
		p.w(")")
		return
	case vt.Kind() == cc.Ptr:
		panic(todo("", pt, vt))
	}

	p.err(n, "invalid arguments of __atomic_store_n: (%v, %v), element kind %v", pt, vt, vt.Kind())
	return
}

func (p *project) argList(n *cc.ArgumentExpressionList) (r []*cc.AssignmentExpression) {
	for ; n != nil; n = n.ArgumentExpressionList {
		r = append(r, n.AssignmentExpression)
	}
	return r
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
		panic(todo("", p.pos(n)))
	}

	va := true
	if len(args) > len(params) && !isVariadic {
		var a []string
		for _, v := range args {
			a = append(a, v.Operand.Type().String())
		}
		sargs := strings.Join(a, ",")
		switch d := pe.Declarator(); {
		case d == nil:
			p.err(pe, "too many arguments (%s) in call to %s", sargs, ft)
		default:
			p.err(pe, "too many arguments (%s) in call to %s of type %s", sargs, d.Name(), ft)
		}
		va = false
	}

	paren := ""
	for i, arg := range args {
		p.w(",%s", tidyComment(" ", arg))
		mode := exprValue
		if at := arg.Operand.Type(); at.Kind() == cc.Array {
			mode = exprDecay
		}
		switch {
		case i < len(params):
			p.assignmentExpression(f, arg, arg.Promote(), mode, fOutermost)
		case va && i == len(params):
			p.w("%sVaList(%s%s, ", p.task.crt, f.bpName, nonZeroUintptr(bpOff))
			paren = ")"
			fallthrough
		default:
			flags := fOutermost
			if arg.Promote().IsIntegerType() {
				switch x := arg.Operand.Value().(type) {
				case cc.Int64Value:
					if x < mathutil.MinInt || x > mathutil.MaxInt {
						flags |= fForceConv
					}
				case cc.Uint64Value:
					if x > mathutil.MaxInt {
						flags |= fForceConv
					}
				}
			}
			p.assignmentExpression(f, arg, arg.Promote(), mode, flags)
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

	panic(todo("", p.pos(n)))
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
	case exprDecay:
		p.primaryExpressionDecay(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) primaryExpressionDecay(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(n, f, d, t, mode, flags)
		default:
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n, n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		p.w("%s", p.wideStringLiteral(n.Operand.Value(), 0))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.expression(f, n.Expression, t, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionVoid(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {

	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		p.w("_ = ")
		p.primaryExpression(f, n, n.Operand.Type(), exprValue, flags|fOutermost)
	case cc.PrimaryExpressionInt, // INTCONST
		cc.PrimaryExpressionFloat,   // FLOATCONST
		cc.PrimaryExpressionEnum,    // ENUMCONST
		cc.PrimaryExpressionChar,    // CHARCONST
		cc.PrimaryExpressionLChar,   // LONGCHARCONST
		cc.PrimaryExpressionString,  // STRINGLITERAL
		cc.PrimaryExpressionLString: // LONGSTRINGLITERAL

		// nop
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.expression(f, n.Expression, n.Expression.Operand.Type(), mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.compoundStatement(f, n.CompoundStatement, "", true, false, 0)
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
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n, n.Token.Src.String(), n.Operand, n.Operand.Type(), flags)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w(" 1 ")
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.w("(")
		defer p.w(")")
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.w("func() %v {", p.typ(n, n.CompoundStatement.Operand.Type()))
		p.compoundStatement(f, n.CompoundStatement, "", true, false, exprValue)
		p.w("}()")
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
				panic(todo("", p.pos(n)))
				p.primaryExpression(f, n, t, exprDecay, flags)
			default:
				p.declarator(n, f, d, t, mode, flags)
			}
		default:
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", p.pos(n)))
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
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", p.pos(n)))
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
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", p.pos(n)))
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
					panic(todo("", p.pos(n), p.pos(d), d.Type(), d.Type().Kind()))
				}
			default:
				panic(todo("", p.pos(n), p.pos(d), d.Type(), d.Type().Kind()))
			}
		default:
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", p.pos(n)))
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
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n, n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionFloat: // FLOATCONST
		p.floatConst(n, n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionEnum: // ENUMCONST
		en := n.ResolvedTo().(*cc.Enumerator)
		if n.ResolvedIn().Parent() == nil {
			if nm := p.enumConsts[en.Token.Value]; nm != "" {
				p.w(" %s ", nm)
				break
			}
		}

		p.intConst(n, "", n.Operand, t, flags)
		p.w("/* %s */", en.Token.Value)
	case cc.PrimaryExpressionChar: // CHARCONST
		p.charConst(n, n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		p.charConst(n, n.Token.Src.String(), n.Operand, t, flags)
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		p.w("%s", p.wideStringLiteral(n.Operand.Value(), 0))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.expression(f, n.Expression, t, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.statementExpression(f, n.CompoundStatement, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) statementExpression(f *function, n *cc.CompoundStatement, t cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.convert(n, n.Operand, t, flags))
	p.w(" func() %v {", p.typ(n, n.Operand.Type()))
	p.compoundStatement(f, n, "", true, false, mode)
	p.w("}()")
}

func (p *project) primaryExpressionLValue(f *function, n *cc.PrimaryExpression, t cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(n, f, d, t, mode, flags)
		default:
			panic(todo("", p.pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionFloat: // FLOATCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionEnum: // ENUMCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionChar: // CHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLChar: // LONGCHARCONST
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		panic(todo("", p.pos(n)))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo("", p.pos(n)))
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
	if p.pass1 {
		return ""
	}

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
	if p.pass1 {
		return ""
	}

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

func (p *project) wideStringLiteral(v cc.Value, pad int) string {
	if p.pass1 {
		return ""
	}

	switch x := v.(type) {
	case cc.WideStringValue:
		id := cc.StringID(x)
		off, ok := p.tsWOffs[id]
		if !ok {
			off = p.wcharSize * uintptr(len(p.tsW))
			s := []rune(id.String())
			if pad != 0 {
				s = append(s, make([]rune, pad)...)
			}
			p.tsW = append(p.tsW, s...)
			p.tsW = append(p.tsW, 0)
			p.tsWOffs[id] = off
		}
		return fmt.Sprintf("%s%s", p.tsWNameP, nonZeroUintptr(off))
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
			panic(todo("", p.pos(n)))
		}

		on = uint64(x)
	case cc.Uint64Value:
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
				if s := p.float2Int(n, x, to); s != "" {
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

func (p *project) float2Int(n cc.Node, x cc.Float32Value, to cc.Type) string {
	switch {
	case to.IsSignedType():
		limits := &signedSaturationLimits[to.Size()]
		v := float64(x)
		switch {
		case math.IsNaN(v):
			panic(todo("", p.pos(n)))
		case math.IsInf(v, -1):
			panic(todo("", p.pos(n)))
		case math.IsInf(v, 1):
			panic(todo("", p.pos(n)))
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
			panic(todo("", p.pos(n)))
		case math.IsInf(v, -1):
			panic(todo("", p.pos(n)))
		case math.IsInf(v, 1):
			panic(todo("", p.pos(n)))
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
		// p.w("/*10568 %T(%#[1]x) %v -> %v */", op.Value(), op.Type(), to) //TODO-
		if flags&fForceNoConv != 0 {
			break
		}

		if !op.Type().IsSignedType() && op.Type().Size() == 8 && op.Value().(cc.Uint64Value) > math.MaxInt64 {
			flags |= fForceRuntimeConv
		}
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
		panic(todo("", p.pos(n)))
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
			panic(todo("", p.pos(n), lhs.Operand.Type()))
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
	case opBitfield:
		p.assignOpValueBitfield(f, n, t, oper, oper2, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignOpValueBitfield(f *function, n *cc.AssignmentExpression, t cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.

	asInt := oper2 == "Shl" || oper2 == "Shr"
	if asInt {
		panic(todo(""))
	}

	ot := n.Operand.Type()
	lhs := n.UnaryExpression
	bf := lhs.Operand.Type().BitField()
	defer p.w("%s", p.convertType(n, ot, t, flags))
	p.w(" func() %v {", p.typ(n, ot))
	switch lhs.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		pe := n.UnaryExpression.PostfixExpression
		switch pe.Case {
		case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
			p.w("__p := ")
			p.postfixExpression(f, pe, pe.Operand.Type(), exprAddrOf, flags|fOutermost)
			p.w("; __v := ")
			p.readBitfield(lhs, "__p", bf, ot)
			p.w(" %s (", oper)
			p.assignmentExpression(f, n.AssignmentExpression, ot, exprValue, flags|fOutermost)
			p.w("); return %sAssignBitFieldPtr%d%s(__p, __v, %d, %d, %#x)", p.task.crt, bf.BitFieldBlockWidth(), p.bfHelperType(ot), bf.BitFieldWidth(), bf.BitFieldOffset(), bf.Mask())
		case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
			panic(todo("", p.pos(n)))
		default:
			panic(todo("", n.Position(), pe.Case))
		}
	default:
		panic(todo("", n.Position(), lhs.Case))
	}
	p.w("}()")
}

func (p *project) readBitfield(n cc.Node, ptr string, bf cc.Field, promote cc.Type) {
	bw := bf.BitFieldBlockWidth()
	m := bf.Mask()
	o := bf.BitFieldOffset()
	w := bf.BitFieldWidth()
	p.w("(%s(*(*uint%d)(unsafe.Pointer(%s))&%#x)", p.typ(n, promote), bw, ptr, m)
	switch {
	case bf.Type().IsSignedType():
		bits := int(promote.Size()) * 8
		p.w("<<%d>>%d)", bits-w-o, bits-w)
	default:
		p.w(">>%d)", o)
	}
}

func (p *project) assignOpValueNormal(f *function, n *cc.AssignmentExpression, t cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	asInt := oper2 == "Shl" || oper2 == "Shr"
	lhs := n.UnaryExpression
	// UnaryExpression "*=" AssignmentExpression etc.
	if d := lhs.Declarator(); d != nil {
		if local := f.locals[d]; local != nil && local.isPinned {
			switch {
			case lhs.Operand.Type().IsArithmeticType():
				defer p.w("%s", p.convertType(n, lhs.Operand.Type(), t, flags))
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
			default:
				panic(todo("", lhs.Operand.Type()))
			}
			return
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
			panic(todo("", p.pos(n), p.pos(d), d.Name()))
		}
		return
	}

	switch {
	case lhs.Operand.Type().IsArithmeticType():
		defer p.w("%s", p.convertType(n, lhs.Operand.Type(), t, flags))
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
		panic(todo("", p.pos(n)))
	}

	p.declarator(n, f, d, d.Type(), exprLValue, flags|fOutermost)
	if oper != "+" && oper != "-" {
		panic(todo("", p.pos(n)))
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
		case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
			switch d := pe.PostfixExpression.Declarator(); {
			case d != nil:
				panic(todo("", p.pos(n)))
			default:
				panic(todo("", p.pos(n)))
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
				panic(todo("", p.pos(n)))
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
			p.statement(f, n.Statement, false, false, false, 0)
			p.w("; goto __%d; __%d:", a, b)
			break
		}

		p.w("for ")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false, false, 0)
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
			p.statement(f, n.Statement, false, false, false, 0)
			p.w(";goto __%d; __%[1]d: if ", b)
			p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
			p.w("{goto __%d};goto __%d;__%[2]d:", a, c)
			break
		}

		v := "ok"
		if !p.pass1 {
			v = f.scope.take(cc.String(v))
		}
		p.w("for %v := true; %[1]v; %[1]v = ", v)
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false, false, 0)
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
			p.statement(f, n.Statement, false, false, false, 0)
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
		p.statement(f, n.Statement, true, false, false, 0)
	case cc.IterationStatementForDecl: // "for" '(' Declaration Expression ';' Expression ')' Statement
		if true || f.block.isFlat() {
			panic(todo("", p.pos(n)))
		}

		panic(todo("", p.pos(n)))
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
			p.statement(f, n.Statement, false, false, false, 0)
			p.w(";__%d: ", a)
			break
		}

		p.w("if ")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false, false, 0)
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
			p.statement(f, n.Statement, false, false, false, 0)
			p.w(";goto __%d; __%d:", b, a)
			p.statement(f, n.Statement2, false, false, false, 0)
			p.w(";__%d:", b)
			break
		}

		p.w("if ")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), exprBool, fOutermost)
		p.statement(f, n.Statement, true, false, false, 0)
		p.w(" else ")
		switch {
		case p.isIfStmt(n.Statement2):
			p.statement(f, n.Statement2, false, true, false, 0)
		default:
			p.statement(f, n.Statement2, true, false, false, 0)
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
		p.statement(f, n.Statement, true, false, true, 0)
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
		panic(todo("", p.pos(n)))
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
			panic(todo("", p.pos(n)))
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
	p.statement(f, n.Statement, false, true, false, 0)
	f.flatSwitchLabels = svLabels
	p.w("__%d:", f.breakCtx)
	f.breakCtx = svBreakCtx
}

func (p *project) expressionStatement(f *function, n *cc.ExpressionStatement) {
	p.w("%s", tidyComment("\n", n))
	// Expression AttributeSpecifierList ';'
	if n.Expression == nil {
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
		r = p.statement(f, n.Statement, false, false, false, 0)
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
		r = p.statement(f, n.Statement, false, false, false, 0)
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
	p.statement(f, n.Statement, false, false, false, 0)
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
