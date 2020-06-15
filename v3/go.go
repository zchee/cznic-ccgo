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
	exprAddrOf              // &foo as uinptr
	exprBool                // foo != 0
	exprFunc                // foo in foo(bar)
	exprLValue              // foo in foo = bar
	exprPSelect             // foo in foo->bar
	exprSelect              // foo in foo.bar
	exprValue               // foo
	exprVoid                //
	exprVoidSingle          // C: a, b -> Go: func() { a; b }()
)

type opKind int

const (
	opNormal opKind = iota
	opArray
	opArrayParameter
	opFunction
	opUnion
	opBitfield
)

type flags byte

const (
	fOutermost flags = 1 << iota
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

	forceRead bool // Possibly never read.
	isPinned  bool // Prevent this local from being placed in Go movable stack.
}

type switchState int

const (
	_                 switchState = iota // Not in switch.
	inSwitchFirst                        // Before seeing "case/default".
	inSwitchCase                         // Seen "case/default".
	inSwitchSeenBreak                    // In switch "case/default" and seen "break/return".
)

type function struct {
	block        *block
	blocks       map[*cc.CompoundStatement]*block
	bpName       string
	breakCtx     int //TODO merge with continueCtx
	continueCtx  int
	flatLabels   int
	fndef        *cc.FunctionDefinition
	gen          *project
	ifCtx        cc.Node
	ignore       map[*cc.Declarator]bool // Pseudo declarators
	labelNames   map[cc.StringID]string
	labels       scope
	locals       map[*cc.Declarator]*local
	off          uintptr         // bp+off allocs
	params       []*cc.Parameter // May differ from what fndef says
	project      *project
	rt           cc.Type // May differ from what fndef says
	scope        scope
	switchCtx    switchState
	tlsName      string
	unusedLabels map[cc.StringID]struct{}
	vaLists      map[*cc.PostfixExpression]uintptr
	vaName       string
	vaType       cc.Type

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
		case v.Type().Kind() == cc.Struct && structHasArray(v.Type()):
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
			case x.Linkage == cc.None && x.Type().Kind() == cc.Union:
				f.pin(x)
			case x.Linkage == cc.None && x.Type().Kind() == cc.Struct && structHasArray(x.Type()):
				f.pin(x)
			case x.Linkage == cc.None && x.Type().Kind() == cc.Array:
				t := x.Type().Elem()
				for ; t.Kind() == cc.Array; t = t.Elem() {
				}
				if t.Kind() == cc.Struct || t.Kind() == cc.Union {
					f.pin(x)
				}
			}
		case *cc.AssignmentExpression:
			switch x.Case {
			case cc.AssignmentExpressionAssign: // foo = bar;
				if d := x.AssignmentExpression.Declarator(); d != nil { // bar
					if f.project.isArrayDeclarator(d) {
						f.pin(d)
					}
				}
				lhs := x.UnaryExpression
				var d *cc.Declarator
				if t := lhs.Operand.Type(); t.IsBitFieldType() {
					switch lhs.Case {
					case cc.UnaryExpressionPostfix:
						switch pe := lhs.PostfixExpression; pe.Case {
						case cc.PostfixExpressionSelect:
							d = pe.PostfixExpression.Declarator()
						case cc.PostfixExpressionPSelect:
							// nop
						default:
							panic(todo("", pe.Case))
						}
					default:
						panic(todo("", lhs.Case))
					}
					if d != nil {
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

func structHasArray(t cc.Type) bool {
	for idx := []int{0}; idx[0] < t.NumField(); idx[0]++ {
		f := t.FieldByIndex(idx)
		switch f.Type().Kind() {
		case cc.Struct:
			if structHasArray(f.Type()) {
				return true
			}
		case cc.Array:
			return true
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
	block.scope.take(f.tlsName)
	if f.vaName != "" {
		block.scope.take(f.vaName)
	}
	for _, item := range work {
		d := item.d
		if !f.ignore[d] && d.IsStatic() {
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
	switch {
	case n == nil:
		p.errors.Add(token.Position{}, fmt.Sprintf(s, args...))
	default:
		p.errors.Add(token.Position(n.Position()), fmt.Sprintf(s, args...))
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

	return p.layoutStaticLocals()
}

func (p *project) layoutEnums() error {
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
	nf := t.NumField()
	idx := []int{0}
	var b strings.Builder
	switch t.Kind() {
	case cc.Struct:
		var m map[uintptr][]cc.Field
		b.WriteString("struct {")
		for idx[0] = 0; idx[0] < nf; idx[0]++ {
			f := t.FieldByIndex(idx)
			ft := f.Type()
			if f.IsBitField() {
				if m == nil {
					m = p.bitFields(t)
				}
				off := f.Offset()
				if m[off][0].Name() != f.Name() {
					continue
				}

				a := []string{fmt.Sprintf("%s %s: %d", f.Type(), f.Name(), f.BitFieldWidth())}
				for idx2 := []int{idx[0] + 1}; idx2[0] < nf; idx2[0]++ {
					f := t.FieldByIndex(idx2)
					if !f.IsBitField() || f.Offset() != off {
						break
					}

					a = append(a, fmt.Sprintf("%s %s: %d", f.Type(), f.Name(), f.BitFieldWidth()))
				}
				fmt.Fprintf(&b, "%s uint%d /* %s */;", p.bitFieldName(f), f.BitFieldBlockWidth(), strings.Join(a, ", "))
				continue
			}

			fmt.Fprintf(&b, "%s %s;", p.fieldName2(f), p.typ(ft))
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

func (p *project) bitFields(t cc.Type) (r map[uintptr][]cc.Field) {
	nf := t.NumField()
	for idx := []int{0}; idx[0] < nf; idx[0]++ {
		f := t.FieldByIndex(idx)
		if f.IsBitField() {
			if r == nil {
				r = map[uintptr][]cc.Field{}
			}
			off := f.Offset()
			r[off] = append(r[off], f)
		}
	}
	return r
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
		if t.Size() > 8 {
			p.err(nil, "unsupported C type: %v", t)
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
			p.err(nil, "unsupported C type: %v", t)
		}
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

			switch d.Type().Kind() {
			case cc.Struct, cc.Union:
				p.checkAlignAttr(d.Type())
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
	for _, v := range p.task.asts {
		p.oneAST(v)
	}
	sort.Slice(p.task.imports, func(i, j int) bool { return p.task.imports[i].path < p.task.imports[j].path })
	p.o(`import (
	"math"
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

var _ = math.Pi
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
				if local.forceRead && !local.isPinned {
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
	if local.forceRead && !local.isPinned {
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
		p.w("%s// var %s %s at %s%s, %d\n", sep, local.name, p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), d.Type().Size())
		return
	}

	p.w("%svar %s %s;", sep, local.name, p.typ(d.Type()))
}

func (p *project) declarator(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprLValue:
		p.declaratorLValue(f, d, t, s, mode, flags)
	case exprFunc:
		p.declaratorFunc(f, d, t, s, mode, flags)
	case exprValue:
		p.declaratorValue(f, d, t, s, mode, flags)
	case exprAddrOf:
		p.declaratorAddrOf(f, d, t)
	case exprSelect:
		p.declaratorSelect(f, d, s)
	case exprPSelect:
		p.declaratorPSelect(f, d, s)
	default:
		panic(todo("", mode))
	}
}

func (p *project) declaratorValue(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	switch k := p.declaratorKind(d); k {
	case opNormal:
		p.declaratorValueNormal(f, d, t, s, mode, flags)
	case opArray:
		p.declaratorValueArray(f, d, t, s, mode, flags)
	case opFunction:
		p.declarator(f, d, t, s, exprAddrOf, flags)
	case opUnion:
		p.declaratorValueUnion(f, d, t, s, mode, flags)
	case opArrayParameter:
		p.declaratorValueArrayParameter(f, d, t, s, mode, flags)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorValueArrayParameter(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(d.Type(), t, flags))
	}
	local := f.locals[d]
	if local.isPinned {
		p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
		return
	}

	p.w("%s", local.name)
	return
}

func (p *project) declaratorValueUnion(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(d.Type(), t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorValueArray(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	if t.IsIntegerType() {
		defer p.w("%s", p.convertType(nil, t, flags))
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		panic(todo("", imp.qualifier, nm))
	}
}

func (p *project) declaratorValueNormal(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(d.Type(), t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorFunc(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	switch k := p.declaratorKind(d); k {
	case opFunction:
		p.declaratorFuncFunc(f, d, t, s, exprValue, flags)
	case opNormal:
		p.declaratorFuncNormal(f, d, t, s, exprValue, flags)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorFuncNormal(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	u := d.Type()
	switch u.Kind() {
	case cc.Ptr:
		u = u.Elem()
		if u.Kind() == cc.Function {
			if local := f.locals[d]; local != nil {
				if local.isPinned {
					panic(todo(""))
				}

				p.w("(*(*")
				p.functionSignature(f, u, "")
				p.w(")(unsafe.Pointer(&%s)))", local.name)
				return
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
					p.err(d, "%v: undefined: %s", d.Position(), d.Name())
					return
				}
				panic(todo(""))
			}
		}

		panic(todo("", pos(d), u))
	default:
		panic(todo("", pos(d), u))
	}
}

func (p *project) declaratorFuncFunc(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			panic(todo(""))
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorLValue(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	switch k := p.declaratorKind(d); k {
	case opNormal, opArrayParameter:
		p.declaratorLValueNormal(f, d, t, s, mode, flags)
	case opArray:
		p.declaratorLValueArray(f, d, t, s, mode, flags)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorLValueArray(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		if d.Type().Kind() == cc.Union {
			panic(todo("", pos(d)))
		}

		panic(todo(""))
		p.w("%sX%s", imp.qualifier, nm)
	}
}

func (p *project) declaratorLValueNormal(f *function, d *cc.Declarator, t, s cc.Type, mode exprMode, flags flags) {
	if d.Type().IsScalarType() {
		defer p.w("%s", p.convertType(d.Type(), t, flags))
	}
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("*(*%s)(unsafe.Pointer(%s%s/* %s */))", p.typ(d.Type()), f.bpName, nonZeroUintptr(local.off), local.name)
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

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

func (p *project) declaratorPSelect(f *function, d *cc.Declarator, s cc.Type) {
	switch k := p.declaratorKind(d); k {
	case opNormal:
		p.declaratorPSelectNormal(f, d, s)
	case opArray:
		panic(todo(""))
		p.declaratorPSelectArray(f, d, s)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorPSelectArray(f *function, d *cc.Declarator, s cc.Type) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(d.Type().Elem()), f.bpName, nonZeroUintptr(local.off), local.name)
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

func (p *project) declaratorPSelectNormal(f *function, d *cc.Declarator, s cc.Type) {
	if f != nil {
		if local := f.locals[d]; local != nil {
			if local.isPinned {
				if s == nil {
					s = d.Type()
				}
				p.w("(*%s)(unsafe.Pointer(%s%s/* &%s */))", p.typ(s), f.bpName, nonZeroUintptr(local.off), local.name)
				return
			}

			p.w("(*%s)(unsafe.Pointer(%s))", p.typ(d.Type().Elem()), local.name)
			return
		}
	}

	tld := p.tlds[d]
	if tld == nil {
		tld = p.externs[d.Name()]
	}
	switch {
	case tld != nil:
		p.w("(*%s)(unsafe.Pointer(%s))", p.typ(d.Type().Elem()), tld.name)
	default:
		panic(todo(""))
	}
}

func (p *project) declaratorSelect(f *function, d *cc.Declarator, s cc.Type) {
	switch k := p.declaratorKind(d); k {
	case opNormal:
		p.declaratorSelectNormal(f, d, s)
	case opArray:
		p.declaratorSelectArray(f, d, s)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorSelectArray(f *function, d *cc.Declarator, s cc.Type) {
	if local := f.locals[d]; local != nil {
		if local.isPinned {
			panic(todo(""))
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

func (p *project) declaratorSelectNormal(f *function, d *cc.Declarator, s cc.Type) {
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

func (p *project) declaratorAddrOf(f *function, d *cc.Declarator, t cc.Type) {
	switch k := p.declaratorKind(d); k {
	case opArray:
		p.declaratorAddrOfArray(f, d)
	case opNormal:
		p.declaratorAddrOfNormal(f, d)
	case opUnion:
		p.declaratorAddrOfUnion(f, d)
	case opFunction:
		p.declaratorAddrOfFunction(f, d)
	case opArrayParameter:
		p.declaratorAddrOfArrayParameter(f, d)
	default:
		panic(todo("", d.Position(), k))
	}
}

func (p *project) declaratorAddrOfArrayParameter(f *function, d *cc.Declarator) {
	p.w("%s", f.locals[d].name)
}

func (p *project) declaratorAddrOfFunction(f *function, d *cc.Declarator) {
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		p.w("*(*uintptr)(unsafe.Pointer(&struct{f ")
		p.functionSignature(f, d.Type(), "")
		p.w("}{%sX%s}))", imp.qualifier, nm)
	}
}

func (p *project) declaratorAddrOfUnion(f *function, d *cc.Declarator) {
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		panic(todo("", pos(d), d.Name()))
	}
}

func (p *project) declaratorAddrOfNormal(f *function, d *cc.Declarator) {
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		panic(todo("", pos(d), d.Name()))
	}
}

func (p *project) declaratorAddrOfArray(f *function, d *cc.Declarator) {
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
			p.err(d, "%v: undefined: %s", d.Position(), d.Name())
			return
		}

		panic(todo("", pos(d), d.Name()))
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

		panic(todo("", from, to))
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
	case cc.Ptr:
		if !force && from.Kind() == to.Kind() {
			return ""
		}

		if to.IsIntegerType() {
			p.w("%s(", p.typ(to))
			return ")"
		}

		panic(todo("%q -> %q", from, to))
	case cc.Function, cc.Struct, cc.Union:
		if !force && from.Kind() == to.Kind() {
			return ""
		}

		panic(todo("%q -> %q", from, to))
	case cc.Double, cc.Float:
		p.w("%s(", p.typ(to))
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

func (p *project) convertInt(op cc.Operand, to cc.Type, flags flags) string {
	force := flags&fForceConv != 0
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
			case cc.Uint64Value:
				switch to.Size() {
				case 1:
					if x <= math.MaxUint8 {
						p.w("%s(", p.typ(to))
						return ")"
					}

					p.w("%sUint8FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 2:
					if x <= math.MaxUint16 {
						p.w("%s(", p.typ(to))
						return ")"
					}

					p.w("%sUint16FromUint%d(", p.task.crt, from.Size()*8)
					return ")"
				case 4:
					if x <= math.MaxUint32 {
						p.w("%s(", p.typ(to))
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

func (p *project) tld(f *function, n *cc.InitDeclarator, sep string) {
	d := n.Declarator
	tld := p.tlds[d]
	if tld == nil { // Dead declaration.
		return
	}

	t := d.Type()
	if d.IsTypedefName {
		if t.Kind() != cc.Void {
			p.w("%stype %s = %s; /* %v */", sep, tld.name, p.typ(t), pos(n))
		}
		return
	}

	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator AttributeSpecifierList
		p.w("%svar %s %s\t/* %v: */", sep, tld.name, p.typ(t), pos(n))
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
		scope := f.blocks[n.CompoundStatement].scope
		f.bpName = scope.take("bp")
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
	p.w(";")
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
		p.zeroValue(f.rt)
	}
	if brace {
		p.w("}")
	}
}

func (p *project) zeroValue(t cc.Type) {
	if t.IsScalarType() {
		p.w("%s(0)", p.typ(t))
		return
	}

	switch t.Kind() {
	case cc.Struct, cc.Union:
		p.w("%s{}", p.typ(t))
	default:
		panic(todo("", t, t.Kind()))
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
	p.w("%s", comment("\n", n))
	if _, ok := n.Context().(*cc.SelectionStatement); ok && f.ifCtx == nil {
		switch f.switchCtx {
		case inSwitchCase:
			f.switchCtx = inSwitchSeenBreak
		case inSwitchSeenBreak:
			// nop but TODO
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
		case n.Expression != nil && f.rt != nil && f.rt.Kind() == cc.Void:
			p.expression(f, n.Expression, f.rt, nil, exprVoid, fOutermost)
		default:
			p.w("return ")
			if n.Expression != nil {
				p.expression(f, n.Expression, f.rt, nil, exprValue, fOutermost)
				break
			}

			if f.rt != nil && f.rt.Kind() != cc.Void {
				p.zeroValue(f.rt)
			}
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
	return r
}

func (p *project) expression(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoid:
		p.expressionVoid(f, n, t, s, mode, flags)
	case exprValue:
		p.expressionValue(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.expressionVoidSingle(f, n, t, s, mode, flags)
	case exprBool:
		p.expressionBool(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.expressionAddrOf(f, n, t, s, mode, flags)
	case exprPSelect:
		p.expressionPSelect(f, n, t, s, mode, flags)
	case exprLValue:
		p.expressionLValue(f, n, t, s, mode, flags)
	case exprFunc:
		p.expressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) expressionFunc(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionLValue(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionPSelect(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionAddrOf(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionBool(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionVoidSingle(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		p.w(" func() {")
		p.expression(f, n, t, s, exprVoid, flags)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionValue(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) expressionVoid(f *function, n *cc.Expression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExpressionAssign: // AssignmentExpression
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
	case cc.ExpressionComma: // Expression ',' AssignmentExpression
		p.expression(f, n.Expression, t, s, mode, flags)
		p.w(";")
		p.assignmentExpression(f, n.AssignmentExpression, t, s, mode, flags)
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
	case t.IsBitFieldType():
		return opBitfield
	case t.Kind() == cc.Function:
		return opFunction
	default:
		return opNormal
	}
}

func (p *project) assignmentExpression(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoid:
		p.assignmentExpressionVoid(f, n, t, s, mode, flags)
	case exprValue:
		p.assignmentExpressionValue(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.assignmentExpressionAddrOf(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.assignmentExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprBool:
		p.assignmentExpressionBool(f, n, t, s, mode, flags)
	case exprLValue:
		p.assignmentExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.assignmentExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.assignmentExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) assignmentExpressionFunc(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
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

func (p *project) assignmentExpressionPSelect(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
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

func (p *project) assignmentExpressionLValue(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
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

func (p *project) assignmentExpressionBool(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.assignmentExpression(f, n, t, s, exprValue, flags|fOutermost)
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

func (p *project) assignmentExpressionVoidSingle(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), nil, exprLValue, flags)
		p.w(" = ")
		mode = exprValue
		if p.isArrayOrPinnedArray(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type()) {
			mode = exprAddrOf
		}
		p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), nil, mode, flags|fOutermost)
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
		p.assignShiftOp(f, n, t, nil, mode, "<<", "Shl", flags)
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		p.assignShiftOp(f, n, t, nil, mode, ">>", "Shr", flags)
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

func (p *project) assignmentExpressionAddrOf(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
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

func (p *project) assignmentExpressionValue(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		p.assignmentExpressionValueAssign(f, n, t, s, mode, flags)
	case cc.AssignmentExpressionMul: // UnaryExpression "*=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "*", "Mul", flags)
	case cc.AssignmentExpressionDiv: // UnaryExpression "/=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "/", "Div", flags)
	case cc.AssignmentExpressionMod: // UnaryExpression "%=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "%", "Rem", flags)
	case cc.AssignmentExpressionAdd: // UnaryExpression "+=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "+", "Add", flags)
	case cc.AssignmentExpressionSub: // UnaryExpression "-=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "-", "Sub", flags)
	case cc.AssignmentExpressionLsh: // UnaryExpremode, ssion "<<=
		panic(todo(""))
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		panic(todo(""))
	case cc.AssignmentExpressionAnd: // UnaryExpression "&=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "&", "And", flags)
	case cc.AssignmentExpressionXor: // UnaryExpression "^=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "^", "Xor", flags)
	case cc.AssignmentExpressionOr: // UnaryExpression "|=" AssignmentExpression
		p.assignOp(f, n, t, s, mode, "|", "Or", flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) assignmentExpressionValueAssign(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	// UnaryExpression '=' AssignmentExpression
	lhs := n.UnaryExpression
	switch k := p.opKind(f, lhs, lhs.Operand.Type()); k {
	case opNormal:
		p.assignmentExpressionValueAssignNormal(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignmentExpressionValueAssignNormal(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	if d := n.UnaryExpression.Declarator(); d != nil {
		if !d.Type().IsScalarType() {
			panic(todo(""))
		}

		if local := f.locals[d]; local != nil {
			if local.isPinned {
				defer p.w(")%s", p.convertType(d.Type(), t, flags))
				p.w("%sAssignPtr%s(", p.task.crt, p.helperType(d.Type()))
				p.w("%s%s /* %s */", f.bpName, nonZeroUintptr(local.off), local.name)
				p.w(", ")
				p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), nil, exprValue, flags|fOutermost)
				return
			}

			defer p.w(")%s", p.convertType(d.Type(), t, flags))
			p.w("%sAssign%s(&%s, ", p.task.crt, p.helperType(d.Type()), local.name)
			p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), nil, exprValue, flags|fOutermost)
			return
		}

		panic(todo("", pos(n)))
	}

	defer p.w(")%s", p.convertType(n.UnaryExpression.Operand.Type(), t, flags))
	p.w("%sAssignPtr%s(", p.task.crt, p.helperType(n.UnaryExpression.Operand.Type()))
	p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), s, exprAddrOf, flags)
	p.w(", ")
	p.assignmentExpression(f, n.AssignmentExpression, n.UnaryExpression.Operand.Type(), nil, exprValue, flags|fOutermost)
}

func (p *project) assignmentExpressionVoid(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AssignmentExpressionCond: // ConditionalExpression
		p.conditionalExpression(f, n.ConditionalExpression, t, s, mode, flags)
	case cc.AssignmentExpressionAssign: // UnaryExpression '=' AssignmentExpression
		lhs := n.UnaryExpression
		lt := lhs.Operand.Type()
		switch k := p.opKind(f, lhs, lt); k {
		case opNormal:
			p.unaryExpression(f, lhs, lt, nil, exprLValue, flags)
			p.w(" = ")
			mode = exprValue
			if p.isArrayOrPinnedArray(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type()) {
				mode = exprAddrOf
			}
			p.assignmentExpression(f, n.AssignmentExpression, lt, nil, mode, flags|fOutermost)
		case opBitfield:
			bf := lt.BitField()
			p.w("%sSetBitFieldPtr%d%s(", p.task.crt, bf.BitFieldBlockWidth(), p.bfHelperType(lt))
			p.unaryExpression(f, lhs, lt, nil, exprAddrOf, flags)
			p.w(", ")
			p.assignmentExpression(f, n.AssignmentExpression, lt, nil, exprValue, flags|fOutermost)
			p.w(", %d, %#x)", bf.BitFieldOffset(), bf.Mask())
		case opUnion:
			p.unaryExpression(f, lhs, lt, nil, exprLValue, flags)
			p.w(" = ")
			p.assignmentExpression(f, n.AssignmentExpression, lt, nil, exprValue, flags|fOutermost)
		default:
			panic(todo("", n.Position(), k))
		}
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
		p.assignShiftOp(f, n, t, nil, mode, "<<", "Shl", flags)
	case cc.AssignmentExpressionRsh: // UnaryExpression ">>=" AssignmentExpression
		p.assignShiftOp(f, n, t, nil, mode, ">>", "Shr", flags)
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
	switch mode {
	case exprValue:
		p.conditionalExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.conditionalExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.conditionalExpressionAddrOf(f, n, t, s, mode, flags)
	case exprBool:
		p.conditionalExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.conditionalExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.conditionalExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.conditionalExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.conditionalExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) conditionalExpressionFunc(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionPSelect(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionLValue(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionVoidSingle(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionBool(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.conditionalExpression(f, n, t, s, exprValue, flags|fOutermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionAddrOf(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		p.w(" func() %s { if ", p.typ(t))
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags|fOutermost)
		p.w(" { return ")
		p.expression(f, n.Expression, t, nil, exprValue, flags|fOutermost)
		p.w("}; return ")
		p.conditionalExpression(f, n.ConditionalExpression, t, nil, exprValue, flags|fOutermost)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionVoid(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		p.w("if ")
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags|fOutermost)
		p.w(" {")
		p.expression(f, n.Expression, t, nil, mode, flags|fOutermost)
		p.w("} else {")
		p.conditionalExpression(f, n.ConditionalExpression, t, nil, mode, flags|fOutermost)
		p.w("}")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) conditionalExpressionValue(f *function, n *cc.ConditionalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ConditionalExpressionLOr: // LogicalOrExpression
		p.logicalOrExpression(f, n.LogicalOrExpression, t, s, mode, flags)
	case cc.ConditionalExpressionCond: // LogicalOrExpression '?' Expression ':' ConditionalExpression
		t = t.Decay()
		p.w(" func() %s { if ", p.typ(t))
		p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags|fOutermost)
		p.w(" { return ")
		p.expression(f, n.Expression, t, nil, exprValue, flags|fOutermost)
		p.w("}; return ")
		p.conditionalExpression(f, n.ConditionalExpression, t, nil, exprValue, flags|fOutermost)
		p.w("}()")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpression(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.logicalOrExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.logicalOrExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.logicalOrExpressionAddrOf(f, n, t, s, mode, flags)
	case exprBool:
		p.logicalOrExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.logicalOrExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.logicalOrExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.logicalOrExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.logicalOrExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) logicalOrExpressionFunc(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionPSelect(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionLValue(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionVoidSingle(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionBool(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		p.binaryLogicalOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionAddrOf(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionVoid(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalOrExpressionLAnd: // LogicalAndExpression
		p.logicalAndExpression(f, n.LogicalAndExpression, t, s, mode, flags)
	case cc.LogicalOrExpressionLOr: // LogicalOrExpression "||" LogicalAndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalOrExpressionValue(f *function, n *cc.LogicalOrExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, n.Operand.Type(), &mode, flags))
	p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags)
	p.w(" ||%s", comment(" ", &n.Token))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), nil, exprBool, flags)
}

func (p *project) binaryLogicalOrExpressionValue(f *function, n *cc.LogicalOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.logicalOrExpression(f, n.LogicalOrExpression, n.LogicalOrExpression.Operand.Type(), nil, exprBool, flags)
	p.w(" ||%s", comment(" ", &n.Token))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), nil, exprBool, flags)
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
	switch mode {
	case exprValue:
		p.logicalAndExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.logicalAndExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.logicalAndExpressionAddrOf(f, n, t, s, mode, flags)
	case exprBool:
		p.logicalAndExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.logicalAndExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.logicalAndExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.logicalAndExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.logicalAndExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) logicalAndExpressionFunc(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionPSelect(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionLValue(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionVoidSingle(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionBool(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		p.binaryLogicalAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionAddrOf(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionVoid(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.LogicalAndExpressionOr: // InclusiveOrExpression
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, t, s, mode, flags)
	case cc.LogicalAndExpressionLAnd: // LogicalAndExpression "&&" InclusiveOrExpression
		p.binaryLogicalAndExpressionValue(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) logicalAndExpressionValue(f *function, n *cc.LogicalAndExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), nil, exprBool, flags)
	p.w(" &&%s", comment(" ", &n.Token))
	p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.InclusiveOrExpression.Operand.Type(), nil, exprBool, flags)
}

func (p *project) binaryLogicalAndExpressionBool(f *function, n *cc.LogicalAndExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.logicalAndExpression(f, n.LogicalAndExpression, n.LogicalAndExpression.Operand.Type(), nil, exprBool, flags)
	p.w(" &&%s", comment(" ", &n.Token))
	p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.InclusiveOrExpression.Operand.Type(), nil, exprBool, flags)
}

func (p *project) inclusiveOrExpression(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.inclusiveOrExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.inclusiveOrExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.inclusiveOrExpressionAddrof(f, n, t, s, mode, flags)
	case exprBool:
		p.inclusiveOrExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.inclusiveOrExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.inclusiveOrExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.inclusiveOrExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.inclusiveOrExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) inclusiveOrExpressionFunc(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionPSelect(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionLValue(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionVoidSingle(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionBool(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		p.binaryInclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionAddrof(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionVoid(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.InclusiveOrExpressionXor: // ExclusiveOrExpression
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, t, s, mode, flags)
	case cc.InclusiveOrExpressionOr: // InclusiveOrExpression '|' ExclusiveOrExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) inclusiveOrExpressionValue(f *function, n *cc.InclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case orOverflows(n.InclusiveOrExpression.Operand, n.ExclusiveOrExpression.Operand, n.Promote()):
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), nil, exprValue, flags)
		p.w(" |%s", comment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
	default:
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), nil, exprValue, flags)
		p.w(" |%s", comment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags)
	}
}

func (p *project) binaryInclusiveOrExpressionBool(f *function, n *cc.InclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case orOverflows(n.InclusiveOrExpression.Operand, n.ExclusiveOrExpression.Operand, n.Promote()):
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), nil, exprValue, flags)
		p.w(" |%s", comment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
	default:
		p.inclusiveOrExpression(f, n.InclusiveOrExpression, n.Promote(), nil, exprValue, flags)
		p.w(" |%s", comment(" ", &n.Token))
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags)
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
	switch mode {
	case exprValue:
		p.exclusiveOrExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.exclusiveOrExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.exclusiveOrExpressionAddrOf(f, n, t, s, mode, flags)
	case exprBool:
		p.exclusiveOrExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.exclusiveOrExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.exclusiveOrExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.exclusiveOrExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.exclusiveOrExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) exclusiveOrExpressionFunc(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionPSelect(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionLValue(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionVoidSingle(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionBool(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		p.binaryExclusiveOrExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionAddrOf(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionVoid(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ExclusiveOrExpressionAnd: // AndExpression
		p.andExpression(f, n.AndExpression, t, s, mode, flags)
	case cc.ExclusiveOrExpressionXor: // ExclusiveOrExpression '^' AndExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) exclusiveOrExpressionValue(f *function, n *cc.ExclusiveOrExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	switch mode {
	case exprValue, exprBool:
		p.binaryExclusiveOrExpressionValue(f, n, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryExclusiveOrExpressionValue(f *function, n *cc.ExclusiveOrExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case xorOverflows(n.ExclusiveOrExpression.Operand, n.AndExpression.Operand, n.Promote()):
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags)
		p.w(" ^%s", comment(" ", &n.Token))
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
	default:
		p.exclusiveOrExpression(f, n.ExclusiveOrExpression, n.Promote(), nil, exprValue, flags)
		p.w(" ^%s", comment(" ", &n.Token))
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags)
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
	switch mode {
	case exprValue:
		p.andExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.andExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.andExpressionAddrof(f, n, t, s, mode, flags)
	case exprBool:
		p.andExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.andExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.andExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.andExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.andExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) andExpressionFunc(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionPSelect(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionLValue(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionVoidSingle(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionBool(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		p.binaryAndExpression(f, n, t, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionAddrof(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionVoid(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AndExpressionEq: // EqualityExpression
		p.equalityExpression(f, n.EqualityExpression, t, s, mode, flags)
	case cc.AndExpressionAnd: // AndExpression '&' EqualityExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) andExpressionValue(f *function, n *cc.AndExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, n.Operand.Type(), &mode, flags))
	switch {
	case andOverflows(n.AndExpression.Operand, n.EqualityExpression.Operand, n.Promote()):
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags)
		p.w(" &%s", comment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
	default:
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags)
		p.w(" &%s", comment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags)
	}
}

func (p *project) binaryAndExpressionValue(f *function, n *cc.AndExpression, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case andOverflows(n.AndExpression.Operand, n.EqualityExpression.Operand, n.Promote()):
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags)
		p.w(" &%s", comment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
	default:
		p.andExpression(f, n.AndExpression, n.Promote(), nil, exprValue, flags)
		p.w(" &%s", comment(" ", &n.Token))
		p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags)
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
	switch mode {
	case exprValue:
		p.equalityExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.equalityExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.equalityExpressionAddrOf(f, n, t, s, mode, flags)
	case exprBool:
		p.equalityExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.equalityExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.equalityExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.equalityExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.equalityExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) equalityExpressionFunc(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, s, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionPSelect(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, s, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionLValue(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, s, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionVoidSingle(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, s, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionBool(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
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

func (p *project) equalityExpressionAddrOf(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, s, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionVoid(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.EqualityExpressionRel: // RelationalExpression
		p.relationalExpression(f, n.RelationalExpression, t, s, mode, flags)
	case cc.EqualityExpressionEq: // EqualityExpression "==" RelationalExpression
		panic(todo(""))
	case cc.EqualityExpressionNeq: // EqualityExpression "!=" RelationalExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) equalityExpressionValue(f *function, n *cc.EqualityExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags)
	p.w(" %s%s", oper, comment(" ", &n.Token))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), nil, exprValue, flags)
}

func (p *project) binaryEqualityExpressionValue(f *function, n *cc.EqualityExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.equalityExpression(f, n.EqualityExpression, n.Promote(), nil, exprValue, flags)
	p.w(" %s%s", oper, comment(" ", &n.Token))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), nil, exprValue, flags)
}

func (p *project) relationalExpression(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.relationalExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.relationalExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.relationalExpressionAddrOf(f, n, t, s, mode, flags)
	case exprBool:
		p.relationalExpressionBool(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.relationalExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprLValue:
		p.relationalExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.relationalExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.relationalExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) relationalExpressionFunc(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, s, mode, flags)
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

func (p *project) relationalExpressionPSelect(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, s, mode, flags)
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

func (p *project) relationalExpressionLValue(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, s, mode, flags)
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

func (p *project) relationalExpressionVoidSingle(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, s, mode, flags)
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

func (p *project) relationalExpressionBool(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
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

func (p *project) relationalExpressionAddrOf(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, s, mode, flags)
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

func (p *project) relationalExpressionVoid(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.RelationalExpressionShift: // ShiftExpression
		p.shiftExpression(f, n.ShiftExpression, t, s, mode, flags)
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

func (p *project) relationalExpressionValue(f *function, n *cc.RelationalExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	flags &^= fOutermost
	defer p.w("%s", p.booleanBinaryExpression(n.Operand, t, &mode, flags))
	p.relationalExpression(f, n.RelationalExpression, n.Promote(), nil, exprValue, flags)
	p.w(" %s%s", oper, comment(" ", &n.Token))
	p.shiftExpression(f, n.ShiftExpression, n.Promote(), nil, exprValue, flags)
}

func (p *project) shiftExpression(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.shiftExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.shiftExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.shiftExpressionAddrOf(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.shiftExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprBool:
		p.shiftExpressionBool(f, n, t, s, mode, flags)
	case exprLValue:
		p.shiftExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.shiftExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.shiftExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) shiftExpressionFunc(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, s, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionPSelect(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, s, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionLValue(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, s, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionBool(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
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

func (p *project) shiftExpressionVoidSingle(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, s, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionAddrOf(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, s, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionVoid(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.ShiftExpressionAdd: // AdditiveExpression
		p.additiveExpression(f, n.AdditiveExpression, t, s, mode, flags)
	case cc.ShiftExpressionLsh: // ShiftExpression "<<" AdditiveExpression
		panic(todo(""))
	case cc.ShiftExpressionRsh: // ShiftExpression ">>" AdditiveExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) shiftExpressionValue(f *function, n *cc.ShiftExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, n.Operand.Type(), &mode, flags))
	switch {
	case n.ShiftExpression.Operand.Type().IsBitFieldType():
		p.w("(")
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
		p.w(")&%#x", n.ShiftExpression.Operand.Type().BitField().Mask())
	case shiftOverflows(n.ShiftExpression.Operand, n.AdditiveExpression.Operand, oper, n.Operand.Type()):
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags|fForceRuntimeConv)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
	case isConstInteger(n.ShiftExpression.Operand):
		s := p.convert(nil, n.Operand.Type(), 0)
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags)
		p.w("%s %s%s", s, oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
	default:
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
	}
}

func (p *project) binaryShiftExpressionValue(f *function, n *cc.ShiftExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case n.ShiftExpression.Operand.Type().IsBitFieldType():
		p.w("(")
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
		p.w(")&%#x", n.ShiftExpression.Operand.Type().BitField().Mask())
	case shiftOverflows(n.ShiftExpression.Operand, n.AdditiveExpression.Operand, oper, n.Operand.Type()):
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags|fForceRuntimeConv)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
	case isConstInteger(n.ShiftExpression.Operand):
		s := p.convert(nil, n.Operand.Type(), 0)
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags)
		p.w("%s %s%s", s, oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
	default:
		p.shiftExpression(f, n.ShiftExpression, n.Operand.Type(), nil, exprValue, flags)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
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
	switch mode {
	case exprValue:
		p.additiveExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.additiveExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.additiveExpressionAddrOf(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.additiveExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprBool:
		p.additiveExpressionBool(f, n, t, s, mode, flags)
	case exprLValue:
		p.additiveExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.additiveExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.additiveExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) additiveExpressionFunc(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, s, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionPSelect(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, s, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionLValue(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, s, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionBool(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
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

func (p *project) additiveExpressionVoidSingle(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, s, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionAddrOf(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, s, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionVoid(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.AdditiveExpressionMul: // MultiplicativeExpression
		p.multiplicativeExpression(f, n.MultiplicativeExpression, t, s, mode, flags)
	case cc.AdditiveExpressionAdd: // AdditiveExpression '+' MultiplicativeExpression
		panic(todo(""))
	case cc.AdditiveExpressionSub: // AdditiveExpression '-' MultiplicativeExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) additiveExpressionValue(f *function, n *cc.AdditiveExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, n.Operand.Type(), &mode, flags))
	lo := n.AdditiveExpression.Operand
	ro := n.MultiplicativeExpression.Operand
	lt := lo.Type()
	rt := ro.Type()
	switch {
	case lt.IsArithmeticType() && rt.IsArithmeticType(): // x +- y
		defer p.w("%s", p.bitFieldPatch2(lo, ro, n.Promote()))
		switch {
		case intAddOverflows(lo, ro, oper, n.Promote()): // i +- j
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
			p.w(" %s%s", oper, comment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
		default:
			var s string
			if isRealType(n.Operand) && n.Operand.Value() != nil {
				s = p.convert(nil, n.Promote(), flags)
			}
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
			p.w("%s %s%s", s, oper, comment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags)
		}
	default:
		panic(todo("", n.Position(), lt, rt, oper))
	}
}

func (p *project) binaryAdditiveExpressionValue(f *function, n *cc.AdditiveExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	lo := n.AdditiveExpression.Operand
	ro := n.MultiplicativeExpression.Operand
	lt := lo.Type()
	rt := ro.Type()
	switch {
	case lt.IsArithmeticType() && rt.IsArithmeticType(): // x +- y
		defer p.w("%s", p.bitFieldPatch2(lo, ro, n.Promote()))
		switch {
		case intAddOverflows(lo, ro, oper, n.Promote()): // i +- j
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
			p.w(" %s%s", oper, comment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
		default:
			var s string
			if isRealType(n.Operand) && n.Operand.Value() != nil {
				s = p.convert(nil, n.Promote(), flags)
			}
			p.additiveExpression(f, n.AdditiveExpression, n.Promote(), nil, exprValue, flags)
			p.w("%s %s%s", s, oper, comment(" ", &n.Token))
			p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags)
		}
	case lt.Kind() == cc.Ptr && rt.IsIntegerType(): // p +- i
		p.additiveExpression(f, n.AdditiveExpression, lt, nil, exprValue, flags)
		p.w(" %s%s uintptr(", oper, comment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, nil, exprValue, flags)
		p.w(")")
		if sz := lt.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	case lt.Kind() == cc.Array && rt.IsIntegerType(): // p +- i
		p.additiveExpression(f, n.AdditiveExpression, lt, nil, exprAddrOf, flags)
		p.w(" %s%s uintptr(", oper, comment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, rt, nil, exprValue, flags)
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
		p.additiveExpression(f, n.AdditiveExpression, p.ast.PtrdiffType, nil, exprValue, flags)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.multiplicativeExpression(f, n.MultiplicativeExpression, p.ast.PtrdiffType, nil, exprValue, flags)
		p.w(")/%d", lt.Elem().Size())
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

func (p *project) multiplicativeExpression(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprValue:
		p.multiplicativeExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.multiplicativeExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.multiplicativeExpressionAddrOf(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.multiplicativeExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprBool:
		p.multiplicativeExpressionBool(f, n, t, s, mode, flags)
	case exprLValue:
		p.multiplicativeExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.multiplicativeExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.multiplicativeExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) multiplicativeExpressionFunc(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
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

func (p *project) multiplicativeExpressionPSelect(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
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

func (p *project) multiplicativeExpressionLValue(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
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

func (p *project) multiplicativeExpressionBool(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
	case
		cc.MultiplicativeExpressionMul, // MultiplicativeExpression '*' CastExpression
		cc.MultiplicativeExpressionDiv, // MultiplicativeExpression '/' CastExpression
		cc.MultiplicativeExpressionMod: // MultiplicativeExpression '%' CastExpression

		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.multiplicativeExpression(f, n, t, s, exprValue, flags|fOutermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) multiplicativeExpressionVoidSingle(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
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

func (p *project) multiplicativeExpressionAddrOf(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
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

func (p *project) multiplicativeExpressionVoid(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.MultiplicativeExpressionCast: // CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
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

func (p *project) multiplicativeExpressionValue(f *function, n *cc.MultiplicativeExpression, t, s cc.Type, mode exprMode, flags flags) {
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
	switch mode {
	case exprValue:
		p.binaryMultiplicativeExpressionValue(f, n, oper, t, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) binaryMultiplicativeExpressionValue(f *function, n *cc.MultiplicativeExpression, oper string, t cc.Type, mode exprMode, flags flags) {
	flags &^= fOutermost
	defer p.w("%s", p.artithmeticBinaryExpression(n.Operand, t, &mode, flags))
	switch {
	case intMulOverflows(n.MultiplicativeExpression.Operand, n.CastExpression.Operand, oper, n.Promote()):
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags)
		p.w(" %s%s", oper, comment(" ", &n.Token))
		p.castExpression(f, n.CastExpression, n.Promote(), nil, exprValue, flags|fForceRuntimeConv)
	default:
		defer p.w("%s", p.bitFieldPatch2(n.MultiplicativeExpression.Operand, n.CastExpression.Operand, n.Promote()))
		var s string
		if isRealType(n.Operand) && n.Operand.Value() != nil {
			s = p.convert(nil, n.Promote(), flags)
		}
		p.multiplicativeExpression(f, n.MultiplicativeExpression, n.Promote(), nil, exprValue, flags)
		p.w("%s %s%s", s, oper, comment(" ", &n.Token))
		if (oper == "/" || oper == "%") && (isZeroReal(n.MultiplicativeExpression.Operand) || isZeroReal(n.CastExpression.Operand)) {
			p.w("%s%sFrom%[2]s(", p.task.crt, p.helperType(n.Promote()))
			defer p.w(")")
		}
		p.castExpression(f, n.CastExpression, n.Promote(), nil, exprValue, flags)
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
	if n.Case == cc.CastExpressionCast {
		if f != nil && n.CastExpression.Operand.Type().Kind() == cc.Ptr { // void *__ccgo_va_arg(__builtin_va_list ap);
			sv := f.vaType
			f.vaType = n.TypeName.Type()
			defer func() { f.vaType = sv }()
		}
	}
	switch mode {
	case exprValue:
		p.castExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.castExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.castExpressionAddrOf(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.castExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprBool:
		p.castExpressionBool(f, n, t, s, mode, flags)
	case exprLValue:
		p.castExpressionLValue(f, n, t, s, mode, flags)
	case exprPSelect:
		p.castExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.castExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) castExpressionFunc(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionPSelect(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		p.castExpression(f, n.CastExpression, n.TypeName.Type(), s, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionLValue(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionBool(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0 ")
		p.castExpression(f, n, n.Operand.Type(), s, exprValue, flags|fOutermost)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionVoidSingle(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionAddrOf(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionVoid(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionValue(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.CastExpressionUnary: // UnaryExpression
		p.unaryExpression(f, n.UnaryExpression, t, s, mode, flags)
	case cc.CastExpressionCast: // '(' TypeName ')' CastExpression
		switch k := p.opKind(f, n.CastExpression, n.CastExpression.Operand.Type()); k {
		case opNormal, opBitfield:
			p.castExpressionValueNormal(f, n, t, s, mode, flags)
		case opArray:
			p.castExpressionValueArray(f, n, t, s, mode, flags)
		case opFunction:
			p.castExpressionValueFunction(f, n, t, s, mode, flags)
		default:
			panic(todo("", n.Position(), k))
		}
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) castExpressionValueFunction(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	op := n.CastExpression.Operand
	tn := n.TypeName.Type()
	switch {
	case op.Type().Kind() == cc.Function:
		switch {
		case tn.Kind() == cc.Ptr && t.Kind() == cc.Ptr:
			p.w("(*(*")
			p.functionSignature(f, op.Type(), "")
			p.w(")(unsafe.Pointer(")
			p.castExpression(f, n.CastExpression, tn, s, exprValue, flags)
			p.w(")))")
		default:
			panic(todo(""))
		}
	default:
		panic(todo("%v: %v -> %v -> %v", pos(n), op.Type(), tn, t))
	}
}

func (p *project) castExpressionValueArray(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	tn := n.TypeName.Type()
	switch {
	case tn.IsScalarType():
		defer p.w("%s", p.convertType(nil, t, flags))
		p.castExpression(f, n.CastExpression, tn, s, exprAddrOf, flags)
	default:
		panic(todo("", pos(n)))
	}
}

func (p *project) castExpressionValueNormal(f *function, n *cc.CastExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '(' TypeName ')' CastExpression
	op := n.CastExpression.Operand
	tn := n.TypeName.Type()
	switch {
	case op.Type().Kind() == cc.Ptr && tn.IsArithmeticType():
		defer p.w("%s", p.convertType(nil, t, flags|fForceConv))
		p.castExpression(f, n.CastExpression, op.Type(), s, mode, flags)
	case tn.IsArithmeticType():
		switch {
		case (tn.Kind() == cc.Float || tn.Kind() == cc.Double) && op.Type().IsIntegerType() && op.Value() != nil && t.IsIntegerType():
			panic(todo(""))
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
			defer p.w("%s", p.convertType(tn, t, flags))
			p.castExpression(f, n.CastExpression, tn, s, mode, flags)
		case cc.Void:
			p.castExpression(f, n.CastExpression, tn, s, exprVoid, flags)
		default:
			panic(todo("", n.Position(), t, t.Kind()))
		}
	}
}

func (p *project) unaryExpression(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprLValue:
		p.unaryExpressionLValue(f, n, t, s, mode, flags)
	case exprValue:
		p.unaryExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.unaryExpressionVoid(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.unaryExpressionAddrOf(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.unaryExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprBool:
		p.unaryExpressionBool(f, n, t, s, mode, flags)
	case exprPSelect:
		p.unaryExpressionPSelect(f, n, t, s, mode, flags)
	case exprFunc:
		p.unaryExpressionFunc(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) unaryExpressionFunc(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo(""))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		switch ce := n.CastExpression.Operand.Type(); ce.Kind() {
		case cc.Ptr:
			p.castExpression(f, n.CastExpression, ce.Elem(), s, mode, flags)
		default:
			panic(todo("", n.Position(), n.CastExpression.Operand.Type()))
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

func (p *project) unaryExpressionPSelect(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		p.unaryExpression(f, n, t, s, exprValue, flags)
	case cc.UnaryExpressionDeref: // '*' CastExpression
		panic(todo(""))
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

func (p *project) unaryExpressionBool(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	if flags&fOutermost == 0 {
		p.w("(")
		defer p.w(")")
	}
	defer p.w(" != 0 ")
	p.unaryExpression(f, n, t, s, exprValue, flags|fOutermost)
}

func (p *project) unaryExpressionVoidSingle(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		panic(todo("", n.Position()))
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

func (p *project) unaryExpressionAddrOf(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		p.unaryExpressionDeref(f, n, t, s, mode, flags)
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

func (p *project) unaryExpressionVoid(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprVoid, flags)
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

func (p *project) unaryExpressionValue(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		p.unaryExpressionPreIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		if t.Kind() != cc.Ptr {
			defer p.w("%s", p.convert(n.Operand, t, flags))
		}
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), nil, exprAddrOf, flags&^fOutermost)
	case cc.UnaryExpressionDeref: // '*' CastExpression
		p.unaryExpressionDeref(f, n, t, s, mode, flags)
	case cc.UnaryExpressionPlus: // '+' CastExpression
		p.w(" +")
		p.castExpression(f, n.CastExpression, t, s, mode, flags)
	case cc.UnaryExpressionMinus: // '-' CastExpression
		switch {
		case isZeroReal(n.CastExpression.Operand):
			p.w(" -")
			defer p.w("%s", p.convert(n.CastExpression.Operand, t, flags))
			p.w("%s%sFrom%[2]s(", p.task.crt, p.helperType(n.CastExpression.Operand.Type()))
			p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags)
			p.w(")")
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
		case n.CastExpression.Operand.Value() != nil:
			switch {
			case !t.IsIntegerType():
				defer p.w("%s", p.convert(n.Operand, t, flags))
				p.w(" ^")
				p.castExpression(f, n.CastExpression, n.Operand.Type(), s, exprValue, flags|fForceRuntimeConv)
			default:
				p.w("^")
				defer p.w("%s", p.convert(n.CastExpression.Operand, t, flags|fForceConv))
				p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags|fOutermost)
			}
		default:
			defer p.w("%s", p.convert(n.Operand, t, flags))
			p.w(" ^")
			p.castExpression(f, n.CastExpression, n.Operand.Type(), s, exprValue, flags)
		}
	case cc.UnaryExpressionNot: // '!' CastExpression
		p.w("%sBool%s(!(", p.task.crt, p.helperType(t))
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprBool, flags|fOutermost)
		p.w("))")
	case cc.UnaryExpressionSizeofExpr: // "sizeof" UnaryExpression
		defer p.w("%s", p.convert(nil, t, flags))
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
		p.w("unsafe.Sizeof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionSizeofType: // "sizeof" '(' TypeName ')'
		defer p.w("%s", p.convert(nil, t, flags))
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
		p.w("unsafe.Sizeof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionLabelAddr: // "&&" IDENTIFIER
		panic(todo("", n.Position()))
	case cc.UnaryExpressionAlignofExpr: // "_Alignof" UnaryExpression
		if n.TypeName.Type().Kind() == cc.Void {
			p.intConst("", n.Operand, t, flags)
			break
		}

		defer p.w("%s", p.convert(nil, t, flags))
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
		p.w("unsafe.Alignof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionAlignofType: // "_Alignof" '(' TypeName ')'
		if n.TypeName.Type().Kind() == cc.Void {
			p.intConst("", n.Operand, t, flags)
			break
		}

		defer p.w("%s", p.convert(nil, t, flags))
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
		p.w("unsafe.Alignof(%s%s)", p.typ(t), s)
	case cc.UnaryExpressionImag: // "__imag__" UnaryExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionReal: // "__real__" UnaryExpression
		panic(todo("", n.Position()))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) unaryExpressionLValue(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.UnaryExpressionPostfix: // PostfixExpression
		p.postfixExpression(f, n.PostfixExpression, t, s, mode, flags)
	case cc.UnaryExpressionInc: // "++" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionDec: // "--" UnaryExpression
		panic(todo(""))
	case cc.UnaryExpressionAddrof: // '&' CastExpression
		panic(todo("", n.Position()))
	case cc.UnaryExpressionDeref: // '*' CastExpression
		p.unaryExpressionDeref(f, n, t, s, mode, flags)
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

func (p *project) unaryExpressionPreIncDec(f *function, n *cc.UnaryExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	switch mode {
	case exprValue:
		p.unaryExpressionPreIncDecValue(f, n, oper, oper2, t, s, mode, flags)
	case exprVoid, exprVoidSingle:
		p.unaryExpressionPreIncDecVoid(f, n, oper, oper2, t, s, mode, flags)
	default:
		panic(todo("", pos(n), mode))
	}
}

func (p *project) unaryExpressionPreIncDecVoid(f *function, n *cc.UnaryExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionPreIncDecVoidNormal(f, n, oper, oper2, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionPreIncDecVoidNormal(f *function, n *cc.UnaryExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	ut := n.UnaryExpression.Operand.Type()
	p.unaryExpression(f, n.UnaryExpression, n.UnaryExpression.Operand.Type(), s, exprLValue, flags)
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

func (p *project) unaryExpressionPreIncDecValue(f *function, n *cc.UnaryExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionPreIncDecValueNormal(f, n, oper, oper2, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionPreIncDecValueNormal(f *function, n *cc.UnaryExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	// "++" UnaryExpression etc.
	defer p.w("%s", p.convert(n.UnaryExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	ut := n.UnaryExpression.Operand.Type()
	p.w("%sPre%s%s(&", p.task.crt, x, p.helperType(ut))
	p.unaryExpression(f, n.UnaryExpression, ut, s, exprLValue, flags)
	p.w(", %d)", p.incDelta(n.PostfixExpression, ut))
}

func (p *project) unaryExpressionDeref(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch mode {
	case exprValue:
		p.unaryExpressionDerefValue(f, n, t, s, mode, flags)
	case exprLValue:
		p.unaryExpressionDerefLValue(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.unaryExpressionDerefAddrOf(f, n, t, s, mode, flags)
	case exprBool:
		p.unaryExpressionDerefBool(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) unaryExpressionDerefBool(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	if flags&fOutermost == 0 {
		p.w("(")
		defer p.w(")")
	}
	p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags|fOutermost)
	p.w(")) != 0")
}

func (p *project) unaryExpressionDerefAddrOf(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags)
}

func (p *project) unaryExpressionDerefLValue(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch k := p.opKind(f, n.CastExpression, n.CastExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionDerefLValueNormal(f, n, t, s, mode, flags)
	case opArray:
		panic(todo(""))
		p.unaryExpressionDerefLValueArray(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionDerefLValueArray(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	defer p.w("))%s", p.convertType(n.CastExpression.Operand.Type().Elem(), t, flags))
	p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags|fOutermost)
}

func (p *project) unaryExpressionDerefLValueNormal(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	defer p.w("))%s", p.convertType(n.CastExpression.Operand.Type().Elem(), t, flags))
	p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags|fOutermost)
}

func (p *project) unaryExpressionDerefValue(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch k := p.opKind(f, n.CastExpression, n.CastExpression.Operand.Type()); k {
	case opNormal:
		p.unaryExpressionDerefValueNormal(f, n, t, s, mode, flags)
	case opArray:
		p.unaryExpressionDerefValueArray(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) unaryExpressionDerefValueArray(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	defer p.w("%s", p.convertType(n.CastExpression.Operand.Type().Elem(), t, flags))
	p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, exprValue, flags|fOutermost)
	p.w("[0]")
}

func (p *project) unaryExpressionDerefValueNormal(f *function, n *cc.UnaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	// '*' CastExpression
	switch op := n.Operand.Type(); {
	case op.Kind() == cc.Array:
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, mode, flags)
	default:
		defer p.w("))%s", p.convertType(n.CastExpression.Operand.Type().Elem(), t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
		p.castExpression(f, n.CastExpression, n.CastExpression.Operand.Type(), s, mode, flags|fOutermost)
	}
}

func (p *project) postfixExpression(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprLValue:
		p.postfixExpressionLValue(f, n, t, s, mode, flags)
	case exprValue:
		p.postfixExpressionValue(f, n, t, s, mode, flags)
	case exprVoid:
		p.postfixExpressionVoid(f, n, t, s, mode, flags)
	case exprFunc:
		p.postfixExpressionFunc(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.postfixExpressionAddrOf(f, n, t, s, mode, flags)
	case exprVoidSingle:
		p.postfixExpressionVoidSingle(f, n, t, s, mode, flags)
	case exprSelect:
		p.postfixExpressionSelect(f, n, t, s, mode, flags)
	case exprPSelect:
		p.postfixExpressionPSelect(f, n, t, s, mode, flags)
	case exprBool:
		p.postfixExpressionBool(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) postfixExpressionBool(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, s, exprValue, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, t, s, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, s, exprValue, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, s, exprValue, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, s, exprValue, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		defer p.w(" != 0")
		p.postfixExpression(f, n, t, s, exprValue, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionPSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionPSelectIndex(f, n, t, s, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionPSelectCall(f, n, t, s, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpression(f, n, t, s, exprValue, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionPSelectPSelect(f, n, t, s, mode, flags)
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

func (p *project) postfixExpressionPSelectCall(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	p.w("(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type().Elem()))
	p.postfixExpressionCall(f, n, t, s, exprValue, flags)
	p.w("))")
}

func (p *project) postfixExpressionPSelectIndex(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	// case opArray:
	// 	p.postfixExpressionSelectIndexArray(f, n, t, s, mode, flags)
	case opNormal:
		p.postfixExpressionPSelectIndexNormal(f, n, t, s, mode, flags)
	// case opArrayParameter:
	// 	p.postfixExpressionSelectIndexArrayParamater(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionPSelectIndexNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	case pe.Kind() != cc.Ptr:
		panic(todo(""))
	default:
		if flags&fOutermost == 0 {
			p.w("(")
			p.w(")")
		}
		p.w("(*(**%s)(unsafe.Pointer(", p.typ(n.Operand.Type().Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags|fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w(")))")
	}
}

func (p *project) postfixExpressionSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionSelectIndex(f, n, t, s, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionSelectSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionSelectPSelect(f, n, t, s, mode, flags)
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

func (p *project) postfixExpressionPSelectPSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opNormal:
		p.postfixExpressionPSelectPSelectNormal(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionPSelectPSelectNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.w("(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type().Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectPSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opNormal:
		p.postfixExpressionSelectPSelectNormal(f, n, t, s, mode, flags)
	case opUnion:
		p.postfixExpressionSelectPSelectUnion(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionSelectPSelectUnion(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n.Operand, t, flags))
		fld, ok := pe.Elem().FieldByName(n.Token2.Value)

		if !ok {
			p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
			return
		}

		p.w("(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, fld.Type(), exprValue, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectPSelectNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionSelectSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opUnion:
		p.postfixExpressionSelectSelectUnion(f, n, t, s, mode, flags)
	case opNormal:
		p.postfixExpressionSelectSelectNormal(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionSelectSelectNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionSelectSelectUnion(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n.Operand, t, flags))
		fld, ok := pe.FieldByName(n.Token2.Value)

		if !ok {
			p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
			return
		}
		p.w("(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, fld.Type(), exprAddrOf, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectIndex(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opArray:
		p.postfixExpressionSelectIndexArray(f, n, t, s, mode, flags)
	case opNormal:
		p.postfixExpressionSelectIndexNormal(f, n, t, s, mode, flags)
	case opArrayParameter:
		p.postfixExpressionSelectIndexArrayParamater(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionSelectIndexArrayParamater(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		p.w("(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectIndexNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo("", pos(n)))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	case pe.Kind() != cc.Ptr:
		p.w("(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprAddrOf, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	default:
		p.w("(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	}
}

func (p *project) postfixExpressionSelectIndexArray(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, s, mode, flags&^fOutermost)
		p.w("[")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost)
		p.w("]")
	}
}

func (p *project) postfixExpressionVoidSingle(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		panic(todo(""))
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		panic(todo(""))
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		panic(todo(""))
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		panic(todo(""))
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionAddrOf(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionAddrOfIndex(f, n, t, s, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionAddrOfSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionAddrOfPSelect(f, n, t, s, mode, flags)
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

func (p *project) postfixExpressionAddrOfPSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case pe.Kind() == cc.Array:
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprAddrOf, flags)
		p.fldOff(pe.Elem(), n.Token2)
	default:
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags)
		p.fldOff(pe.Elem(), n.Token2)
	}
}

func (p *project) postfixExpressionAddrOfIndex(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
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
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags)
		case pe.Kind() == cc.Array && d != nil && d.IsParameter:
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags)
		default:
			p.postfixExpression(f, n.PostfixExpression, pe, s, mode, flags)
		}
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags) }, n.Expression.Operand)
		if sz := pe.Decay().Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	}
}

func (p *project) postfixExpressionAddrOfSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		fallthrough
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, nil, s, mode, flags)
		p.fldOff(pe, n.Token2)
	}
}

func (p *project) postfixExpressionFunc(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		switch n.Operand.Type().Kind() {
		case cc.Ptr:
			p.w("(*(*")
			p.functionSignature(f, n.Operand.Type().Elem(), "")
			p.w(")(unsafe.Pointer(")
			p.postfixExpression(f, n, t, s, exprValue, flags)
			p.w(")))")
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		switch n.Operand.Type().Kind() {
		case cc.Ptr:
			p.w("(*(*")
			p.functionSignature(f, n.Operand.Type().Elem(), "")
			p.w(")(unsafe.Pointer(")
			p.postfixExpression(f, n, t, s, exprValue, flags)
			p.w(")))")
		default:
			panic(todo("", n.Position(), n.Operand.Type()))
		}
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		switch n.Operand.Type().Kind() {
		case cc.Ptr:
			p.w("(*(*")
			p.functionSignature(f, n.Operand.Type().Elem(), "")
			p.w(")(unsafe.Pointer(")
			p.postfixExpression(f, n, t, s, exprValue, flags)
			p.w(")))")
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

func (p *project) postfixExpressionVoid(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		panic(todo(""))
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, t, s, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		panic(todo(""))
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		panic(todo(""))
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionValue(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		if p.isArray(f, n.PrimaryExpression, n.Operand.Type()) && t.Kind() == cc.Ptr {
			mode = exprAddrOf
		}
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionValueIndex(f, n, t, s, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		p.postfixExpressionCall(f, n, t, s, mode, flags)
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionValueSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionValuePSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionValuePSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opNormal:
		p.postfixExpressionValuePSelectNormal(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionValuePSelectNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().IsBitFieldType():
		fld, ok := pe.Elem().FieldByName(n.Token2.Value)
		if !ok {
			panic(todo(""))
		}

		defer p.w("%s", p.convertType(fld.Promote(), t, flags))
		switch pe.Kind() {
		case cc.Array:
			x := p.convertType(nil, fld.Promote(), flags)
			p.w("*(*%s)(unsafe.Pointer(", p.typ(fld.Type()))
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprAddrOf, flags)
			p.fldOff(pe.Elem(), n.Token2)
			p.w("))")
			p.w("&%#x>>%d", fld.Mask(), fld.BitFieldOffset())
			p.w("%s", x)
			if fld.Type().IsSignedType() {
				p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
			}
		default:
			x := p.convertType(nil, fld.Promote(), flags)
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprPSelect, flags)
			p.w(".%s /* .%s */", p.bitFieldName(fld.BitFieldBlockFirst()), fld.Name())
			p.w("&%#x>>%d", fld.Mask(), fld.BitFieldOffset())
			p.w("%s", x)
			if fld.Type().IsSignedType() {
				p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
			}
		}
	case n.Operand.Type().Kind() == cc.Array:
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags)
		p.fldOff(n.PostfixExpression.Operand.Type().Elem(), n.Token2)
	default:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionValueIndex(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opArray:
		p.postfixExpressionValueIndexArray(f, n, t, s, mode, flags)
	case opNormal:
		p.postfixExpressionValueIndexNormal(f, n, t, s, mode, flags)
	case opArrayParameter:
		p.postfixExpressionValueIndexArrayParameter(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}
func (p *project) postfixExpressionValueIndexArrayParameter(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags|fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	default:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.w("*(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags&^fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
		p.w("))")
	}
}

func (p *project) postfixExpressionValueIndexNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags|fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	default:
		switch pe := n.PostfixExpression.Operand.Type(); pe.Kind() {
		case cc.Ptr:
			defer p.w("%s", p.convert(n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		case cc.Array:
			defer p.w("%s", p.convert(n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprAddrOf, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		default:
			panic(todo("", pos(n), pe))
		}
	}
}

func (p *project) postfixExpressionValueIndexArray(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	pe := n.PostfixExpression.Operand.Type()
	switch n.Operand.Type().Kind() {
	case cc.Array:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		if flags&fOutermost == 0 {
			p.w("(")
			defer p.w(")")
		}
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprAddrOf, flags|fOutermost)
		p.w(" + ")
		p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
		if sz := pe.Elem().Size(); sz != 1 {
			p.w("*%d", sz)
		}
	default:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, s, mode, flags&^fOutermost)
		p.w("[")
		p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost)
		p.w("]")
	}
}

func (p *project) postfixExpressionValueSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opNormal:
		p.postfixExpressionValueSelectNormal(f, n, t, s, mode, flags)
	case opUnion:
		p.postfixExpressionValueSelectUnion(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionValueSelectUnion(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		fld, ok := pe.FieldByName(n.Token2.Value)
		if !ok {
			p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
			return
		}

		p.postfixExpression(f, n.PostfixExpression, pe, fld.Type(), exprAddrOf, flags|fOutermost)
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		fld, ok := pe.FieldByName(n.Token2.Value)
		if !ok {
			p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
			return
		}

		p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, fld.Type(), exprAddrOf, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionValueSelectNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	pe := n.PostfixExpression.Operand.Type()
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		p.postfixExpression(f, n, t, s, exprAddrOf, flags)
	case n.Operand.Type().IsBitFieldType():
		fld, ok := pe.FieldByName(n.Token2.Value)
		if !ok {
			p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
			return
		}

		defer p.w("%s", p.convertType(fld.Promote(), t, flags))
		x := p.convertType(nil, fld.Promote(), flags)
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprSelect, flags)
		p.w(".%s /* .%s */", p.bitFieldName(fld.BitFieldBlockFirst()), fld.Name())
		p.w("&%#x>>%d", fld.Mask(), fld.BitFieldOffset())
		p.w("%s", x)
		if fld.Type().IsSignedType() {
			p.w("<<%d>>%[1]d", int(fld.Promote().Size()*8)-fld.BitFieldWidth())
		}

	default:
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionLValue(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PostfixExpressionPrimary: // PrimaryExpression
		p.primaryExpression(f, n.PrimaryExpression, t, s, mode, flags)
	case cc.PostfixExpressionIndex: // PostfixExpression '[' Expression ']'
		p.postfixExpressionLValueIndex(f, n, t, s, mode, flags)
	case cc.PostfixExpressionCall: // PostfixExpression '(' ArgumentExpressionList ')'
		panic(todo(""))
	case cc.PostfixExpressionSelect: // PostfixExpression '.' IDENTIFIER
		p.postfixExpressionLValueSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionPSelect: // PostfixExpression "->" IDENTIFIER
		p.postfixExpressionLValuePSelect(f, n, t, s, mode, flags)
	case cc.PostfixExpressionInc: // PostfixExpression "++"
		p.postfixExpressionIncDec(f, n, "++", "+=", t, s, mode, flags)
	case cc.PostfixExpressionDec: // PostfixExpression "--"
		p.postfixExpressionIncDec(f, n, "--", "-=", t, s, mode, flags)
	case cc.PostfixExpressionComplit: // '(' TypeName ')' '{' InitializerList ',' '}'
		panic(todo(""))
	case cc.PostfixExpressionTypeCmp: // "__builtin_types_compatible_p" '(' TypeName ',' TypeName ')'
		panic(todo(""))
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) postfixExpressionLValuePSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression "->" IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type().Elem()); k {
	case opNormal:
		p.postfixExpressionLValuePSelectNormal(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionLValuePSelectNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		defer p.w("%s", p.convert(n.Operand, t, flags))
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprPSelect, flags)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionLValueIndex(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opArray:
		p.postfixExpressionLValueIndexArray(f, n, t, s, mode, flags)
	case opNormal:
		p.postfixExpressionLValueIndexNormal(f, n, t, s, mode, flags)
	case opArrayParameter:
		p.postfixExpressionLValueIndexArrayParameter(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionLValueIndexArrayParameter(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	defer p.w("%s", p.convert(n.Operand, t, flags))
	pe := n.PostfixExpression.Operand.Type()
	p.w("*(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
	p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags&^fOutermost)
	p.w(" + ")
	p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
	if sz := pe.Elem().Size(); sz != 1 {
		p.w("*%d", sz)
	}
	p.w("))")
}

func (p *project) postfixExpressionLValueIndexNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	switch {
	case n.Operand.Type().Kind() == cc.Array:
		panic(todo(""))
	default:
		switch pe := n.PostfixExpression.Operand.Type(); pe.Kind() {
		case cc.Ptr:
			defer p.w("%s", p.convert(n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprValue, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		case cc.Array:
			defer p.w("%s", p.convert(n.Operand, t, flags))
			p.w("*(*%s)(unsafe.Pointer(", p.typ(pe.Elem()))
			p.postfixExpression(f, n.PostfixExpression, pe, s, exprAddrOf, flags&^fOutermost)
			p.w(" + ")
			p.uintptr(func() { p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost) }, n.Expression.Operand)
			if sz := pe.Elem().Size(); sz != 1 {
				p.w("*%d", sz)
			}
			p.w("))")
		default:
			panic(todo("", pos(n), pe))
		}
	}
}

func (p *project) postfixExpressionLValueIndexArray(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '[' Expression ']'
	pe := n.PostfixExpression.Operand.Type()
	p.postfixExpression(f, n.PostfixExpression, pe, s, mode, flags&^fOutermost)
	p.w("[")
	p.expression(f, n.Expression, n.Expression.Operand.Type(), s, exprValue, flags|fOutermost)
	p.w("]")
}

func (p *project) postfixExpressionLValueSelect(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '.' IDENTIFIER
	switch k := p.opKind(f, n.PostfixExpression, n.PostfixExpression.Operand.Type()); k {
	case opNormal:
		p.postfixExpressionLValueSelectNormal(f, n, t, s, mode, flags)
	case opUnion:
		p.postfixExpressionLValueSelectUnion(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionLValueSelectUnion(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	fld, ok := pe.FieldByName(n.Token2.Value)
	if !ok {
		p.err(&n.Token2, "uknown field: %s", n.Token2.Value)
		return
	}

	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		p.w("*(*%s)(unsafe.Pointer(", p.typ(n.Operand.Type()))
		p.postfixExpression(f, n.PostfixExpression, pe, fld.Type(), exprAddrOf, flags|fOutermost)
		p.w("))")
	}
}

func (p *project) postfixExpressionLValueSelectNormal(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		pe := n.PostfixExpression.Operand.Type()
		p.postfixExpression(f, n.PostfixExpression, pe, s, exprSelect, flags&^fOutermost)
		p.w(".%s", p.fieldName(n.Token2.Value))
	}
}

func (p *project) postfixExpressionIncDec(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	switch mode {
	case exprVoidSingle, exprVoid:
		p.postfixExpressionIncDecVoid(f, n, oper, oper2, t, s, mode, flags)
	case exprLValue:
		p.postfixExpressionIncDecLValue(f, n, oper, oper2, t, s, mode, flags)
	case exprValue:
		p.postfixExpressionIncDecValue(f, n, oper, oper2, t, s, mode, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) postfixExpressionIncDecValue(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	switch k := p.opKind(f, n, n.Operand.Type()); k {
	case opNormal:
		p.postfixExpressionIncDecValueNormal(f, n, oper, oper2, t, s, mode, flags)
	case opBitfield:
		p.postfixExpressionIncDecValueBitfield(f, n, oper, oper2, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionIncDecValueBitfield(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	defer p.w("%s", p.convert(n.PostfixExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	bf := pe.BitField()
	p.w("%sPost%sBitFieldPtr%d%s(", p.task.crt, x, bf.BitFieldBlockWidth(), p.bfHelperType(pe))
	p.postfixExpression(f, n.PostfixExpression, pe, s, exprAddrOf, flags)
	p.w(", %d, %d, %#x)", p.incDelta(n.PostfixExpression, pe), bf.BitFieldOffset(), bf.Mask())
}

func (p *project) postfixExpressionIncDecValueNormal(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	defer p.w("%s", p.convert(n.PostfixExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	p.w("%sPost%s%s(&", p.task.crt, x, p.helperType(pe))
	p.postfixExpression(f, n.PostfixExpression, pe, s, exprLValue, flags)
	p.w(", %d)", p.incDelta(n.PostfixExpression, pe))
}

func (p *project) postfixExpressionIncDecLValue(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	switch k := p.opKind(f, n, n.Operand.Type()); k {
	case opNormal:
		p.postfixExpressionIncDecLValueNormal(f, n, oper, oper2, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionIncDecLValueNormal(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	defer p.w("%s", p.convert(n.PostfixExpression.Operand, t, flags))
	x := "Dec"
	if oper == "++" {
		x = "Inc"
	}
	p.w("%sPost%s%s(&", p.task.crt, x, p.helperType(pe))
	p.postfixExpression(f, n.PostfixExpression, pe, s, exprLValue, flags)
	p.w(", %d)", p.incDelta(n.PostfixExpression, pe))
}

func (p *project) postfixExpressionIncDecVoid(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	switch k := p.opKind(f, n, n.Operand.Type()); k {
	case opNormal:
		p.postfixExpressionIncDecVoidNormal(f, n, oper, oper2, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) postfixExpressionIncDecVoidNormal(f *function, n *cc.PostfixExpression, oper, oper2 string, t, s cc.Type, mode exprMode, flags flags) {
	pe := n.PostfixExpression.Operand.Type()
	p.postfixExpression(f, n.PostfixExpression, pe, s, exprLValue, flags)
	if pe.IsIntegerType() || pe.Kind() == cc.Ptr && p.incDelta(n, pe) == 1 {
		p.w("%s", oper)
		return
	}

	switch pe.Kind() {
	case cc.Ptr, cc.Float, cc.Double:
		p.w("%s %d", oper2, p.incDelta(n, pe))
		return
	}

	panic(todo(""))
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

func (p *project) postfixExpressionCall(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '(' ArgumentExpressionList ')'
	switch mode {
	case exprVoid:
		p.postfixExpressionCallVoid(f, n, t, s, mode, flags)
	case exprValue:
		p.postfixExpressionCallValue(f, n, t, s, mode, flags)
	case exprBool:
		p.postfixExpressionCallBool(f, n, t, s, mode, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) postfixExpressionCallBool(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
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
			p.w("%sVa%s(&", p.task.crt, p.helperType(f.vaType))
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), nil, exprLValue, flags)
			p.w(")")
			return
		}
	}

	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), s, exprFunc, flags&^fOutermost)
	p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
}

func (p *project) postfixExpressionCallValue(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '(' ArgumentExpressionList ')'
	defer p.w("%s", p.convert(n.Operand, t, flags))
	if d := n.PostfixExpression.Declarator(); d != nil {
		switch d.Name() {
		case idVaEnd:
			p.w("_ = ")
			arg := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, arg, arg.Operand.Type(), nil, exprValue, flags)
			return
		case idVaStart:
			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), nil, exprLValue, flags)
			p.w(" = %s", f.vaName)
			return
		case idVaArg:
			if !f.vaType.IsScalarType() {
				panic(todo("", f.vaType))
			}

			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.w("%sVa%s(&", p.task.crt, p.helperType(f.vaType))
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), nil, exprLValue, flags)
			p.w(")")
			return
		}
	}
	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), s, exprFunc, flags&^fOutermost)
	p.argumentExpressionList(f, n.PostfixExpression, n.ArgumentExpressionList, f.vaLists[n])
}

func (p *project) postfixExpressionCallVoid(f *function, n *cc.PostfixExpression, t, s cc.Type, mode exprMode, flags flags) {
	// PostfixExpression '(' ArgumentExpressionList ')'
	if d := n.PostfixExpression.Declarator(); d != nil {
		switch d.Name() {
		case idVaEnd:
			p.w("_ = ")
			arg := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, arg, arg.Operand.Type(), nil, exprValue, flags)
			return
		case idVaStart:
			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), nil, exprLValue, flags)
			p.w(" = %s", f.vaName)
			return
		case idVaArg:
			if !f.vaType.IsScalarType() {
				panic(todo("", f.vaType))
			}

			lhs := n.ArgumentExpressionList.AssignmentExpression
			p.w("%sVa%s(&", p.task.crt, p.helperType(f.vaType))
			p.assignmentExpression(f, lhs, lhs.Operand.Type(), nil, exprLValue, flags)
			p.w(")")
			return
		}
	}
	p.postfixExpression(f, n.PostfixExpression, n.PostfixExpression.Operand.Type(), s, exprFunc, flags&^fOutermost)
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
		if at := arg.Operand.Type(); at.Kind() == cc.Array && p.detectArray(f, arg, true, true, nil) {
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
	case exprLValue:
		p.primaryExpressionLValue(f, n, t, s, mode, flags)
	case exprValue:
		p.primaryExpressionValue(f, n, t, s, mode, flags)
	case exprFunc:
		p.primaryExpressionFunc(f, n, t, s, mode, flags)
	case exprAddrOf:
		p.primaryExpressionAddrOf(f, n, t, s, mode, flags)
	case exprSelect:
		p.primaryExpressionSelect(f, n, t, s, mode, flags)
	case exprPSelect:
		p.primaryExpressionPSelect(f, n, t, s, mode, flags)
	case exprBool:
		p.primaryExpressionBool(f, n, t, s, mode, flags)
	case exprVoid:
		p.primaryExpressionVoid(f, n, t, s, mode, flags)
	default:
		panic(todo("", n.Position(), mode))
	}
}

func (p *project) primaryExpressionVoid(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	if mode == exprVoid && n.Operand.Value() != nil && n.IsSideEffectsFree {
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
		p.expression(f, n.Expression, t, s, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionBool(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
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
			p.declarator(f, d, d.Type(), s, exprValue, flags)
		default:
			panic(todo("", pos(n)))
		}
	case cc.PrimaryExpressionInt: // INTCONST
		p.intConst(n.Token.Src.String(), n.Operand, n.Operand.Type(), flags)
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
		p.expression(f, n.Expression, t, s, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionPSelect(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			switch k := p.declaratorKind(d); k {
			case opArray:
				panic(todo(""))
				p.primaryExpression(f, n, t, s, exprAddrOf, flags)
			default:
				p.declarator(f, d, t, s, mode, flags)
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
		p.expression(f, n.Expression, t, s, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionSelect(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(f, d, t, s, mode, flags)
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
		panic(todo(""))
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionAddrOf(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(f, d, t, s, mode, flags)
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
		p.expression(f, n.Expression, t, s, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionFunc(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(f, d, t, s, mode, flags)
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
		p.expression(f, n.Expression, t, s, mode, flags)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionValue(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
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
		panic(todo(""))
	case cc.PrimaryExpressionString: // STRINGLITERAL
		p.w("%s", p.stringLiteral(n.Operand.Value()))
	case cc.PrimaryExpressionLString: // LONGSTRINGLITERAL
		panic(todo(""))
	case cc.PrimaryExpressionExpr: // '(' Expression ')'
		p.w("(")
		defer p.w(")")
		p.expression(f, n.Expression, t, s, mode, flags|fOutermost)
	case cc.PrimaryExpressionStmt: // '(' CompoundStatement ')'
		p.err(n, "statement expressions not supported")
	default:
		panic(todo("%v: internal error: %v", n.Position(), n.Case))
	}
}

func (p *project) primaryExpressionLValue(f *function, n *cc.PrimaryExpression, t, s cc.Type, mode exprMode, flags flags) {
	switch n.Case {
	case cc.PrimaryExpressionIdent: // IDENTIFIER
		switch d := n.Declarator(); {
		case d != nil:
			p.declarator(f, d, t, s, mode, flags)
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
		p.expression(f, n.Expression, t, s, mode, flags|fOutermost)
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
	return fmt.Sprintf("%s%s", p.tsNameP, nonZeroUintptr(off))
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

			p.w("math.Float64frombits(%#x)", math.Float64bits(float64(x)))
		case cc.Float:
			if snValid && float32(sn) == float32(x) { // Prefer original form.
				p.w("%s", src)
				return
			}

			p.w("math.Float32frombits(%#x)", math.Float32bits(float32(x)))
		default:
			defer p.w("%s", p.convert(op, to, 0))
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
					defer p.w("%s%s", s, p.convertType(op.Type(), to, 0))
					break
				}
			}

			defer p.w("%s", p.convert(op, to, 0))
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

func (p *project) intConst(src string, op cc.Operand, to cc.Type, flags flags) {
	ptr := to.Kind() == cc.Ptr
	switch {
	case to.IsArithmeticType():
		defer p.w("%s", p.convert(op, to, flags))
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
			panic(todo(""))
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

func (p *project) assignShiftOp(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "<<=" AssignmentExpression etc.
	switch mode {
	case exprVoid:
		p.assignShiftOpVoid(f, n, t, s, mode, oper, oper2, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) assignShiftOpVoid(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "<<=" AssignmentExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.assignShiftOpVoidNormal(f, n, t, s, mode, oper, oper2, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignShiftOpVoidNormal(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	switch {
	case n.Operand.Type().IsBitFieldType():
		panic(todo(""))
	default:
		if d := n.UnaryExpression.Declarator(); d != nil {
			p.declarator(f, d, d.Type(), s, exprLValue, flags|fOutermost)
			p.w(" %s= ", oper)
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
			return
		}

		panic(todo(""))
	}
}

func (p *project) assignOp(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	switch mode {
	case exprVoid, exprVoidSingle:
		p.assignOpVoid(f, n, t, s, mode, oper, oper2, flags)
	case exprValue:
		p.assignOpValue(f, n, t, s, mode, oper, oper2, flags)
	default:
		panic(todo("", mode))
	}
}

func (p *project) assignOpValue(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.assignOpValueNormal(f, n, t, s, oper, oper2, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignOpValueNormal(f *function, n *cc.AssignmentExpression, t, s cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	if d := n.UnaryExpression.Declarator(); d != nil {
		if local := f.locals[d]; local != nil && local.isPinned {
			panic(todo("", pos(n), pos(d), d.Name()))
			//TODO- 	p.declarator(f, d, d.Type(), s, exprLValue, flags)
			//TODO- 	switch {
			//TODO- 	case d.Type().IsArithmeticType():
			//TODO- 		p.w(" %s= ", oper)
			//TODO- 		defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
			//TODO- 		p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
			//TODO- 	default:
			//TODO- 		panic(todo("", d.Type().Kind()))
			//TODO- 	}
			//TODO- 	return
		}

		switch {
		case d.Type().IsArithmeticType():
			defer p.w("%s", p.convertType(d.Type(), t, flags))
			p.w("%sAssign%s%s(&", p.task.crt, oper2, p.helperType(d.Type()))
			p.declarator(f, d, d.Type(), s, exprLValue, flags|fOutermost)
			p.w(", ")
			p.assignmentExpression(f, n.AssignmentExpression, d.Type(), s, exprValue, flags|fOutermost)
			p.w(")")
		default:
			panic(todo("", pos(n), pos(d), d.Name()))
		}
		//TODO- p.declarator(f, d, d.Type(), s, exprLValue, flags|fOutermost)
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
		//TODO- 	p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
		//TODO- default:
		//TODO- 	p.w(" = ")
		//TODO- 	defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
		//TODO- 	p.declarator(f, d, n.Promote(), s, exprValue, flags|fOutermost)
		//TODO- 	p.w(" %s (", oper)
		//TODO- 	p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
		//TODO- 	p.w(")")
		//TODO- }
		return
	}

	lhs := n.UnaryExpression
	switch {
	case lhs.Operand.Type().IsArithmeticType():
		defer p.w("%s", p.convertType(lhs.Operand.Type(), n.Operand.Type(), flags))
		p.w("%sAssign%sPtr%s(", p.task.crt, oper2, p.helperType(lhs.Operand.Type()))
		p.unaryExpression(f, lhs, lhs.Operand.Type(), s, exprAddrOf, flags|fOutermost)
		p.w(", ")
		p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), s, exprValue, flags|fOutermost)
		p.w(")")
	//TODO- case lhs.Operand.Type().Kind() == cc.Ptr:
	//TODO- 	p.w("*(*%s)(unsafe.Pointer(", p.typ(lhs.Operand.Type()))
	//TODO- 	p.unaryExpression(f, lhs, lhs.Operand.Type(), s, exprAddrOf, flags|fOutermost)
	//TODO- 	p.w(")) %s= (", oper)
	//TODO- 	p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), s, exprValue, flags|fOutermost)
	//TODO- 	p.w(")")
	//TODO- 	if dd := p.incDelta(n, lhs.Operand.Type()); dd != 1 {
	//TODO- 		p.w("*%d", dd)
	//TODO- 	}
	default:
		panic(todo("", lhs.Operand.Type()))
	}
}

func (p *project) assignOpVoid(f *function, n *cc.AssignmentExpression, t, s cc.Type, mode exprMode, oper, oper2 string, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	switch k := p.opKind(f, n.UnaryExpression, n.UnaryExpression.Operand.Type()); k {
	case opNormal:
		p.assignOpVoidNormal(f, n, t, s, oper, oper2, mode, flags)
	case opBitfield:
		p.assignOpVoidBitfield(f, n, t, s, oper, oper2, mode, flags)
	case opArrayParameter:
		p.assignOpVoidArrayParameter(f, n, t, s, oper, oper2, mode, flags)
	default:
		panic(todo("", n.Position(), k))
	}
}

func (p *project) assignOpVoidArrayParameter(f *function, n *cc.AssignmentExpression, t, s cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	d := n.UnaryExpression.Declarator()
	if local := f.locals[d]; local != nil && local.isPinned {
		panic(todo(""))
	}

	p.declarator(f, d, d.Type(), s, exprLValue, flags|fOutermost)
	if oper != "+" && oper != "-" {
		panic(todo(""))
	}

	p.w(" %s= ", oper)
	if dd := p.incDelta(d, d.Type()); dd != 1 {
		p.w("%d*", dd)
	}
	p.w("uintptr(")
	p.assignmentExpression(f, n.AssignmentExpression, n.AssignmentExpression.Operand.Type(), s, exprValue, flags|fOutermost)
	p.w(")")
}

func (p *project) assignOpVoidBitfield(f *function, n *cc.AssignmentExpression, t, s cc.Type, oper, oper2 string, mode exprMode, flags flags) {
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
				p.unaryExpression(f, lhs, lt, nil, exprAddrOf, flags)
				p.w(", (")
				s := p.convertType(lt, n.Promote(), flags)
				p.unaryExpression(f, lhs, lt, nil, exprValue, flags)
				p.w(")%s %s ", s, oper)
				s = p.convertType(lt, n.Promote(), flags)
				p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), nil, exprValue, flags|fOutermost)
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

func (p *project) assignOpVoidNormal(f *function, n *cc.AssignmentExpression, t, s cc.Type, oper, oper2 string, mode exprMode, flags flags) {
	// UnaryExpression "*=" AssignmentExpression etc.
	if d := n.UnaryExpression.Declarator(); d != nil {
		if local := f.locals[d]; local != nil && local.isPinned {
			p.declarator(f, d, d.Type(), s, exprLValue, flags)
			switch {
			case d.Type().IsArithmeticType():
				p.w(" %s= ", oper)
				defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
				p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
			default:
				panic(todo("", d.Type().Kind()))
			}
			return
		}

		p.declarator(f, d, d.Type(), s, exprLValue, flags|fOutermost)
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
			defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
		default:
			p.w(" = ")
			defer p.w("%s", p.convertType(n.Promote(), d.Type(), flags))
			p.declarator(f, d, n.Promote(), s, exprValue, flags|fOutermost)
			p.w(" %s (", oper)
			p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
			p.w(")")
		}
		return
	}

	lhs := n.UnaryExpression
	switch {
	case lhs.Operand.Type().IsArithmeticType():
		p.w("*(*%s)(unsafe.Pointer(", p.typ(lhs.Operand.Type()))
		p.unaryExpression(f, lhs, lhs.Operand.Type(), s, exprAddrOf, flags|fOutermost)
		p.w(")) %s= ", oper)
		defer p.w("%s", p.convertType(n.Promote(), lhs.Operand.Type(), flags))
		p.w("(")
		p.assignmentExpression(f, n.AssignmentExpression, n.Promote(), s, exprValue, flags|fOutermost)
		p.w(")")
	case lhs.Operand.Type().Kind() == cc.Ptr:
		p.w("*(*%s)(unsafe.Pointer(", p.typ(lhs.Operand.Type()))
		p.unaryExpression(f, lhs, lhs.Operand.Type(), s, exprAddrOf, flags|fOutermost)
		p.w(")) %s= (", oper)
		p.assignmentExpression(f, n.AssignmentExpression, lhs.Operand.Type(), s, exprValue, flags|fOutermost)
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
	switch {
	case f.block.isFlat:
		p.w("%s", comment("\n", n))
		switch n.Case {
		case cc.IterationStatementWhile: // "while" '(' Expression ')' Statement
			panic(todo(""))
		case cc.IterationStatementDo: // "do" Statement "while" '(' Expression ')' ';'
			panic(todo(""))
		case cc.IterationStatementFor: // "for" '(' Expression ';' Expression ';' Expression ')' Statement
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
				p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprVoidSingle, fOutermost)
			}
			p.w("; __%d:", a)
			if n.Expression2 != nil {
				p.w("if !(")
				p.expression(f, n.Expression2, n.Expression2.Operand.Type(), nil, exprBool, fOutermost)
				p.w(") { goto __%d }", c)
			}
			p.w("; ")
			p.statement(f, n.Statement, false)
			p.w(";goto __%d; __%[1]d:", b)
			if n.Expression3 != nil {
				p.expression(f, n.Expression3, n.Expression3.Operand.Type(), nil, exprVoidSingle, fOutermost)
			}
			p.w(";goto __%d; __%d:", a, c)
		case cc.IterationStatementForDecl: // "for" '(' Declaration Expression ';' Expression ')' Statement
			panic(todo(""))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
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
				p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprVoidSingle, fOutermost)
			}
			p.w("; ")
			if n.Expression2 != nil {
				p.expression(f, n.Expression2, n.Expression2.Operand.Type(), nil, exprBool, fOutermost)
			}
			p.w("; ")
			if n.Expression3 != nil {
				p.expression(f, n.Expression3, n.Expression3.Operand.Type(), nil, exprVoidSingle, fOutermost)
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
	p.w("%s", comment("\n", n))
	switch {
	case f.block.isFlat:
		switch n.Case {
		case cc.SelectionStatementIf: // "if" '(' Expression ')' Statement
			// if !expr goto a
			// stmt
			// a:
			sv := f.ifCtx
			f.ifCtx = n
			a := f.flatLabel()
			p.w("if !(")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.w(") { goto __%d };", a)
			p.statement(f, n.Statement, false)
			p.w(";__%d: ", a)
			f.ifCtx = sv
		case cc.SelectionStatementIfElse: // "if" '(' Expression ')' Statement "else" Statement
			// if !expr goto a
			// stmt
			// goto b
			// a:
			// stmt2
			// b:
			sv := f.ifCtx
			f.ifCtx = n
			a := f.flatLabel()
			b := f.flatLabel()
			p.w("if !(")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.w(") { goto __%d };", a)
			p.statement(f, n.Statement, false)
			p.w(";goto __%d; __%d:", b, a)
			p.statement(f, n.Statement2, false)
			p.w(";__%d:", b)
			f.ifCtx = sv
		case cc.SelectionStatementSwitch: // "switch" '(' Expression ')' Statement
			panic(todo(""))
		default:
			panic(todo("%v: internal error: %v", n.Position(), n.Case))
		}
	default:
		switch n.Case {
		case cc.SelectionStatementIf: // "if" '(' Expression ')' Statement
			sv := f.ifCtx
			f.ifCtx = n
			p.w("if ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.statement(f, n.Statement, true)
			f.ifCtx = sv
		case cc.SelectionStatementIfElse: // "if" '(' Expression ')' Statement "else" Statement
			sv := f.ifCtx
			f.ifCtx = n
			p.w("if ")
			p.expression(f, n.Expression, n.Expression.Operand.Type(), nil, exprBool, fOutermost)
			p.statement(f, n.Statement, true)
			p.w(" else ")
			p.statement(f, n.Statement2, true)
			f.ifCtx = sv
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
		p.labeledStatementFlat(f, n)
		return
	}

	switch n.Case {
	case cc.LabeledStatementLabel: // IDENTIFIER ':' AttributeSpecifierList Statement
		if _, ok := f.unusedLabels[n.Token.Value]; ok {
			p.w("goto %s;", f.labelNames[n.Token.Value])
		}
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
		if _, ok := f.unusedLabels[n.Token.Value]; ok {
			p.w("goto %s;", f.labelNames[n.Token.Value])
		}
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
		case f == nil:
			p.w(", uintptr")
		default:
			p.w(", %s uintptr", f.vaName)
		}
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