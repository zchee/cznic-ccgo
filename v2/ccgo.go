// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package ccgo translates C99 ASTs to Go source code. Work In Progress. API unstable.
package ccgo // import "modernc.org/ccgo/v2"

//TODO must respect 'volatile' -> use sync.Atomic

import (
	"bytes"
	"container/list"
	"fmt"
	"go/token"
	"io"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"sort"
	"strings"

	"modernc.org/cc/v2"
	"modernc.org/ir"
)

var (
	isTesting bool // Test hook
	log       = func(string, ...interface{}) {}
	logging   bool
)

func init() {
	if fn := os.Getenv("CCGOLOG"); fn != "" {
		logging = true
		f, err := os.OpenFile(fn, os.O_APPEND|os.O_CREATE|os.O_WRONLY|os.O_SYNC, 0644)
		if err != nil {
			panic(err)
		}

		pid := fmt.Sprintf("[pid %v] ", os.Getpid())

		log = func(s string, args ...interface{}) {
			if s == "" {
				s = strings.Repeat("%v ", len(args))
			}
			_, fn, fl, _ := runtime.Caller(1)
			s = fmt.Sprintf(pid+"%s:%d: "+s, append([]interface{}{filepath.Base(fn), fl}, args...)...)
			switch {
			case len(s) != 0 && s[len(s)-1] == '\n':
				fmt.Fprint(f, s)
			default:
				fmt.Fprintln(f, s)
			}
		}
	}
}

type gen struct {
	allocatedStack     int
	crtPrefix          string
	enqueued           map[interface{}]struct{}
	enumConsts         map[int]struct{}
	err                error
	file               string
	helpers            map[string]int
	in                 *cc.TranslationUnit
	model              cc.Model
	nextLabel          int
	num                int
	nums               map[*cc.Declarator]int
	opaqueStructTags   map[int]struct{}
	out                io.Writer
	out0               bytes.Buffer
	producedEnumTags   map[int]struct{}
	producedNamedTypes map[int]struct{}
	producedStructTags map[int]struct{}
	producedTLDs       map[string]struct{}
	queue              list.List
	tCache             map[tCacheKey]string
	tldPreamble        bytes.Buffer
	tweaks             NewObjectTweaks

	needAlloca bool
	needNZ32   bool //TODO -> crt
	needNZ64   bool //TODO -> crt
}

func newGen(out io.Writer, in *cc.TranslationUnit, file string, tweaks *NewObjectTweaks) *gen {
	crtPrefix := crt
	if tweaks.FreeStanding {
		crtPrefix = ""
	}
	return &gen{
		crtPrefix:          crtPrefix,
		enqueued:           map[interface{}]struct{}{},
		enumConsts:         map[int]struct{}{},
		file:               file,
		helpers:            map[string]int{},
		in:                 in,
		model:              in.Model,
		nums:               map[*cc.Declarator]int{},
		opaqueStructTags:   map[int]struct{}{},
		out:                out,
		producedEnumTags:   map[int]struct{}{},
		producedNamedTypes: map[int]struct{}{},
		producedStructTags: map[int]struct{}{},
		producedTLDs:       map[string]struct{}{},
		tCache:             map[tCacheKey]string{},
		tweaks:             *tweaks,
	}
}

func (g *gen) enqueue(n interface{}) {
	if _, ok := g.enqueued[n]; ok {
		return
	}

	g.enqueued[n] = struct{}{}
	switch x := n.(type) {
	case *cc.Declarator:
		if x.Linkage == cc.LinkageNone {
			return
		}

		if x.Linkage == cc.LinkageInternal {
			g.enqueueNumbered(x)
			return
		}

		if x.DeclarationSpecifier.IsExtern() {
			//TODO?
			return
		}
	}

	g.queue.PushBack(n)
}

func (g *gen) enqueueNumbered(n *cc.Declarator) {
	if _, ok := g.nums[n]; ok {
		return
	}

	g.num++
	g.nums[n] = g.num
	g.queue.PushBack(n)
}

func (g *gen) gen() (err error) {
	//traceWrites = true //TODO- DBG
	defer func() {
		if g.err == nil {
			g.err = err
		}
	}()

	if g.crtPrefix == "" {
		g.w("\nconst Lfreestanding = \"1\"\n")
	}

	if g.tweaks.DefineValues {
		g.defs()
	}

	for l := g.in.ExternalDeclarationList; l != nil; l = l.ExternalDeclarationList {
		switch n := l.ExternalDeclaration; n.Case {
		case cc.ExternalDeclarationDecl: // Declaration
			if o := n.Declaration.InitDeclaratorListOpt; o != nil {
				for l := o.InitDeclaratorList; l != nil; l = l.InitDeclaratorList {
					d := l.InitDeclarator.Declarator
					ds := d.DeclarationSpecifier
					if ds.IsTypedef() {
						continue
					}

					if d.Linkage != cc.LinkageExternal && len(d.Attributes) == 0 {
						continue
					}

					t := cc.UnderlyingType(d.Type)
					switch {
					case t.Kind() == cc.Function:
						if len(d.Attributes) == 0 {
							continue
						}
					default:
						if d.DeclarationSpecifier.IsExtern() {
							if len(d.Attributes) != 0 {
								g.linkInfo(d, true)
								if _, err := g.out.Write(g.out0.Bytes()); err != nil {
									return err
								}

								g.out0.Reset()
							}
							continue
						}
					}

					g.tld(d)
				}
				break
			}

			for _, v := range n.Declaration.Attributes {
				if len(v) == 0 {
					continue
				}

				switch t := v[0]; t.Rune {
				case cc.IDENTIFIER:
					switch t.Val {
					case idPacked, idPacked2:
						//TODO Can we always safely ignore the __packed__ attribute of a struct?
					default:
						todo("", g.position(n), cc.PrettyString(v))
					}
				default:
					todo("", g.position(n), cc.PrettyString(v))
				}
			}
		case cc.ExternalDeclarationFunc: // FunctionDefinition
			d := n.FunctionDefinition.Declarator
			if d.Linkage == cc.LinkageExternal || len(d.Attributes) != 0 {
				g.tld(d)
			}
		default:
			todo("unexpected %v", n.Case)
		}
	}
	g.defineQueued()

	var a []string
	for k := range g.opaqueStructTags {
		a = append(a, string(dict.S(k)))
	}
	sort.Strings(a)
	for _, k := range a {
		tag := dict.SID(k)
		if _, ok := g.producedStructTags[tag]; !ok {
			g.w("\n\ntype S%s struct{ uintptr }", k)
		}
	}

	if err := newOpt().do(g.out, &g.out0, testFn); err != nil {
		todo("", err)
	}
	return nil
}

func (g *gen) defs() {
	tu := g.in
	var a []string
	for _, v := range tu.Macros {
		if v.IsFnLike {
			continue
		}

		nm := string(dict.S(v.DefTok.Val))
		if strings.HasPrefix(nm, "__") {
			continue
		}

		a = append(a, nm)
	}
	sort.Strings(a)
	for _, nm := range a {
		v := tu.Macros[dict.SID(nm)]
		if v == nil {
			continue
		}

		op, err := v.Eval(tu.Model, tu.Macros)
		if err != nil {
			continue
		}

		switch x := op.Value.(type) {
		case *ir.Float32Value:
			if f := float64(x.Value); math.IsInf(f, 0) || math.IsNaN(f) || f == 0 && math.Signbit(f) {
				break
			}

			g.w("\n%sDD%s = \"%v\"\n", lConstPrefix, nm, x.Value)
		case *ir.Float64Value:
			if f := x.Value; math.IsInf(f, 0) || math.IsNaN(f) || f == 0 && math.Signbit(f) {
				break
			}

			g.w("\n%sDD%s = \"%v\"\n", lConstPrefix, nm, x.Value)
		case *ir.Int64Value:
			switch {
			case op.Type.IsUnsigned():
				g.w("\n%sDD%s = \"%v\"\n", lConstPrefix, nm, uint64(cc.ConvertInt64(x.Value, op.Type, tu.Model)))
			default:
				g.w("\n%sDD%s = \"%v\"\n", lConstPrefix, nm, x.Value)
			}
		case *ir.StringValue:
			g.w("\n%sDD%s = %q\n", lConstPrefix, nm, fmt.Sprintf("%q", dict.S(int(x.StringID))))
		default:
			panic(fmt.Errorf("%T", x))
		}
	}
}

func (g *gen) position(n cc.Node) token.Position {
	return g.in.FileSet.PositionFor(n.Pos(), true)
}

func (g *gen) tpos(t cc.Type) (r token.Position) {
	if n, ok := t.(cc.Node); ok && n.Pos() != 0 {
		r = g.position(n)
	}
	return r
}

func (g *gen) w(s string, args ...interface{}) {
	if _, err := fmt.Fprintf(&g.out0, s, args...); err != nil {
		todo("", err)
	}

	if traceWrites {
		fmt.Fprintf(os.Stderr, s, args...)
	}
}

func (g *gen) wPreamble(s string, args ...interface{}) {
	if _, err := fmt.Fprintf(&g.tldPreamble, s, args...); err != nil {
		todo("", err)
	}
}

func (g gen) escaped(n *cc.Declarator) bool {
	if n == nil {
		return false
	}

	if n.IsTLD() || n.DeclarationSpecifier.IsStatic() {
		return false
	}

	if n.AddressTaken {
		return true
	}

	switch underlyingType(n.Type, false).(type) {
	case *cc.ArrayType:
		return !n.IsFunctionParameter
	default:
		return false
	}
}

func (g *gen) shiftMod(t cc.Type) int {
	if g.model.Sizeof(t) > 4 {
		return 64
	}

	return 32
}

func (g *gen) registerHelper(a ...interface{}) string {
	b := make([]string, len(a))
	for i, v := range a {
		b[i] = fmt.Sprint(v)
	}
	k := strings.Join(b, "$")
	if id := g.helpers[k]; id != 0 {
		return fmt.Sprintf(b[0], id)
	}

	id := len(g.helpers) + 1
	g.helpers[k] = id
	g.wPreamble("\n\nconst Lh"+b[0]+" = %q\n", id, k)
	return fmt.Sprintf(b[0], id)
}
