// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sort"
	"strings"

	"golang.org/x/mod/semver"
	"modernc.org/cc/v4"
	"modernc.org/sortutil"
)

type name int

const (
	//  package __ccgo_object_file_v1
	objectFilePackageName       = objectFilePackageNamePrefix + objectFileSemver
	objectFilePackageNamePrefix = "__ccgo_object_file_"
	objectFileSemver            = "v1"
)

const (
	define name = iota
	enumConst
	external
	imports
	internal
	macro
	none
	taggedEum
	taggedStruct
	taggedUnion
	typename
	unpinned
)

var (
	_ writer = (*buf)(nil)

	// Don't change the association once established, otherwise the major
	// objectFileSemver must be incremented.
	//
	// The concatenation of a tag and a valid C identifier must not create a Go
	// keyword neither it can be a prefix of a Go predefined identifier.
	tags = [...]string{
		define:       "df", // #define
		enumConst:    "ec", // enumerator constant
		external:     "X",  // external linkage
		imports:      "iq", // import qualifier
		internal:     "il", // internal linkage
		macro:        "mv", // macro value
		none:         "ln", // linkage none
		taggedEum:    "te", // tagged enum
		taggedStruct: "ts", // tagged struct
		taggedUnion:  "tu", // tagged union
		typename:     "tn", // type name
		unpinned:     "un", // unpinned
	}
)

func init() {
	if !semver.IsValid(objectFileSemver) {
		panic(todo("internal error: invalid objectFileSemver: %q", objectFileSemver))
	}
}

type writer interface {
	w(s string, args ...interface{})
}

type buf bytes.Buffer

func (b *buf) w(s string, args ...interface{}) { fmt.Fprintf((*bytes.Buffer)(b), s, args...) }

func tag(nm name) string { return tags[nm] }

// errHandler is a function called on error.
type errHandler func(msg string, args ...interface{})

type ctx struct {
	ast         *cc.AST
	cfg         *cc.Config
	eh          errHandler
	ifn         string
	imports     map[string]string // import path: qualifier
	out         io.Writer
	task        *Task
	taggedEnums nameSet
	typenames   nameSet

	closed bool
}

func newCtx(task *Task, eh errHandler) *ctx {
	return &ctx{
		cfg:     task.cfg,
		eh:      eh,
		imports: map[string]string{},
		task:    task,
	}
}

func (c *ctx) err(err error) { c.eh(err.Error()) }

func (c ctx) w(s string, args ...interface{}) {
	if c.closed {
		return
	}

	if _, err := fmt.Fprintf(c.out, s, args...); err != nil {
		c.err(err)
		c.closed = true
	}
}

func (c *ctx) compile(ifn, ofn string) error {
	f, err := os.Create(ofn)
	if err != nil {
		return err
	}

	defer func() {
		if err := f.Close(); err != nil {
			c.err(errorf("%v", err))
			return
		}

		if err := exec.Command("gofmt", "-w", "-r", "(x) -> x", ofn).Run(); err != nil {
			c.err(errorf("%s: gofmt: %v", ifn, err))
		}
	}()

	w := bufio.NewWriter(f)
	c.out = w

	defer func() {
		if err := w.Flush(); err != nil {
			c.err(errorf("%v", err))
		}
	}()

	sources := []cc.Source{
		{Name: "<predefined>", Value: c.cfg.Predefined},
		{Name: "<builtin>", Value: cc.Builtin},
	}
	if c.task.defs != "" {
		sources = append(sources, cc.Source{Name: "<command-line>", Value: c.task.defs})
	}
	sources = append(sources, cc.Source{Name: ifn, FS: c.cfg.FS})
	if c.ast, err = cc.Translate(c.cfg, sources); err != nil {
		return err
	}

	c.ifn = ifn
	c.prologue(c)
	c.defines(c)
	for n := c.ast.TranslationUnit; n != nil; n = n.TranslationUnit {
		c.externalDeclaration(c, n.ExternalDeclaration)
	}
	return nil
}

func (c *ctx) defines(w writer) {
	var a []*cc.Macro
	for _, v := range c.ast.Macros {
		if v.IsConst && len(v.ReplacementList()) == 1 {
			a = append(a, v)
		}
	}
	if len(a) == 0 {
		return
	}

	sort.Slice(a, func(i, j int) bool { return a[i].Name.SrcStr() < a[j].Name.SrcStr() })
	var b []string
	w.w("\n\nconst (")
	for _, m := range a {
		r := m.ReplacementList()[0].SrcStr()
		switch x := m.Value().(type) {
		case cc.Int64Value:
			b = append(b, fmt.Sprintf("\n%s%s = %v // %v:", tag(macro), m.Name.Src(), x, pos(m.Name)))
		case cc.UInt64Value:
			b = append(b, fmt.Sprintf("\n%s%s = %v // %v:", tag(macro), m.Name.Src(), x, pos(m.Name)))
		case cc.Float64Value:
			if s := fmt.Sprint(x); s == r {
				b = append(b, fmt.Sprintf("\n%s%s = %s // %v:", tag(macro), m.Name.Src(), s, pos(m.Name)))
			}
		case cc.StringValue:
			b = append(b, fmt.Sprintf("\n%s%s = %q // %v:", tag(macro), m.Name.Src(), x[:len(x)-1], pos(m.Name)))
		}

		w.w("\n%s%s = %q // %v:", tag(define), m.Name.Src(), r, pos(m.Name))
	}
	w.w("\n)")
	if len(b) == 0 {
		return
	}

	w.w("\n\nconst (\n%s\n)", strings.Join(b, "\n"))
}

func (c *ctx) prologue(w writer) {
	w.w(`// Code generated for %s/%s by '%s %s', DO NOT EDIT.

//go:build ignore
// +build ignore

package %s
`,
		c.task.goos, c.task.goarch,
		filepath.Base(c.task.args[0]),
		strings.Join(c.task.args[1:], " "),
		objectFilePackageName,
	)
	var a []string
	if len(a) == 0 {
		return
	}

	a = a[:sortutil.Dedupe(sort.StringSlice(a))]
	var ns nameSpace
	w.w("\nimport (")
	for _, v := range a {
		q := tag(imports) + ns.take(path.Base(v))
		c.imports[v] = q
		w.w("\t%s %q\n", q, v)
	}
	w.w("\n)")
}

func (c *ctx) externalDeclaration(w writer, n *cc.ExternalDeclaration) {
	return //TODO-
	switch n.Case {
	case cc.ExternalDeclarationFuncDef: // FunctionDefinition
		w.w("\n\n//TODO %T %v (%v: )\n// %s // %v:", n, n.Case, origin(1), cc.NodeSource(n.FunctionDefinition.DeclarationSpecifiers, n.FunctionDefinition.Declarator), pos(n))
		//TODO c.err(errorf("TODO %v", n.Case))
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
				c.defineTaggedEnum(w, x)
			default:
				trc("%v: %T", n.Position(), x)
				c.err(errorf("TODO %v %T", n.Case, x))
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

func (c *ctx) defineTaggedEnum(w writer, t *cc.EnumType) {
	nmt := t.Tag()
	if nm := nmt.SrcStr(); nm != "" {
		if !c.taggedEnums.add(nm) {
			return
		}

		w.w("\n\ntype %s%s = %s // %v:", tag(taggedEum), nm, c.typ(t.UnderlyingType()), pos(nmt))
	}
	w.w("\n\nconst (")
	for _, v := range t.Enumerators() {
		w.w("\n\t%s%s = %v // %v: ", tag(enumConst), v.Token.Src(), v.Value(), pos(v))
	}
	w.w("\n)")
}

func (c *ctx) initDeclarator(w writer, n *cc.InitDeclarator, external bool) {
	d := n.Declarator
	if n.Asm != nil {
		w.w("\n\n//TODO %T %v (%v: )\n// %s // %v:", n, n.Case, origin(1), cc.NodeSource(d), pos(n))
		c.err(errorf("TODO asm %v", n.Case))
		return //TODO-
	}

	nm := d.Name()
	switch n.Case {
	case cc.InitDeclaratorDecl: // Declarator Asm
		switch {
		case d.IsTypename():
			if external && c.typenames.add(nm) {
				w.w("\n\ntype %s%s = %s // %v:", tag(typename), nm, c.typedef(d.Type()), pos(d))
			}
		default:
			trc("", d.Position(), d.Name(), d.Type())
			c.err(errorf("TODO %v", n.Case))
		}
	case cc.InitDeclaratorInit: // Declarator Asm '=' Initializer
		trc("", d.Position(), d.Name(), d.Type())
		c.err(errorf("TODO %v", n.Case))
	default:
		c.err(errorf("internal error %T %v", n, n.Case))
	}
}

func (c *ctx) typedef(t cc.Type) string {
	var b strings.Builder
	c.typ0(&b, t, false)
	return b.String()
}

func (c *ctx) typ(t cc.Type) string {
	var b strings.Builder
	c.typ0(&b, t, true)
	return b.String()
}

func (c *ctx) typ0(b *strings.Builder, t cc.Type, useTypename bool) {
	if tn := t.Typedef(); tn != nil && useTypename && tn.LexicalScope().Parent == nil {
		fmt.Fprintf(b, "%s%s", tag(typename), tn.Name())
		return
	}

	switch x := t.(type) {
	case *cc.PointerType:
		b.WriteString("uintptr")
	case *cc.PredefinedType:
		switch {
		case cc.IsIntegerType(t):
			if t.Size() <= 8 {
				if !cc.IsSignedInteger(t) {
					b.WriteByte('u')
				}
				fmt.Fprintf(b, "int%d", 8*t.Size())
				break
			}

			b.WriteString("int")
			c.err(errorf("TODO %T", x))
		default:
			b.WriteString("int")
			c.err(errorf("TODO %T", x))
		}
	default:
		b.WriteString("int")
		c.err(errorf("TODO %T", x))
	}
}
