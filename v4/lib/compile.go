// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"bufio"
	"bytes"
	"fmt"
	"go/token"
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
	objectFileCommentPrefix = "Code generated for "
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

func (b *buf) bytes() []byte                   { return (*bytes.Buffer)(b).Bytes() }
func (b *buf) w(s string, args ...interface{}) { fmt.Fprintf((*bytes.Buffer)(b), s, args...) }

func tag(nm name) string { return tags[nm] }

// errHandler is a function called on error.
type errHandler func(msg string, args ...interface{})

type ctx struct {
	ast           *cc.AST
	cfg           *cc.Config
	eh            errHandler
	enumerators   nameSet
	ft            *cc.FunctionType
	ifn           string
	imports       map[string]string // import path: qualifier
	out           io.Writer
	taggedEnums   nameSet
	taggedStructs nameSet
	taggedUnions  nameSet
	task          *Task
	typenames     nameSet

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

func (c *ctx) err(err error) { c.eh("%s", err.Error()) }

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
			b = append(b, fmt.Sprintf("\n%s%s = %v // %v:", tag(macro), m.Name.Src(), x, c.pos(m.Name)))
		case cc.UInt64Value:
			b = append(b, fmt.Sprintf("\n%s%s = %v // %v:", tag(macro), m.Name.Src(), x, c.pos(m.Name)))
		case cc.Float64Value:
			if s := fmt.Sprint(x); s == r {
				b = append(b, fmt.Sprintf("\n%s%s = %s // %v:", tag(macro), m.Name.Src(), s, c.pos(m.Name)))
			}
		case cc.StringValue:
			b = append(b, fmt.Sprintf("\n%s%s = %q // %v:", tag(macro), m.Name.Src(), x[:len(x)-1], c.pos(m.Name)))
		}

		w.w("\n%s%s = %q // %v:", tag(define), m.Name.Src(), r, c.pos(m.Name))
	}
	w.w("\n)")
	if len(b) == 0 {
		return
	}

	w.w("\n\nconst (\n%s\n)", strings.Join(b, "\n"))
}

func (c *ctx) pos(n cc.Node) (r token.Position) {
	if n != nil {
		r = token.Position(n.Position())
		if !c.task.fullPaths {
			r.Filename = filepath.Base(r.Filename)
		}
	}
	return r
}

func (c *ctx) prologue(w writer) {
	w.w(`// %s%s/%s by '%s %s', DO NOT EDIT.

//go:build ignore
// +build ignore

package %s
`,
		objectFileCommentPrefix,
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

func (c *ctx) linkageTag(d *cc.Declarator) string {
	switch d.Linkage() {
	case cc.External:
		return tag(external)
	case cc.Internal:
		return tag(internal)
	case cc.None:
		return tag(none)
	default:
		c.err(errorf("%v: internal error: %v", d.Position(), d.Linkage()))
		return tag(none)
	}
}
