// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v2"

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"runtime"
	"runtime/debug"
	"strings"

	"modernc.org/cc/v2"
	"modernc.org/strutil"
	"modernc.org/xc"
)

const (
	ap   = "ap"
	crt  = "crt."
	null = "null"
)

var (
	allocaDeclarator = &cc.Declarator{}
	bNL              = []byte{'\n'}
	bPanic           = []byte("panic")
	dict             = xc.Dict

	idAlias                  = dict.SID("alias")
	idAligned                = dict.SID("aligned")
	idBuiltinAlloca          = dict.SID("__builtin_alloca")
	idBuiltinTypesCompatible = dict.SID("__builtin_types_compatible__") // Implements __builtin_types_compatible_p
	idBuiltinVaList          = dict.SID("__builtin_va_list")
	idConst                  = dict.SID("const")
	idFuncName               = dict.SID("__func__")
	idGo                     = dict.SID("__GO__")
	idGo2                    = dict.SID("__GO2__")
	idLS                     = dict.SID("LS")
	idMain                   = dict.SID("main")
	idNoClone                = dict.SID("noclone")
	idNoInline               = dict.SID("noinline")
	idNoInline2              = dict.SID("__noinline__")
	idNoReturn               = dict.SID("noreturn")
	idNoReturn2              = dict.SID("__noreturn__")
	idPacked                 = dict.SID("packed")
	idPacked2                = dict.SID("__packed__")
	idPure                   = dict.SID("pure")
	idStdcall                = dict.SID("stdcall")
	idVaList                 = dict.SID("va_list")
	idVisibility             = dict.SID("visibility")
	idVisibility2            = dict.SID("__visibility__")
	idWcharT                 = dict.SID("wchar_t")
	idWeak                   = dict.SID("weak")
	idWeak2                  = dict.SID("__weak__")

	mangles = map[int]string{
		dict.SID("__builtin_cimag"):    "imag",
		dict.SID("__builtin_cimagf"):   "imag",
		dict.SID("__builtin_cimagl"):   "imag",
		dict.SID("__builtin_complex"):  "complex",
		dict.SID("__builtin_complexf"): "complex",
		dict.SID("__builtin_complexl"): "complex",
	}

	testFn      string
	traceOpt    bool
	traceTODO   bool
	traceWrites bool
)

func pretty(v interface{}) string { return strutil.PrettyString(v, "", "", nil) }

func debugStack0() []byte { return debug.Stack() }

func debugStack() []byte {
	b := debug.Stack()
	b = b[bytes.Index(b, bPanic)+1:]
	b = b[bytes.Index(b, bPanic):]
	b = b[bytes.Index(b, bNL)+1:]
	return b
}

func debugStack2() []byte {
	b := debug.Stack()
	b = b[bytes.Index(b, bPanic)+1:]
	b = b[bytes.Index(b, bPanic):]
	b = b[bytes.Index(b, bNL)+1:]
	a := bytes.SplitN(b, bNL, 3)
	if len(a) > 2 {
		a = a[:2]
	}
	if len(a) > 1 {
		a = a[1:]
	}
	return bytes.Join(a, bNL)
}

func isSingleExpression(n *cc.ExprList) bool { return n.ExprList == nil }

func mangleIdent(nm int, exported bool) string {
	switch {
	case exported:
		return fmt.Sprintf("X%s", dict.S(nm))
	default:
		return fmt.Sprintf("_%s", dict.S(nm))
	}
}

func mangleLabel(nm int) string {
	return fmt.Sprintf("l%s", dict.S(nm))
}

func roundup(n, to int64) int64 {
	if r := n % to; r != 0 {
		return n + to - r
	}

	return n
}

func strComment(s []byte) string {
	const max = 16
	if len(s) > max {
		s = append(append([]byte(nil), s[:max]...), []byte("...")...)
	}
	s = bytes.Replace(s, []byte("*/"), []byte(`*\x2f`), -1)
	return fmt.Sprintf("/* %q */", s)
}

func todo(msg string, args ...interface{}) {
	_, f, l, _ := runtime.Caller(1)
	s := msg
	if s == "" {
		s = strings.Repeat("%v ", len(args))
	}
	log("%v:%d: TODO: %s", f, l, fmt.Sprintf(s, args...)) //TODOOK
	if traceTODO {
		fmt.Fprintf(os.Stderr, "\n\n%v:%d: TODO\n\n%s\n", f, l, fmt.Sprintf(s, args...)) //TODOOK
		os.Exit(1)
	}
	panic(fmt.Errorf("\n\n%v:%d: TODO\n\n%s", f, l, fmt.Sprintf(s, args...))) //TODOOK
}

func isFnPtr(t cc.Type, out *cc.Type) bool {
	switch x := cc.UnderlyingType(t).(type) {
	case *cc.FunctionType:
		*out = x
		return true
	case *cc.PointerType:
		if x.Item.Kind() == cc.Function {
			if out != nil {
				*out = x.Item
			}
			return true
		}
	}
	return false
}

func (g *gen) typeComment(t cc.Type) (r string) {
	const max = 64
	defer func() {
		r = strings.Replace(r, "\n", "", -1)
		if len(r) > max+3 {
			r = r[:max/2] + "..." + r[len(r)-max/2:]
		}
	}()

	switch x := t.(type) {
	case *cc.NamedType:
		return fmt.Sprintf("T%s = %s", dict.S(x.Name), g.typeComment(x.Type))
	case *cc.PointerType:
		n := 1
		for {
			t, ok := underlyingType(x.Item, true).(*cc.PointerType)
			if !ok {
				switch {
				case x.Item == cc.Void:
					return fmt.Sprintf("%svoid", strings.Repeat("*", n))
				default:
					return fmt.Sprintf("%s%s", strings.Repeat("*", n), g.typeComment(x.Item))
				}
			}

			x = t
			n++
		}
	case *cc.ArrayType:
		if x.IsVLA() {
			return fmt.Sprintf("*%s", g.typeComment(x.Item))
		}

		return g.ptyp(t, false, 1)
	default:
		return g.ptyp(t, false, 1)
	}
}

func cpFile(dst, src string, buf []byte) (err error) {
	var d, s *os.File
	if s, err = os.Open(src); err != nil {
		return err
	}

	defer func() {
		if e := s.Close(); e != nil && err == nil {
			err = e
		}
	}()

	if d, err = os.Create(dst); err != nil {
		return err
	}

	defer func() {
		if e := d.Close(); e != nil && err == nil {
			err = e
		}
	}()

	_, err = io.CopyBuffer(d, s, buf)
	return err
}

func fixMain(n *cc.Declarator) bool {
	if n.Name() == idMain && n.Linkage == cc.LinkageExternal {
		n.Type = &cc.FunctionType{
			Params: []cc.Type{
				cc.Int,
				&cc.PointerType{Item: &cc.PointerType{Item: cc.Char}},
			},
			Result: cc.Int,
		}
		return true
	}

	return false
}

type lineScanner struct {
	err  error
	line []byte
	r    *bufio.Reader
}

func newLineScanner(r io.Reader) *lineScanner { return &lineScanner{r: bufio.NewReader(r)} }

func (s *lineScanner) Err() error   { return s.err }
func (s *lineScanner) Text() string { return string(s.line) }

func (s *lineScanner) Scan() bool {
	s.line = s.line[:0]
	for {
		line, isPrefix, err := s.r.ReadLine()
		if err != nil {
			if err != io.EOF {
				s.err = err
			}
			return false
		}

		s.line = append(s.line, line...)
		if !isPrefix {
			return true
		}
	}
}

func isVLA(d *cc.Declarator) bool { return isVLAType(d.Type) }

func isVLAType(t cc.Type) bool {
	x, ok := underlyingType(t, false).(*cc.ArrayType)
	return ok && x.IsVLA()
}
