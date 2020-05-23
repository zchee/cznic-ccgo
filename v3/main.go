// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate stringer -output stringer.go -type=exprMode

package main // import "modernc.org/ccgo/v3"

import (
	"bufio"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strconv"
	"strings"

	"golang.org/x/tools/go/packages"
	"modernc.org/cc/v3"
	"modernc.org/opt"
)

const (
	builtin = `
#undef __GNUC__
typedef void *__builtin_va_list;
`
	defaultCrt = "modernc.org/crt/v3"
)

func todo(s string, args ...interface{}) string { //TODO-
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	pc, fn, fl, _ := runtime.Caller(1)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
	}
	r := fmt.Sprintf("%s:%d:%s: TODOTODO %s", fn, fl, fns, s) //TODOOK
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

func trc(s string, args ...interface{}) string { //TODO-
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	_, fn, fl, _ := runtime.Caller(1)
	r := fmt.Sprintf("\n%s:%d: TRC %s", fn, fl, s)
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

func main() {
	if err := newTask(os.Args, os.Stdout, os.Stderr).main(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

type imported struct {
	path      string              // Eg. "example.com/user/foo".
	name      string              // Eg. "foo" from "package foo".
	qualifier string              // Eg. "foo." or "foo2." if renamed due to name conflict.
	exports   map[string]struct{} // Eg. {"New": {}, "Close": {}, ...}.

	used bool
}

type task struct {
	D               []string // -D
	I               []string // -I
	U               []string // -U
	args            []string
	asts            []*cc.AST
	cfg             *cc.Config
	crt             string
	crtImportPath   string // -ccgo-crt-import-path
	goarch          string
	goos            string
	ignoredIncludes string // -ccgo-ignored-includes
	imports         []*imported
	l               []string // -l
	o               string   // -o
	out             io.Writer
	pkgName         string // -ccgo-pkgname
	sources         []cc.Source
	stderr          io.Writer
	stdout          io.Writer

	E bool // -E
}

func newTask(args []string, stdout, stderr io.Writer) *task {
	if stdout == nil {
		stdout = os.Stdout
	}
	if stderr == nil {
		stderr = os.Stderr
	}
	return &task{
		args:          args,
		crt:           "crt.",
		crtImportPath: defaultCrt,
		goarch:        env("GOARCH", runtime.GOARCH),
		goos:          env("GOOS", runtime.GOOS),
		pkgName:       "main",
		stderr:        stderr,
		stdout:        stdout,
	}
}

func env(name, deflt string) (r string) {
	r = deflt
	if s := os.Getenv(name); s != "" {
		r = s
	}
	return r
}

func (t *task) main() (err error) {
	opts := opt.NewSet()
	opts.Arg("D", true, func(arg, value string) error { t.D = append(t.D, value); return nil })
	opts.Arg("I", true, func(opt, arg string) error { t.I = append(t.I, arg); return nil })
	opts.Arg("U", true, func(arg, value string) error { t.U = append(t.U, value); return nil })
	opts.Arg("ccgo-crt-import-path", false, func(arg, value string) error { t.crtImportPath = value; return nil })
	opts.Arg("ccgo-pkgname", false, func(arg, value string) error { t.pkgName = value; return nil })
	opts.Arg("ccgo-ignored-includes", false, func(arg, value string) error { t.ignoredIncludes = value; return nil })
	opts.Opt("E", func(opt string) error { t.E = true; return nil })
	opts.Arg("l", true, func(arg, value string) error {
		value = strings.TrimSpace(value)
		a := strings.Split(value, ",")
		t.l = append(t.l, a...)
		return nil
	})
	opts.Arg("o", false, func(arg, value string) error {
		if t.o != "" {
			return fmt.Errorf("multiple argument: -o %s", value)
		}

		t.o = value
		return nil
	})
	if err := opts.Parse(t.args[1:], func(arg string) error {
		if strings.HasPrefix(arg, "-") {
			return fmt.Errorf("unexpected option: %s", arg)
		}

		switch filepath.Ext(arg) {
		case ".c", ".h":
			t.sources = append(t.sources, cc.Source{Name: arg})
		default:
			return fmt.Errorf("unexpected file type: %s", arg)
		}

		return nil
	}); err != nil {
		return err
	}
	if len(t.sources) == 0 {
		return fmt.Errorf("no input files specified")
	}

	if t.crtImportPath == "" {
		return fmt.Errorf("empty crt import path")
	}

	t.l = append(t.l, t.crtImportPath)
	m := map[string]struct{}{}
	for _, v := range t.l {
		v = strings.TrimSpace(v)
		if _, ok := m[v]; !ok {
			t.imports = append(t.imports, &imported{path: v})
			m[v] = struct{}{}
		}
	}
	t.imports[len(t.imports)-1].used = true // crt is always imported
	abi, err := cc.NewABIFromEnv()
	if err != nil {
		return err
	}

	var re *regexp.Regexp
	if t.ignoredIncludes != "" {
		if re, err = regexp.Compile(t.ignoredIncludes); err != nil {
			return err
		}
	}

	t.cfg = &cc.Config{
		ABI:     abi,
		Config3: cc.Config3{PreserveWhiteSpace: true, IgnoreInclude: re},
	}
	hostPredefined, hostIncludes, hostSysIncludes, err := cc.HostConfig("")
	if err != nil {
		return err
	}
	var sources []cc.Source
	if hostPredefined != "" {
		sources = append(sources, cc.Source{Name: "<predefined>", Value: hostPredefined})
	}
	sources = append(sources, cc.Source{Name: "<builtin>", Value: builtin})
	if len(t.D) != 0 {
		var a []string
		for _, v := range t.D {
			if i := strings.IndexByte(v, '='); i > 0 {
				a = append(a, fmt.Sprintf("#define %s %s", v[:i], v[i+1:]))
				continue
			}

			a = append(a, fmt.Sprintf("#define %s 1", v))
		}
		a = append(a, "\n")
		sources = append(sources, cc.Source{Name: "<defines>", Value: strings.Join(a, "\n")})
	}
	if len(t.U) != 0 {
		var a []string
		for _, v := range t.U {
			a = append(a, fmt.Sprintf("#undef %s", v))
		}
		a = append(a, "\n")
		sources = append(sources, cc.Source{Name: "<undefines>", Value: strings.Join(a, "\n")})
	}

	// https://pubs.opengroup.org/onlinepubs/9699919799/utilities/c99.html
	//
	// Headers whose names are enclosed in double-quotes ( "" ) shall be
	// searched for first in the directory of the file with the #include
	// line, then in directories named in -I options, and last in the usual
	// places
	includePaths := append([]string{"@"}, t.I...)
	includePaths = append(includePaths, hostIncludes...)
	includePaths = append(includePaths, hostSysIncludes...)
	includePaths = append(includePaths, filepath.FromSlash("/usr/include")) //TODO nix only
	// For headers whose names are enclosed in angle brackets ( "<>" ), the
	// header shall be searched for only in directories named in -I options
	// and then in the usual places.
	sysIncludePaths := append(t.I, hostSysIncludes...)
	for _, v := range t.sources {
		tuSources := append(sources, v)
		if t.E {
			t.cfg.PreprocessOnly = true
			if err := cc.Preprocess(t.cfg, includePaths, sysIncludePaths, tuSources, os.Stdout); err != nil {
				return err
			}
			continue
		}

		ast, err := cc.Translate(t.cfg, includePaths, sysIncludePaths, tuSources)
		if err != nil {
			return err
		}

		t.asts = append(t.asts, ast)
	}
	if t.E {
		return nil
	}

	if t.o == "" {
		t.o = fmt.Sprintf("a_%s_%s.go", t.goos, t.goarch)
	}
	f, err2 := os.Create(t.o)
	if err2 != nil {
		return err2
	}

	defer func() {
		if e := f.Close(); e != nil && err == nil {
			err = e
			return
		}

		if out, e := exec.Command("gofmt", "-l", "-s", "-w", t.o).CombinedOutput(); e != nil && err == nil {
			err = fmt.Errorf(strings.Join([]string{string(out), e.Error()}, ": "))
		}
	}()

	w := bufio.NewWriter(f)

	defer func() {
		if e := w.Flush(); e != nil && err == nil {
			err = e
		}
	}()

	t.out = w
	p, err := newProject(t)
	if err != nil {
		return err
	}

	return p.main()
}

// Get exported symbols from package having import path 'path'.
func (t *task) capi(path string) (pkgName string, exports map[string]struct{}, err error) {
	defer func() {
		if err != nil {
			err = fmt.Errorf("loading C exports from %s: %v", path, err)
		}
	}()

	exports = map[string]struct{}{}
	pkgs, err := packages.Load(
		&packages.Config{
			Mode: packages.NeedFiles,
			Env:  append(os.Environ(), fmt.Sprintf("GOOS=%s", t.goos), fmt.Sprintf("GOARCH=%s", t.goarch)),
		},
		path,
	)
	if err != nil {
		return "", nil, err
	}

	if len(pkgs) != 1 {
		return "", nil, fmt.Errorf("expected one package, loaded %d", len(pkgs))
	}

	pkg := pkgs[0]
	if len(pkg.Errors) != 0 {
		var a []string
		for _, v := range pkg.Errors {
			a = append(a, v.Error())
		}
		return "", nil, fmt.Errorf("%s", strings.Join(a, "\n"))
	}
	base := fmt.Sprintf("capi_%s_%s.go", t.goos, t.goarch)
	var fn string
	for _, v := range pkg.GoFiles {
		if filepath.Base(v) == base {
			fn = v
			break
		}
	}
	if fn == "" {
		return "", nil, fmt.Errorf("file %s not found", base)
	}

	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, fn, nil, 0)
	if err != nil {
		return "", nil, err
	}

	obj, ok := file.Scope.Objects["CAPI"]
	if !ok {
		return "", nil, fmt.Errorf("CAPI not declared in %s", fn)
	}

	switch obj.Kind {
	case ast.Var:
		// ok
	default:
		return "", nil, fmt.Errorf("unexpected CAPI object kind: %v", obj.Kind)
	}

	spec, ok := obj.Decl.(*ast.ValueSpec)
	if !ok {
		return "", nil, fmt.Errorf("unexpected CAPI object type: %T", obj.Decl)
	}

	if len(spec.Values) != 1 {
		return "", nil, fmt.Errorf("expected one CAPI expression, got %v", len(spec.Values))
	}

	ast.Inspect(spec.Values[0], func(n ast.Node) bool {
		if x, ok := n.(*ast.BasicLit); ok {
			var key string
			if key, err = strconv.Unquote(x.Value); err != nil {
				err = fmt.Errorf("invalid CAPI key value: %s", x.Value)
				return false
			}

			exports[key] = struct{}{}
		}
		return true
	})
	return file.Name.String(), exports, err
}
