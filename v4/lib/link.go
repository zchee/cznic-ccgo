// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"bufio"
	"fmt"
	"go/ast"
	"go/parser"
	"go/printer"
	"go/token"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"

	"golang.org/x/mod/semver"
	"golang.org/x/tools/go/packages"
)

const (
	objectFile = iota
	objectPkg
)

type object struct {
	externs   nameSet
	fset      *token.FileSet
	id        string // file name or import path
	pkgName   string // for kind == objectPkg
	qualifier string

	kind int // {objectFile, objectPkg}
}

func newObject(kind int, id string) *object {
	return &object{
		kind: kind,
		id:   id,
	}
}

func (o *object) load(fset *token.FileSet) (file *ast.File, err error) {
	if o.kind == objectPkg {
		return nil, errorf("object.load: internal error: wrong kind")
	}

	o.fset = fset
	return parser.ParseFile(fset, o.id, nil, parser.DeclarationErrors|parser.ParseComments)
}

func (t *Task) link() (err error) {
	if len(t.inputFiles)+len(t.linkFiles) == 0 {
		return errorf("no input files")
	}

	defer func() {
		for _, v := range t.compiledfFiles {
			os.Remove(v)
		}
	}()

	if len(t.inputFiles) != 0 {
		if err := t.compile(""); err != nil {
			return err
		}
	}

	for i, v := range t.linkFiles {
		if x, ok := t.compiledfFiles[v]; ok {
			t.linkFiles[i] = x
		}
	}

	fset := token.NewFileSet()
	objects := map[string]*object{}
	mode := os.Getenv("GO111MODULE")
	for _, v := range t.linkFiles {
		var object *object
		switch {
		case strings.HasPrefix(v, "-l="):
			object, err = t.getPkgSymbols(v[len("-l="):], mode)
		default:
			object, err = t.getFileSymbols(fset, v)
		}
		if err != nil {
			return err
		}

		if _, ok := objects[v]; !ok {
			objects[v] = object
		}
	}
	fset = nil

	switch {
	case t.o == "":
		return errorf("TODO %v %v %v %v", t.args, t.inputFiles, t.compiledfFiles, t.linkFiles)
	case strings.HasSuffix(t.o, ".go"):
		l, err := newLinker(t)
		if err != nil {
			return err
		}

		return l.link(t.o, t.linkFiles, objects)
	default:
		return errorf("TODO %v %v %v %v", t.args, t.inputFiles, t.compiledfFiles, t.linkFiles)
	}
}

func (t *Task) getPkgSymbols(importPath, mode string) (r *object, err error) {
	switch mode {
	case "", "on":
		// ok
	default:
		return nil, errorf("GO111MODULE=%s not supported", mode)
	}

	pkgs, err := packages.Load(
		&packages.Config{
			Mode: packages.NeedFiles,
			Env:  append(os.Environ(), fmt.Sprintf("GOOS=%s", t.goos), fmt.Sprintf("GOARCH=%s", t.goarch)),
		},
		importPath,
	)
	if err != nil {
		return nil, err
	}

	if len(pkgs) != 1 {
		return nil, errorf("%s: expected one package, loaded %d", importPath, len(pkgs))
	}

	pkg := pkgs[0]
	if len(pkg.Errors) != 0 {
		var a []string
		for _, v := range pkg.Errors {
			a = append(a, v.Error())
		}
		return nil, errorf("%s", strings.Join(a, "\n"))
	}

	r = newObject(objectPkg, importPath)
	base := fmt.Sprintf("capi_%s_%s.go", t.goos, t.goarch)
	var fn string
	for _, v := range pkg.GoFiles {
		if filepath.Base(v) == base {
			fn = v
			break
		}
	}
	if fn == "" {
		return nil, errorf("%s: file %s not found", importPath, base)
	}

	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, fn, nil, 0)
	if err != nil {
		return nil, errorf("%s: %v", importPath, err)
	}

	obj, ok := file.Scope.Objects["CAPI"]
	if !ok {
		return nil, errorf("%s: CAPI not declared in %s", importPath, fn)
	}

	switch obj.Kind {
	case ast.Var:
		// ok
	default:
		return nil, errorf("%s: unexpected CAPI object kind: %v", importPath, obj.Kind)
	}

	spec, ok := obj.Decl.(*ast.ValueSpec)
	if !ok {
		return nil, errorf("%s: unexpected CAPI object type: %T", importPath, obj.Decl)
	}

	if len(spec.Values) != 1 {
		return nil, errorf("%s: expected one CAPI expression, got %v", importPath, len(spec.Values))
	}

	r.pkgName = file.Name.String()
	ast.Inspect(spec.Values[0], func(n ast.Node) bool {
		if x, ok := n.(*ast.BasicLit); ok {
			var key string
			if key, err = strconv.Unquote(x.Value); err != nil {
				err = errorf("%s: invalid CAPI key value: %s", importPath, x.Value)
				return false
			}

			r.externs.add(tag(external) + key)
		}
		return true
	})
	return r, err
}

func (t *Task) getFileSymbols(fset *token.FileSet, fn string) (r *object, err error) {
	file, err := parser.ParseFile(fset, fn, nil, parser.DeclarationErrors|parser.ParseComments)
	if err != nil {
		return nil, err
	}

	pkgName := file.Name.String()
	if !strings.HasPrefix(pkgName, objectFilePackageNamePrefix) {
		return nil, errorf("%s: package %s is not a ccgo object file", fn, pkgName)
	}

	version := pkgName[len(objectFilePackageNamePrefix):]
	if !semver.IsValid(version) {
		return nil, errorf("%s: package %s has invalid semantic version", fn, pkgName)
	}

	if semver.Compare(version, objectFileSemver) != 0 {
		return nil, errorf("%s: package %s has incompatible semantic version compared to %s", fn, pkgName, objectFileSemver)
	}

	for _, v := range file.Comments {
		for _, w := range v.List {
			if w.Slash > file.Package {
				break
			}

			line := w.Text
			x := strings.Index(line, generatedFilePrefix)
			if x < 0 {
				continue
			}

			s := line[x+len(generatedFilePrefix):]
			if len(s) == 0 {
				continue
			}

			if !strings.HasPrefix(s, fmt.Sprintf("%s/%s", t.goos, t.goarch)) {
				return nil, errorf("%s: object file was compiled for different target: %s", fn, line)
			}
		}
	}

	r = newObject(objectFile, fn)
	x := tag(external)
	for k, v := range file.Scope.Objects {
		if strings.HasPrefix(k, x) {
			if _, ok := r.externs[k]; ok {
				return nil, errorf("invalid object file: multiple defintions of %s", k[len(x):])
			}

			switch v.Kind {
			case ast.Var, ast.Fun:
				r.externs.add(k)
			default:
				return nil, errorf("invalid object file: symbol %s of kind %v", k[len(x):], v.Kind)
			}
		}
	}
	return r, nil
}

type linker struct {
	errors  errors
	externs map[string]*object
	fset    *token.FileSet
	goTags  []string
	imports []*object
	out     io.Writer
	task    *Task
	tld     nameSpace
	tsName  string

	closed bool
}

func newLinker(task *Task) (*linker, error) {
	goTags := tags
	for i := range tags {
		switch name(i) {
		case ccgo, noneStatic:
			goTags[i] = task.prefixCcgo
		case define:
			goTags[i] = task.prefixDefine
		case enumConst:
			goTags[i] = task.prefixEnumerator
		case external:
			goTags[i] = task.prefixExternal
		case importQualifier:
			goTags[i] = task.prefixImportQualifier
		case internal:
			goTags[i] = task.prefixInternal
		case macro:
			goTags[i] = task.prefixMacro
		case none:
			goTags[i] = task.prefixNone
		case preserve:
			goTags[i] = ""
		case taggedEum:
			goTags[i] = task.prefixEnum
		case taggedStruct:
			goTags[i] = task.prefixStruct
		case taggedUnion:
			goTags[i] = task.prefixUnion
		case typename:
			goTags[i] = task.prefixTypename
		case unpinned:
			goTags[i] = task.prefixUnpinned
		default:
			return nil, errorf("internal error: %v", name(i))
		}
	}
	return &linker{
		externs: map[string]*object{},
		fset:    token.NewFileSet(),
		goTags:  goTags[:],
		task:    task,
	}, nil
}

func (l *linker) err(err error)                      { l.errors.add(err) }
func (l *linker) rawName(linkName string) (r string) { return linkName[len(tag(symKind(linkName))):] }

func (l *linker) goName(linkName string) (r string) {
	return l.goTags[symKind(linkName)] + l.rawName(linkName)
}

func (l *linker) w(s string, args ...interface{}) {
	if l.closed {
		return
	}

	if _, err := fmt.Fprintf(l.out, s, args...); err != nil {
		l.err(err)
		l.closed = true
	}
}

func (l *linker) link(ofn string, linkFiles []string, objects map[string]*object) error {
	var tld nameSet
	// Build the symbol table.
	for _, linkFile := range linkFiles {
		object := objects[linkFile]
		for nm := range object.externs {
			if _, ok := l.externs[nm]; !ok {
				l.externs[nm] = object
				trc("extern %s is defined in %s", nm, object.id)
			}
			tld.add(nm)
			//TODO other symbol kinds
		}
	}
	l.tld.registerSet(l, tld, true)
	l.tsName = l.tld.reg.put("ts")

	// Check for unresolved references.
	for _, linkFile := range linkFiles {
		if object := objects[linkFile]; object.kind == objectFile {
			file, err := object.load(l.fset)
			if err != nil {
				return errorf("loading %s: %v", object.id, err)
			}

			for _, ident := range file.Unresolved {
				nm := ident.Name
				if !strings.HasPrefix(nm, tag(external)) {
					continue
				}

				lib, ok := l.externs[nm]
				if !ok {
					return errorf("%v: undefined reference to '%s'", linkFile, l.rawName(nm))
				}

				if lib.qualifier == "" {
					lib.qualifier = l.tld.registerName(l, tag(importQualifier)+lib.pkgName)
					l.imports = append(l.imports, lib)
				}
			}
		}
	}

	f, err := os.Create(ofn)
	if err != nil {
		return errorf("%s", err)
	}

	defer func() {
		if err := f.Close(); err != nil {
			l.err(errorf("%s", err))
		}

		if err := exec.Command("gofmt", "-w", ofn).Run(); err != nil {
			l.err(errorf("%s: gofmt: %v", ofn, err))
		}
	}()

	out := bufio.NewWriter(f)
	l.out = out

	defer func() {
		if err := out.Flush(); err != nil {
			l.err(errorf("%s", err))
		}
	}()

	nm := l.task.packageName
	if nm == "" {
		nm = "main"
	}
	l.prologue(nm)
	if len(l.imports) != 0 {
		l.w("\n\nimport (")
		for _, v := range l.imports {
			l.w("\n\t")
			if v.pkgName != v.qualifier {
				l.w("%s ", v.qualifier)
			}
			l.w("%q", v.id)
		}
		l.w("\n)\n\n")
	}

	for _, linkFile := range linkFiles {
		object := objects[linkFile]
		if object.kind != objectFile {
			continue
		}

		file, err := object.load(l.fset)
		if err != nil {
			return errorf("loading %s: %v", object.id, err)
		}

		for _, decl := range file.Decls {
			if err := l.decl(file, object, decl); err != nil {
				return err
			}
		}
	}
	return l.errors.err()
}

func (l *linker) prologue(nm string) {
	l.w(`// %s%s/%s by '%s %s'%s.

//go:build %[2]s && %[3]s
// +build %[2]s,%[3]s

package %[7]s
`,
		generatedFilePrefix,
		l.task.goos, l.task.goarch,
		filepath.Base(l.task.args[0]),
		strings.Join(l.task.args[1:], " "),
		generatedFileSuffix,
		nm,
	)
}

func (l *linker) decl(file *ast.File, object *object, n ast.Decl) error {
	switch x := n.(type) {
	case *ast.GenDecl:
		return l.genDecl(x)
	case *ast.FuncDecl:
		return l.funcDecl(file, object, x)
	default:
		return errorf("TODO %T", x)
	}
}

func (l *linker) funcDecl(file *ast.File, object *object, n *ast.FuncDecl) error {
	l.w("\n\n")
	info := l.newFnInfo(object, n)
	w := 0
	for _, stmt := range n.Body.List {
		if stmt := l.stmtPrune(stmt, info); stmt != nil {
			n.Body.List[w] = stmt
			w++
		}
	}
	n.Body.List = n.Body.List[:w]
	ast.Walk(&renamer{info}, n)
	return printer.Fprint(l.out, l.fset, n)
}

var _ ast.Visitor = (*renamer)(nil)

type renamer struct {
	info *fnInfo
}

func (r *renamer) Visit(n ast.Node) ast.Visitor {
	if x, ok := n.(*ast.Ident); ok {
		if nm := r.info.name(x.Name); nm != "" {
			if symKind(x.Name) == external {
				obj := r.info.linker.externs[x.Name]
				if obj.kind == objectPkg {
					x.Name = fmt.Sprintf("%s.%s", obj.qualifier, nm)
					return r
				}
			}

			x.Name = nm
		}
	}
	return r
}

func (l *linker) stmtPrune(n ast.Stmt, info *fnInfo) ast.Stmt {
	switch x := n.(type) {
	case *ast.AssignStmt:
		for _, expr := range x.Lhs {
			switch y := expr.(type) {
			case *ast.Ident:
				if c, ok := info.mentions[y.Name]; ok && c == 1 {
					if len(x.Lhs) == 1 && l.goName(y.Name) == "__func__" {
						panic(todo(""))
						return nil
					}

					y.Name = "_"
					continue
				}
			default:
				l.err(errorf("TODO %T", y))
			}
		}
	case *ast.DeclStmt:
		gd := x.Decl.(*ast.GenDecl)
		if gd.Tok != token.VAR {
			return n
		}

		w := 0
		for _, spec := range gd.Specs {
			vs := spec.(*ast.ValueSpec)
			w2 := 0
			for i, name := range vs.Names {
				if c, ok := info.mentions[name.Name]; ok && c == 1 {
					if len(vs.Names) == 1 {
						break
					}

					name.Name = "_"
				}
				vs.Names[w2] = name
				if vs.Values != nil {
					vs.Values[w2] = vs.Values[i]
				}
				w2++
			}
			if w2 == 0 {
				continue
			}

			vs.Names = vs.Names[:w2]
			if vs.Values != nil {
				vs.Values = vs.Values[:w2]
			}
			gd.Specs[w] = spec
			w++
		}
		if w == 0 {
			return nil
		}

		gd.Specs = gd.Specs[:w]
	}
	return n
}

var _ ast.Visitor = (*fnInfo)(nil)

type fnInfo struct {
	ns        nameSpace
	linkNames nameSet
	linker    *linker
	mentions  map[string]int // key is link name
	object    *object
}

func (l *linker) newFnInfo(object *object, n *ast.FuncDecl) (r *fnInfo) {
	r = &fnInfo{linker: l, object: object}
	ast.Walk(r, n)
	var a []string
	for k := range r.linkNames {
		a = append(a, k)
	}
	sort.Slice(a, func(i, j int) bool {
		if symRank(a[i]) < symRank(a[j]) {
			return true
		}

		return a[i] < a[j]
	})
	r.ns.registerSet(l, r.linkNames, false)
	r.linkNames = nil
	return r
}

func (fi *fnInfo) name(linkName string) string {
	switch symKind(linkName) {
	case external:
		if goName := fi.linker.tld.dict[linkName]; goName != "" {
			return goName
		}
	default:
		if goName := fi.ns.dict[linkName]; goName != "" {
			return goName
		}
	}
	return linkName
}

func (fi *fnInfo) Visit(n ast.Node) ast.Visitor {
	switch x := n.(type) {
	case *ast.Ident:
		fi.linkNames.add(x.Name)
	}
	return fi
}

func (l *linker) genDecl(n *ast.GenDecl) error {
	switch n.Tok {
	case token.VAR:
		for _, spec := range n.Specs {
			if err := l.varSpec(spec.(*ast.ValueSpec)); err != nil {
				return err
			}
		}
	}
	return nil
}

func (l *linker) varSpec(n *ast.ValueSpec) error {
	return errorf("TODO %v:", l.fset.Position(n.Pos()))
}
