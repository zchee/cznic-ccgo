// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
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
	externs map[string]struct{}
	id      string // file name or import path
	kind    int    // {objectFile, objectPkg}
	pkgName string // for kind == objectPkg
}

func newObject(kind int, id string) *object {
	return &object{
		externs: map[string]struct{}{},
		kind:    kind,
		id:      id,
	}
}

func (t *Task) link() (err error) {
	if len(t.inputFiles)+len(t.linkFiles) == 0 {
		return errorf("no input files")
	}

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
	return errorf("TODO %v %v %v", t.args, t.inputFiles, t.linkFiles)
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

			r.externs[key] = struct{}{}
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
			x := strings.Index(line, objectFileCommentPrefix)
			if x < 0 {
				continue
			}

			s := line[x+len(objectFileCommentPrefix):]
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
				r.externs[k] = struct{}{}
			default:
				return nil, errorf("invalid object file: smybol %s of kind %v", k[len(x):], v.Kind)
			}
		}
	}
	return r, nil
}
