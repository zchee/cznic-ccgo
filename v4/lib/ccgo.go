// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package ccgo implements the ccgo command.
package ccgo // import "modernc.org/ccgo/v4/lib"

// -export-X, -unexport-X flags

import (
	"flag"
	"fmt"
	"io"
	"io/fs"
	"path/filepath"
	"strings"

	"modernc.org/cc/v4"
	"modernc.org/opt"
)

var (
	oTraceL = flag.Bool("trcl", false, "Print produced link files.")
	oTraceG = flag.Bool("trcg", false, "Print produced Gofiles.")
)

// Task represents a compilation job.
type Task struct {
	D              []string // -D
	I              []string // -I
	O              string   // -O
	U              []string // -U
	args           []string // command name in args[0]
	cfg            *cc.Config
	cfgArgs        []string
	compiledfFiles map[string]string // .c -> .go
	defs           string
	fs             fs.FS
	goarch         string
	goos           string
	inputFiles     []string
	l              []string // -l
	linkFiles      []string
	o              string // -o
	packageName    string // --package-name
	//TODO prefixUnpinned        string // --prefix-unpinned <string>
	prefixAutomatic       string // --prefix-automatic <string>
	prefixCcgoAutomatic   string
	prefixDefine          string // --prefix-define <string>
	prefixEnumerator      string // --prefix-enumerator <string>
	prefixExternal        string // --prefix-external <string>
	prefixField           string // --prefix-field <string>
	prefixImportQualifier string // --prefix-import-qualifier <string>
	prefixMacro           string // --prefix-macro <string>
	prefixStaticInternal  string // --prefix-static-internal <string>
	prefixStaticNone      string // --prefix-static-none <string>
	prefixTaggedEnum      string // --prefix-tagfed-enum <string>
	prefixTaggedStruct    string // --prefix-tagged-struct <string>
	prefixTypename        string // --prefix-typename <string>
	prefixTaggedUnion     string // --prefix-taged-union <string>
	std                   string // -std
	stderr                io.Writer
	stdout                io.Writer
	tlsQualifier          string

	E              bool // -E
	c              bool // -c
	fullPaths      bool // -full-paths
	nostdinc       bool // -nostdinc
	nostdlib       bool // -nostdlib
	packageNameSet bool
	pthread        bool // -pthread
}

// NewTask returns a newly created Task. args[0] is the command name.
func NewTask(goos, goarch string, args []string, stdout, stderr io.Writer, fs fs.FS) (r *Task) {
	return &Task{
		args:           args,
		compiledfFiles: map[string]string{},
		fs:             fs,
		goarch:         goarch,
		goos:           goos,
		//TODO prefixUnpinned:        "U",
		prefixAutomatic:       "",
		prefixCcgoAutomatic:   "",
		prefixDefine:          "D",
		prefixEnumerator:      "C",
		prefixExternal:        "X",
		prefixField:           "F",
		prefixImportQualifier: "",
		prefixMacro:           "D",
		prefixStaticInternal:  "s",
		prefixStaticNone:      "s",
		prefixTaggedEnum:      "T",
		prefixTaggedStruct:    "T",
		prefixTypename:        "T",
		prefixTaggedUnion:     "T",
		stderr:                stderr,
		stdout:                stdout,
		tlsQualifier:          "libc.",
	}
}

// Main executes task.
func (t *Task) Main() (err error) {
	switch len(t.args) {
	case 0:
		return errorf("invalid arguments")
	case 1:
		return errorf("no input files")
	}

	set := opt.NewSet()
	set.Arg("-package-name", false, func(opt, val string) error { t.packageName = val; t.packageNameSet = true; return nil })
	set.Arg("-prefix-automatic", false, func(opt, val string) error { t.prefixAutomatic = val; return nil })
	set.Arg("-prefix-define", false, func(opt, val string) error { t.prefixDefine = val; return nil })
	set.Arg("-prefix-enumerator", false, func(opt, val string) error { t.prefixEnumerator = val; return nil })
	set.Arg("-prefix-external", false, func(opt, val string) error { t.prefixExternal = val; return nil })
	set.Arg("-prefix-field", false, func(opt, val string) error { t.prefixField = val; return nil })
	set.Arg("-prefix-import-qualifier", false, func(opt, val string) error { t.prefixImportQualifier = val; return nil })
	set.Arg("-prefix-macro", false, func(opt, val string) error { t.prefixMacro = val; return nil })
	set.Arg("-prefix-static-none", false, func(opt, val string) error { t.prefixStaticNone = val; return nil })
	set.Arg("-prefix-tagged-enum", false, func(opt, val string) error { t.prefixTaggedEnum = val; return nil })
	set.Arg("-prefix-tagged-struct", false, func(opt, val string) error { t.prefixTaggedStruct = val; return nil })
	set.Arg("-prefix-tagged-union", false, func(opt, val string) error { t.prefixTaggedUnion = val; return nil })
	set.Arg("-prefix-typename", false, func(opt, val string) error { t.prefixTypename = val; return nil })
	//TODO set.Arg("-prefix-unpinned", false, func(opt, val string) error { t.prefixUnpinned = val; return nil })
	set.Arg("D", true, func(opt, val string) error { t.D = append(t.D, fmt.Sprintf("%s%s", opt, val)); return nil })
	set.Arg("I", true, func(opt, val string) error { t.I = append(t.I, val); return nil })
	set.Arg("O", true, func(opt, val string) error { t.O = fmt.Sprintf("%s%s", opt, val); return nil })
	set.Arg("U", true, func(opt, val string) error { t.U = append(t.U, fmt.Sprintf("%s%s", opt, val)); return nil })
	set.Arg("l", true, func(opt, val string) error {
		t.l = append(t.l, val)
		t.linkFiles = append(t.linkFiles, opt+"="+val)
		return nil
	})
	set.Arg("o", true, func(opt, val string) error { t.o = val; return nil })
	set.Arg("std", true, func(opt, val string) error { t.std = fmt.Sprintf("%s=%s", opt, val); return nil })
	set.Opt("E", func(opt string) error { t.E = true; return nil })
	set.Opt("c", func(opt string) error { t.c = true; return nil })
	set.Opt("extended-errors", func(opt string) error { extendedErrors = true; return nil })
	set.Opt("full-paths", func(opt string) error { t.fullPaths = true; return nil })
	set.Opt("nostdinc", func(opt string) error { t.nostdinc = true; return nil })
	set.Opt("nostdlib", func(opt string) error { t.nostdlib = true; return nil })
	set.Opt("pthread", func(opt string) error { t.pthread = true; t.cfgArgs = append(t.cfgArgs, opt); return nil })

	// Ignored
	set.Arg("MF", true, func(opt, val string) error { return nil })
	set.Arg("MQ", true, func(opt, val string) error { return nil })
	set.Arg("MT", true, func(opt, val string) error { return nil })
	set.Opt("M", func(opt string) error { return nil })
	set.Opt("MD", func(opt string) error { return nil })
	set.Opt("MM", func(opt string) error { return nil })
	set.Opt("MMD", func(opt string) error { return nil })
	set.Opt("MP", func(opt string) error { return nil })
	set.Opt("Qunused-arguments", func(opt string) error { return nil })
	set.Opt("S", func(opt string) error { return nil })
	set.Opt("dynamiclib", func(opt string) error { return nil })
	set.Opt("herror_on_warning", func(opt string) error { return nil })
	set.Opt("pedantic", func(opt string) error { return nil })
	set.Opt("pipe", func(opt string) error { return nil })
	set.Opt("s", func(opt string) error { return nil })
	set.Opt("shared", func(opt string) error { return nil })
	set.Opt("static", func(opt string) error { return nil })
	set.Opt("w", func(opt string) error { return nil })

	if err := set.Parse(t.args[1:], func(opt string) error {
		if strings.HasPrefix(opt, "-") {
			return errorf(" unrecognized command-line option '%s'", opt)
		}

		if strings.HasSuffix(opt, ".c") || strings.HasSuffix(opt, ".h") {
			t.inputFiles = append(t.inputFiles, opt)
			t.linkFiles = append(t.linkFiles, opt)
			return nil
		}

		if strings.HasSuffix(opt, ".go") {
			t.linkFiles = append(t.linkFiles, opt)
			return nil
		}

		return errorf("unexpected argument %s", opt)
	}); err != nil {
		return errorf("parsing %v: %v", t.args[1:], err)
	}

	t.cfgArgs = append(t.cfgArgs, t.D...)
	t.cfgArgs = append(t.cfgArgs, t.U...)
	t.cfgArgs = append(t.cfgArgs,
		t.O,
		t.std,
	)

	cfg, err := cc.NewConfig(t.goos, t.goarch, t.cfgArgs...)
	if err != nil {
		return err
	}

	t.I = t.I[:len(t.I):len(t.I)]
	cfg.IncludePaths = append([]string{""}, t.I...)
	cfg.IncludePaths = append(cfg.IncludePaths, cfg.HostIncludePaths...)
	cfg.IncludePaths = append(cfg.IncludePaths, cfg.HostSysIncludePaths...)
	cfg.SysIncludePaths = append(t.I, cfg.HostSysIncludePaths...)
	t.defs = buildDefs(t.D, t.U)
	cfg.FS = t.fs
	t.cfg = cfg
	if t.E {
		panic(todo(""))
	}

	if t.c {
		return t.compile(t.o)
	}

	switch {
	case t.nostdlib:
		t.tlsQualifier = ""
	default:
		t.linkFiles = append(t.linkFiles, "-l=modernc.org/libc")
	}
	return t.link()
}

// -c
func (t *Task) compile(optO string) error {
	switch len(t.inputFiles) {
	case 0:
		return errorf("no input files")
	case 1:
		// ok
	default:
		if t.o != "" && t.c {
			return errorf("cannot specify '-o' with '-c' with multiple files")
		}
	}

	p := newParallel()
	for _, ifn := range t.inputFiles {
		ifn := ifn
		ofn := optO
		if ofn == "" {
			ofn = filepath.Base(ifn) + ".go"
		}
		t.compiledfFiles[ifn] = ofn
		p.exec(func() error { return newCtx(t, p.eh).compile(ifn, ofn) })
	}
	return p.wait()
}
