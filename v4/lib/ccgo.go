// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Package ccgo implements the ccgo command.
package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"io"
	"io/fs"
	"path/filepath"
	"strings"

	"golang.org/x/mod/semver"
	"modernc.org/cc/v4"
	"modernc.org/opt"
)

const (
	builtin = `
#define __extension__
#define __restrict_arr restrict

#ifndef __builtin_va_list
#define __builtin_va_list __builtin_va_list
typedef void *__builtin_va_list;
#endif

#ifndef __builtin_va_arg
#define __builtin_va_arg __builtin_va_arg
#define __builtin_va_arg(va, type) (*(type*)__builtin_va_arg_impl(va))
#endif

#define __builtin_offsetof(type, member) ((size_t)&(((type*)0)->member))
#define __builtin_types_compatible_p(t1, t2) __builtin_types_compatible_p_impl((t1)0, (t2)0)

#ifdef __SIZE_TYPE__
typedef __SIZE_TYPE__ size_t;
#else
#error __SIZE_TYPE__ undefined
#endif

#ifdef __WCHAR_TYPE__
typedef __WCHAR_TYPE__ wchar_t;
#else
#error __WCHAR_TYPE__ undefined
#endif

#ifdef __PTRDIFF_TYPE__
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#else
#error __PTRDIFF_TYPE__ undefined
#endif

#define __FUNCTION__ __func__
#define __PRETTY_FUNCTION__ __func__

#ifdef __clang__
#define __builtin_convertvector(src, type) (*(type*)&src)
#endif
`

	objectFilePackageName       = objectFilePackageNamePrefix + objectFileSemver
	objectFilePackageNamePrefix = "__ccgo_object_file_"
	objectFileSemver            = "v1"
)

func init() {
	if !semver.IsValid(objectFileSemver) {
		panic(todo("invalid objectFileSemver: %q", objectFileSemver))
	}
}

// Task represents a compilation job.
type Task struct {
	D                []string // -D
	I                []string // -I
	O                string   // -O
	U                []string // -U
	args             []string // command name in args[0]
	cfg              *cc.Config
	cfgArgs          []string
	defs             string
	fs               fs.FS
	goarch           string
	goos             string
	inputFiles       []string
	l                []string // -l
	link             []string
	o                string // -o
	packageName      string // --package-name
	prefixDefine     string // --prefix-define <string>
	prefixEnum       string // --prefix-enum <string>
	prefixEnumerator string // --prefix-enumerator <string>
	prefixExternal   string // --prefix-external <string>
	prefixInternal   string // --prefix-internal <string>
	prefixNone       string // --prefix-none <string>
	prefixStruct     string // --prefix-struct <string>
	prefixTypename   string // --prefix-typename <string>
	prefixUnion      string // --prefix-union <string>
	prefixUnpinned   string // --prefix-unpinned <string>
	std              string // -std
	stderr           io.Writer
	stdout           io.Writer

	E              bool // -E
	c              bool // -c
	nostdinc       bool // -nostdinc
	nostdlib       bool // -nostdlib
	packageNameSet bool
	pthread        bool // -pthread
}

// NewTask returns a newly created Task. args[0] is the command name.
func NewTask(goos, goarch string, args []string, stdout, stderr io.Writer, fs fs.FS) (r *Task) {
	return &Task{
		args:   args,
		fs:     fs,
		goarch: goarch,
		goos:   goos,
		stderr: stderr,
		stdout: stdout,
	}
}

// Main executes task.
func (t *Task) Main() (err error) {
	if len(t.args) < 2 {
		return fmt.Errorf("invalid arguments %v", t.args)
	}

	set := opt.NewSet()
	set.Arg("-package-name", false, func(opt, val string) error { t.packageName = val; t.packageNameSet = true; return nil })
	set.Arg("-prefix-define", false, func(opt, val string) error { t.prefixDefine = val; return nil })
	set.Arg("-prefix-enum", false, func(opt, val string) error { t.prefixEnum = val; return nil })
	set.Arg("-prefix-enumerator", false, func(opt, val string) error { t.prefixEnumerator = val; return nil })
	set.Arg("-prefix-external", false, func(opt, val string) error { t.prefixExternal = val; return nil })
	set.Arg("-prefix-internal", false, func(opt, val string) error { t.prefixInternal = val; return nil })
	set.Arg("-prefix-none", false, func(opt, val string) error { t.prefixNone = val; return nil })
	set.Arg("-prefix-struct", false, func(opt, val string) error { t.prefixStruct = val; return nil })
	set.Arg("-prefix-typename", false, func(opt, val string) error { t.prefixTypename = val; return nil })
	set.Arg("-prefix-union", false, func(opt, val string) error { t.prefixUnion = val; return nil })
	set.Arg("-prefix-unpinned", false, func(opt, val string) error { t.prefixUnpinned = val; return nil })
	set.Arg("D", true, func(opt, val string) error { t.D = append(t.D, fmt.Sprintf("%s%s", opt, val)); return nil })
	set.Arg("I", true, func(opt, val string) error { t.I = append(t.I, val); return nil })
	set.Arg("O", true, func(opt, val string) error { t.O = fmt.Sprintf("%s%s", opt, val); return nil })
	set.Arg("U", true, func(opt, val string) error { t.U = append(t.U, fmt.Sprintf("%s%s", opt, val)); return nil })
	set.Arg("l", true, func(opt, val string) error { t.l = append(t.l, val); t.link = append(t.link, val); return nil })
	set.Arg("o", true, func(opt, val string) error { t.o = val; return nil })
	set.Arg("std", true, func(opt, val string) error { t.std = fmt.Sprintf("%s=%s", opt, val); return nil })
	set.Opt("E", func(opt string) error { t.E = true; return nil })
	set.Opt("c", func(opt string) error { t.c = true; return nil })
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
			return fmt.Errorf(" unrecognized command-line option '%s'", opt)
		}

		if strings.HasSuffix(opt, ".c") || strings.HasSuffix(opt, ".h") {
			t.inputFiles = append(t.inputFiles, opt)
			t.link = append(t.link, opt)
			return nil
		}

		return fmt.Errorf("unexpected argument %s", opt)
	}); err != nil {
		return fmt.Errorf("parsing %v: %v", t.args[1:], err)
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
		return t.compile()
	}

	return fmt.Errorf("TODO %v %v", t.args, t.inputFiles)
}

func (t *Task) clone() *Task { r := *t; return &r }

// -c
func (t *Task) compile() error {
	switch len(t.inputFiles) {
	case 0:
		return fmt.Errorf("no input files")
	case 1:
		// ok
	default:
		if t.o != "" {
			return fmt.Errorf("cannot specify '-o' with '-c' with multiple files")
		}
	}

	p := newParallel()
	for _, v := range t.inputFiles {
		p.exec(func() {
			p.err(t.compile1(v, func(msg string, args ...interface{}) { p.err(fmt.Errorf(msg, args...)) }))
		})
	}
	return p.wait()
}

func (t *Task) compile1(ifn string, eh errHandler) error {
	//  package __ccgo_object_file_v1
	//
	//	enumerator constant	c<name>
	//	#define			d<name>
	//	enum tag		e<name>
	//	linkage internal	i<name>
	//	struct tag		s<name>
	//	union tag		u<name>
	//	unpinned declarator	p<name>
	//	typedef			t<name>
	//	linkage external	x<name>
	//	linkage none		_<name>
	t = t.clone()
	t.packageName = objectFilePackageName
	t.prefixDefine = "d"
	t.prefixEnum = "e"
	t.prefixEnumerator = "c"
	t.prefixExternal = "x"
	t.prefixInternal = "i"
	t.prefixNone = "_"
	t.prefixStruct = "s"
	t.prefixTypename = "t"
	t.prefixUnion = "u"
	t.prefixUnpinned = "p"
	ofn := t.o
	if ofn == "" {
		base := filepath.Base(ifn)
		ext := filepath.Ext(base)
		ofn = base[:len(base)-len(ext)] + ".go"
	}
	return newCtx(t, eh).compile(ifn, ofn)
}
