// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate stringer -output stringer.go -type=exprMode,opKind

// Package ccgo implements the ccgo command.
package ccgo // import "modernc.org/ccgo/v3/lib"

import (
	"bufio"
	"encoding/csv"
	"fmt"
	"go/ast"
	"go/build"
	"go/parser"
	"go/token"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"strconv"
	"strings"
	"time"

	"golang.org/x/tools/go/packages"
	"modernc.org/cc/v3"
	_ "modernc.org/libc"
	"modernc.org/opt"
)

//TODO parallel

//TODO CPython
//TODO Cython
//TODO gmp
//TODO gofrontend
//TODO gsl
//TODO minigmp
//TODO mpc
//TODO mpfr
//TODO pcre
//TODO pcre2
//TODO quickjs
//TODO redis
//TODO wolfssl
//TODO zdat
//TODO zlib
//TODO zstd

//TODO 2020-07-17
//
// Fix += and friends
//
// Audit all unsafe.Pointer conversions
//
// Remove double dereferencing **
//
// Shifts must not use n.Promote on left opearand
//
// Un-array
//
// Pass more CSmith tests.

//TODO merge VaList slots of distinct top level statements.

//TODO turn void
//
//	a = b = c = d
//
// where all but the first and last of a, b, c, ... are declarators, into
//
//	c = d
//	b = c
//	a = b

//TODO define and use all tagged struct types, including inner ones, for example SQLite's SrcList_item.

//TODO rewrite return conditionalExpression so it has no closures. Partially done.

//TODO define and restore simple named constants. Having
//
//	#define FOO 42
//	...
//	case FOO:
//
// we do not yet define FOO and generate
//
//	case 42:

//TODO do not generate a terminating semicolon for empty statements.

//TODO replace
//
//	var sqlite3_data_directory uintptr = uintptr(0) /* sqlite3.c:156345:17 */
//
// by
//
//	var sqlite3_data_directory uintptr = 0 /* sqlite3.c:156345:17 */
//
// or
//
//	var sqlite3_data_directory = uintptr(0) /* sqlite3.c:156345:17 */

//TODO drop all non-referenced declarators unless forced by a command line flag.

const (
	builtin = `
#ifdef __PTRDIFF_TYPE__
typedef __PTRDIFF_TYPE__ ptrdiff_t;
#else
#error __PTRDIFF_TYPE__ undefined
#endif

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

#ifdef __SIZEOF_INT128__
typedef __INT64_TYPE__ __int128_t[2];	//TODO
typedef __UINT64_TYPE__ __uint128_t[2];	//TODO
#endif;

#define _FILE_OFFSET_BITS 64
#define __FUNCTION__ __func__
#define __PRETTY_FUNCTION__ __func__
#define __asm __asm__
#define __attribute__(...)
#define __builtin_constant_p(x) __builtin_constant_p_impl(0, x)
#define __builtin_offsetof(type, member) ((__SIZE_TYPE__)&(((type*)0)->member))
#define __builtin_va_arg(ap, type) (type)__ccgo_va_arg(ap)
#define __builtin_va_copy(dst, src) dst = src
#define __builtin_va_end(ap) __ccgo_va_end(ap)
#define __builtin_va_start(ap, v) __ccgo_va_start(ap)
#define __ccgo_fd_zero(set) __builtin_memset(set, 0, sizeof(fd_set))
#define __ccgo_tcl_default_double_rounding(set) ((void)0)
#define __ccgo_tcl_ieee_double_rounding(set) ((void)0)
#define __extension__
#define __has_include(...) __has_include_impl(#__VA_ARGS__)
#define __has_include_impl(x)
#define __inline__ inline
#define __signed signed
#define asm __asm__
#define in6addr_any (*__ccgo_in6addr_anyp())

typedef void *__builtin_va_list;
typedef long double __float128;

#if defined(__MINGW32__) || defined(__MINGW64__)
typedef __builtin_va_list va_list;
int gnu_printf(const char *format, ...);
int gnu_scanf(const char *format, ...);
int ms_printf(const char *format, ...);
int ms_scanf(const char *format, ...);
#define _VA_LIST_DEFINED
#define __extension__
#endif

__UINT16_TYPE__ __builtin_bswap16 (__UINT16_TYPE__ x);
__UINT32_TYPE__ __builtin_bswap32 (__UINT32_TYPE__ x);
__UINT64_TYPE__ __builtin_bswap64 (__UINT64_TYPE__ x);
char *__builtin___strcat_chk (char *dest, const char *src, size_t os);
char *__builtin___strcpy_chk (char *dest, const char *src, size_t os);
char *__builtin___strncpy_chk(char *dest, char *src, size_t n, size_t os);
char *__builtin_strchr(const char *s, int c);
char *__builtin_strcpy(char *dest, const char *src);
double __builtin_copysign ( double x, double y );
double __builtin_fabs(double x);
double __builtin_huge_val (void);
double __builtin_inf (void);
float __builtin_copysignf ( float x, float y );
float __builtin_huge_valf (void);
float __builtin_inff (void);
float __builtin_nanf (const char *str);
int __builtin___sprintf_chk (char *s, int flag, size_t os, const char *fmt, ...);
int __builtin__snprintf_chk(char * str, size_t maxlen, int flag, size_t strlen, const char * format);
int __builtin_abs(int j);
int __builtin_add_overflow();
int __builtin_clzll (unsigned long long);
int __builtin_constant_p_impl(int, ...);
int __builtin_isnan(double);
int __builtin_memcmp(const void *s1, const void *s2, size_t n);
int __builtin_mul_overflow();
int __builtin_printf(const char *format, ...);
int __builtin_snprintf(char *str, size_t size, const char *format, ...);
int __builtin_sprintf(char *str, const char *format, ...);
int __builtin_strcmp(const char *s1, const char *s2);
int __builtin_sub_overflow();
long __builtin_expect (long exp, long c);
long long __builtin_llabs(long long j);
size_t __builtin_object_size (void * ptr, int type);
size_t __builtin_strlen(const char *s);
void *__builtin___memcpy_chk (void *dest, const void *src, size_t n, size_t os);
void *__builtin___memmove_chk (void *dest, const void *src, size_t n, size_t os);
void *__builtin___memset_chk (void *dstpp, int c, size_t len, size_t dstlen);
void *__builtin_malloc(size_t size);
void *__builtin_memcpy(void *dest, const void *src, size_t n);
void *__builtin_memset(void *s, int c, size_t n);
void *__builtin_mmap(void *addr, size_t length, int prot, int flags, int fd, __INTPTR_TYPE__ offset);
void *__ccgo_va_arg(__builtin_va_list ap);
void __builtin_abort(void);
void __builtin_exit(int status);
void __builtin_free(void *ptr);
void __builtin_prefetch (const void *addr, ...);
void __builtin_trap (void);
void __builtin_unreachable (void);
void __ccgo_va_end(__builtin_va_list ap);
void __ccgo_va_start(__builtin_va_list ap);
`
	defaultCrt = "modernc.org/libc"
)

func origin(skip int) string {
	pc, fn, fl, _ := runtime.Caller(skip)
	fn = filepath.Base(fn)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
	}
	return fmt.Sprintf("%s:%d:%s", fn, fl, fns)
}

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
	r := fmt.Sprintf("%s:%d: TRC %s", fn, fl, s)
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

// Task represents a compilation job.
type Task struct {
	D                               []string // -D
	I                               []string // -I
	U                               []string // -U
	args                            []string
	asts                            []*cc.AST
	capif                           string
	cfg                             *cc.Config
	crt                             string
	crtImportPath                   string // -crt-import-path
	exportDefines                   string // -export-defines
	exportEnums                     string // -export-enums
	exportExterns                   string // -export-externs
	exportFields                    string // -export-fields
	exportStructs                   string // -export-structs
	exportTypedefs                  string // -export-typedefs
	goarch                          string
	goos                            string
	hide                            map[string]struct{} // -hide
	hostConfigCmd                   string              // -host-config-cmd
	hostConfigOpts                  string              // -host-config-opts
	ignoredIncludes                 string              // -ignored-includes
	imported                        []*imported
	l                               []string // -l
	o                               string   // -o
	out                             io.Writer
	pkgName                         string // -pkgname
	replaceFdZero                   string // -replace-fd-zero
	replaceTclDefaultDoubleRounding string // -replace-tcl-default-double-rounding
	replaceTclIeeeDoubleRounding    string // -replace-tcl-default-double-rounding
	scriptFn                        string // -script
	sources                         []cc.Source
	stderr                          io.Writer
	stdout                          io.Writer
	symSearchOrder                  []int // >= 0: asts[i], < 0 : imported[-i-1]

	E                     bool // -E
	allErrors             bool // -all-errors
	cover                 bool // -cover-instrumentation
	coverC                bool // -cover-instrumentation-c
	errTrace              bool // -err-trace
	exportDefinesValid    bool // -export-defines present
	exportEnumsValid      bool // -export-enums present
	exportExternsValid    bool // -export-externs present
	exportFieldsValid     bool // -export-fields present
	exportStructsValid    bool // -export-structs present
	exportTypedefsValid   bool // -export-typedefs present
	fullPathComments      bool // -full-path-comments
	header                bool // -header
	isScripted            bool
	mingw                 bool
	nostdinc              bool // -nostdinc
	nostdlib              bool // -nostdlib
	traceTranslationUnits bool // -trace-translation-units
	verifyStructs         bool // -verify-structs
	watch                 bool // -watch-instrumentation
	windows               bool // -windows
}

// NewTask returns a newly created Task.
func NewTask(args []string, stdout, stderr io.Writer) *Task {
	if stdout == nil {
		stdout = os.Stdout
	}
	if stderr == nil {
		stderr = os.Stderr
	}
	return &Task{
		args: args,
		cfg: &cc.Config{
			DoNotTypecheckAsm:         true,
			LongDoubleIsDouble:        true,
			SharedFunctionDefinitions: &cc.SharedFunctionDefinitions{},
		},
		crt:           "libc.",
		crtImportPath: defaultCrt,
		goarch:        env("TARGET_GOARCH", env("GOARCH", runtime.GOARCH)),
		goos:          env("TARGET_GOOS", env("GOOS", runtime.GOOS)),
		hide:          map[string]struct{}{},
		hostConfigCmd: env("CCGO_CPP", ""),
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

// Get exported symbols from package having import path 'path'.
func (t *Task) capi(path string) (pkgName string, exports map[string]struct{}, err error) {
	defer func() {
		if err != nil {
			wd, err2 := os.Getwd()
			err = fmt.Errorf("(wd %q, %v): loading C exports from %s (GOPATH=%v GO111MODULE=%v): %v", wd, err2, path, os.Getenv("GOPATH"), os.Getenv("GO111MODULE"), err)
		}
	}()

	pkgs, err := packages.Load(
		&packages.Config{
			Mode: packages.NeedFiles,
			Env:  append(os.Environ(), fmt.Sprintf("GOOS=%s", t.goos), fmt.Sprintf("GOARCH=%s", t.goarch)),
		},
		path,
	)
	switch {
	case err == nil:
		if len(pkgs) != 1 {
			err = fmt.Errorf("expected one package, loaded %d", len(pkgs))
			break
		}

		pkg := pkgs[0]
		if len(pkg.Errors) != 0 {
			var a []string
			for _, v := range pkg.Errors {
				a = append(a, v.Error())
			}
			err = fmt.Errorf("%s", strings.Join(a, "\n"))
			break
		}

		return t.capi2(pkg.GoFiles)
	}

	gopath := os.Getenv("GOPATH")
	if gopath == "" || !filepath.IsAbs(gopath) {
		return "", nil, err
	}

	ctx := build.Context{
		GOARCH:   t.goarch,
		GOOS:     t.goos,
		GOPATH:   gopath,
		Compiler: "gc",
	}
	arg := filepath.Join(gopath, "src", path)
	pkg, err2 := ctx.ImportDir(arg, 0)
	if err2 != nil {
		return "", nil, fmt.Errorf("%v (ImportDir %q: %v)", err, arg, err2)
	}

	for i, v := range pkg.GoFiles {
		pkg.GoFiles[i] = filepath.Join(gopath, "src", path, v)
	}
	return t.capi2(pkg.GoFiles)
}

func (t *Task) capi2(files []string) (pkgName string, exports map[string]struct{}, err error) {
	exports = map[string]struct{}{}
	base := fmt.Sprintf("capi_%s_%s.go", t.goos, t.goarch)
	var fn string
	for _, v := range files {
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

func main() {
	if err := NewTask(os.Args, os.Stdout, os.Stderr).Main(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

// Main executes task.
func (t *Task) Main() (err error) {
	opts := opt.NewSet()
	opts.Arg("D", true, func(arg, value string) error { t.D = append(t.D, value); return nil })
	opts.Arg("I", true, func(opt, arg string) error { t.I = append(t.I, arg); return nil })
	opts.Arg("U", true, func(arg, value string) error { t.U = append(t.U, value); return nil })
	opts.Arg("crt-import-path", false, func(arg, value string) error { t.crtImportPath = value; return nil })
	opts.Arg("export-defines", false, func(arg, value string) error { t.exportDefines = value; t.exportDefinesValid = true; return nil })
	opts.Arg("export-enums", false, func(arg, value string) error { t.exportEnums = value; t.exportEnumsValid = true; return nil })
	opts.Arg("export-externs", false, func(arg, value string) error { t.exportExterns = value; t.exportExternsValid = true; return nil })
	opts.Arg("export-fields", false, func(arg, value string) error { t.exportFields = value; t.exportFieldsValid = true; return nil })
	opts.Arg("export-structs", false, func(arg, value string) error { t.exportStructs = value; t.exportStructsValid = true; return nil })
	opts.Arg("export-typedefs", false, func(arg, value string) error { t.exportTypedefs = value; t.exportTypedefsValid = true; return nil })
	opts.Arg("host-config-cmd", false, func(arg, value string) error { t.hostConfigCmd = value; return nil })
	opts.Arg("host-config-opts", false, func(arg, value string) error { t.hostConfigOpts = value; return nil })
	opts.Arg("ignored-includes", false, func(arg, value string) error { t.ignoredIncludes = value; return nil })
	opts.Arg("pkgname", false, func(arg, value string) error { t.pkgName = value; return nil })
	opts.Arg("replace-fd-zero", false, func(arg, value string) error { t.replaceFdZero = value; return nil })
	opts.Arg("replace-tcl-default-double-rounding", false, func(arg, value string) error { t.replaceTclDefaultDoubleRounding = value; return nil })
	opts.Arg("replace-tcl-ieee-double-rounding", false, func(arg, value string) error { t.replaceTclIeeeDoubleRounding = value; return nil })
	opts.Arg("script", false, func(arg, value string) error { t.scriptFn = value; return nil })
	opts.Opt("E", func(opt string) error { t.E = true; return nil })
	opts.Opt("all-errors", func(opt string) error { t.allErrors = true; return nil })
	opts.Opt("cover-instrumentation", func(opt string) error { t.cover = true; return nil })
	opts.Opt("cover-instrumentation-c", func(opt string) error { t.coverC = true; return nil })
	opts.Opt("err-trace", func(opt string) error { t.errTrace = true; return nil })
	opts.Opt("full-path-comments", func(opt string) error { t.fullPathComments = true; return nil })
	opts.Opt("header", func(opt string) error { t.header = true; return nil })
	opts.Opt("nostdinc", func(opt string) error { t.nostdinc = true; return nil })
	opts.Opt("trace-translation-units", func(opt string) error { t.traceTranslationUnits = true; return nil })
	opts.Opt("verify-structs", func(opt string) error { t.verifyStructs = true; return nil })
	opts.Opt("watch-instrumentation", func(opt string) error { t.watch = true; return nil })
	opts.Opt("windows", func(opt string) error { t.windows = true; return nil })
	opts.Opt("nostdlib", func(opt string) error {
		t.nostdlib = true
		t.crt = ""
		t.crtImportPath = ""
		return nil
	})
	opts.Arg("hide", false, func(arg, value string) error {
		value = strings.TrimSpace(value)
		a := strings.Split(value, ",")
		for _, v := range a {
			t.hide[v] = struct{}{}
		}
		return nil
	})
	opts.Arg("l", true, func(arg, value string) error {
		value = strings.TrimSpace(value)
		a := strings.Split(value, ",")
		for _, v := range a {
			t.l = append(t.l, v)
			t.symSearchOrder = append(t.symSearchOrder, -len(t.l))
		}
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
		case ".h":
			t.symSearchOrder = append(t.symSearchOrder, len(t.sources))
			t.sources = append(t.sources, cc.Source{Name: arg})
		case ".c":
			t.symSearchOrder = append(t.symSearchOrder, len(t.sources))
			t.sources = append(t.sources, cc.Source{Name: arg, DoNotCache: true})

		default:
			return fmt.Errorf("unexpected file type: %s", arg)
		}

		return nil
	}); err != nil {
		return err
	}
	if t.scriptFn != "" {
		return t.script(t.scriptFn)
	}

	if len(t.sources) == 0 {
		return fmt.Errorf("no input files specified")
	}

	if t.crtImportPath != "" {
		t.l = append(t.l, t.crtImportPath)
		t.symSearchOrder = append(t.symSearchOrder, -len(t.l))
		m := map[string]struct{}{}
		for _, v := range t.l {
			v = strings.TrimSpace(v)
			if _, ok := m[v]; !ok {
				t.imported = append(t.imported, &imported{path: v})
				m[v] = struct{}{}
			}
		}
		t.imported[len(t.imported)-1].used = true // crt is always imported
	}
	abi, err := cc.NewABI(t.goos, t.goarch)
	if err != nil {
		return err
	}

	var re *regexp.Regexp
	if t.ignoredIncludes != "" {
		if re, err = regexp.Compile(t.ignoredIncludes); err != nil {
			return err
		}
	}

	t.cfg.ABI = abi
	t.cfg.ReplaceMacroFdZero = t.replaceFdZero
	t.cfg.ReplaceMacroTclDefaultDoubleRounding = t.replaceTclDefaultDoubleRounding
	t.cfg.ReplaceMacroTclIeeeDoubleRounding = t.replaceTclIeeeDoubleRounding
	t.cfg.Config3.IgnoreInclude = re
	t.cfg.Config3.NoFieldAndBitfieldOverlap = true
	t.cfg.Config3.PreserveWhiteSpace = true
	t.cfg.Config3.UnsignedEnums = true
	hostConfigOpts := strings.Split(t.hostConfigOpts, ",")
	if t.hostConfigOpts == "" {
		hostConfigOpts = nil
	}
	hostPredefined, hostIncludes, hostSysIncludes, err := cc.HostConfig(t.hostConfigCmd, hostConfigOpts...)
	if err != nil {
		return err
	}

	if t.mingw = detectMingw(hostPredefined); t.mingw {
		t.windows = true
	}
	if t.nostdinc {
		hostIncludes = nil
		hostSysIncludes = nil
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
		sources = append(sources, cc.Source{Name: "<defines>", Value: strings.Join(a, "\n"), DoNotCache: true})
	}
	if len(t.U) != 0 {
		var a []string
		for _, v := range t.U {
			a = append(a, fmt.Sprintf("#undef %s", v))
		}
		a = append(a, "\n")
		sources = append(sources, cc.Source{Name: "<undefines>", Value: strings.Join(a, "\n"), DoNotCache: true})
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
	// For headers whose names are enclosed in angle brackets ( "<>" ), the
	// header shall be searched for only in directories named in -I options
	// and then in the usual places.
	sysIncludePaths := append(t.I, hostSysIncludes...)
	if t.traceTranslationUnits {
		fmt.Printf("target: %s/%s\n", t.goos, t.goarch)
		if t.hostConfigCmd != "" {
			fmt.Printf("host config cmd: %s\n", t.hostConfigCmd)
		}
	}
	for i, v := range t.sources {
		tuSources := append(sources, v)
		if t.E {
			t.cfg.PreprocessOnly = true
			if err := cc.Preprocess(t.cfg, includePaths, sysIncludePaths, tuSources, t.stdout); err != nil {
				return err
			}
			continue
		}

		var t0 time.Time
		if t.traceTranslationUnits {
			fmt.Printf("C front end %d/%d: %s ... ", i+1, len(t.sources), v.Name)
			t0 = time.Now()
		}
		ast, err := cc.Translate(t.cfg, includePaths, sysIncludePaths, tuSources)
		if err != nil {
			return err
		}

		if t.traceTranslationUnits {
			fmt.Println(time.Since(t0))
		}
		t.asts = append(t.asts, ast)
		memGuard(i, t.isScripted)
	}
	if t.E || t.isScripted {
		return nil
	}

	return t.link()
}

func (t *Task) link() (err error) {
	if t.o == "" {
		t.o = fmt.Sprintf("a_%s_%s.go", t.goos, t.goarch)
	}
	dir := filepath.Dir(t.o)
	t.capif = filepath.Join(dir, fmt.Sprintf("capi_%s_%s.go", t.goos, t.goarch))
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

func (t *Task) script(fn string) error {
	f, err := os.Open(fn)
	if err != nil {
		return err
	}

	defer f.Close()

	r := csv.NewReader(f)
	r.Comment = '#'
	r.FieldsPerRecord = -1
	r.TrimLeadingSpace = true
	script, err := r.ReadAll()
	if err != nil {
		return err
	}

	ccgo := []string{t.args[0]}
	for i, line := range script {
		dir := line[0]
		args := line[1:]
		for _, v := range args {
			if strings.HasSuffix(v, ".c") || strings.HasSuffix(v, ".h") {
				v = filepath.Join(dir, v)
				if t.traceTranslationUnits {
					fmt.Printf("ccgo.script: translate %s\n", v)
				}
				t.symSearchOrder = append(t.symSearchOrder, len(t.sources))
				t.sources = append(t.sources, cc.Source{Name: v})
			}
		}
		t2 := NewTask(append(ccgo, args...), t.stdout, t.stderr)
		t2.cfg.SharedFunctionDefinitions = t.cfg.SharedFunctionDefinitions
		t2.isScripted = true
		if err := inDir(dir, t2.Main); err != nil {
			return err
		}

		t.asts = append(t.asts, t2.asts...)
		if i == 0 {
			t.cfg = t2.cfg
		}
	}
	if t.crtImportPath != "" {
		t.l = append(t.l, t.crtImportPath)
		t.symSearchOrder = append(t.symSearchOrder, -len(t.l))
		m := map[string]struct{}{}
		for _, v := range t.l {
			v = strings.TrimSpace(v)
			if _, ok := m[v]; !ok {
				t.imported = append(t.imported, &imported{path: v})
				m[v] = struct{}{}
			}
		}
		t.imported[len(t.imported)-1].used = true // crt is always imported
	}
	return t.link()
}

func inDir(dir string, f func() error) (err error) {
	var cwd string
	if cwd, err = os.Getwd(); err != nil {
		return err
	}

	defer func() {
		if err2 := os.Chdir(cwd); err2 != nil {
			err = err2
		}
	}()

	if err = os.Chdir(dir); err != nil {
		return err
	}

	return f()
}

func detectMingw(s string) bool {
	return strings.Contains(s, "#define __MINGW")
}

func memGuard(i int, force bool) {
	if totalRam == 0 || totalRam > 64e9 {
		return
	}

	var ms runtime.MemStats
	runtime.ReadMemStats(&ms)
	switch {
	case ms.Alloc < totalRam/2:
		return
	case ms.Alloc < (8*totalRam)/10:
		if force {
			break
		}

		switch {
		case totalRam < 1e9:
			// ok
		case totalRam < 16e9:
			if i&1 == 1 {
				return
			}
		default:
			if i&3 != 3 {
				return
			}
		}
	}

	debug.FreeOSMemory()
}
