// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:generate stringer -output stringer.go -type=exprMode,opKind

package main // import "modernc.org/ccgo/v3"

import (
	"bufio"
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

//TODO gmp
//TODO gsl
//TODO minigmp
//TODO mpc
//TODO mpfr
//TODO pcre
//TODO pcre2
//TODO quickjs
//TODO zdat
//TODO zlib
//TODO CPython
//TODO Cython
//TODO redis
//TODO gofrontend
//TODO wolfssl

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
#define __builtin_offsetof(type, member) ((__SIZE_TYPE__)&(((type*)0)->member))
#define __builtin_va_arg(ap, type) (type)__ccgo_va_arg(ap)
#define __builtin_va_copy(dst, src) dst = src
#define __builtin_va_end(ap) __ccgo_va_end(ap)
#define __builtin_va_start(ap, v) __ccgo_va_start(ap)
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
char *__builtin_strchr(const char *s, int c);
char *__builtin_strcpy(char *dest, const char *src);
double __builtin_copysign ( double x, double y );
double __builtin_fabs(double x);
double __builtin_huge_val (void);
double __builtin_inf (void);
float __builtin_copysignf ( float x, float y );
float __builtin_huge_valf (void);
float __builtin_inff (void);
int __builtin_abs(int j);
int __builtin_add_overflow();
int __builtin_clzll (unsigned long long);
int __builtin_memcmp(const void *s1, const void *s2, size_t n);
int __builtin_mul_overflow();
int __builtin_printf(const char *format, ...);
int __builtin_snprintf(char *str, size_t size, const char *format, ...);
int __builtin_sprintf(char *str, const char *format, ...);
int __builtin_strcmp(const char *s1, const char *s2);
int __builtin_sub_overflow();
long __builtin_expect (long exp, long c);
long long __builtin_llabs(long long j);
size_t __builtin_strlen(const char *s);
void *__builtin_malloc(size_t size);
void *__builtin_memcpy(void *dest, const void *src, size_t n);
void *__builtin_memset(void *s, int c, size_t n);
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
	r := fmt.Sprintf("\n%s:%d: TRC %s", fn, fl, s)
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

type task struct {
	D               []string // -D
	I               []string // -I
	U               []string // -U
	args            []string
	asts            []*cc.AST
	capif           string
	cfg             *cc.Config
	crt             string
	crtImportPath   string // -ccgo-crt-import-path
	exportDefines   string // -ccgo-export-defines
	exportEnums     string // -ccgo-export-enums
	exportExterns   string // -ccgo-export-externs
	exportFields    string // -ccgo-export-fields
	exportStructs   string // -ccgo-export-structs
	exportTypedefs  string // -ccgo-export-typedefs
	goarch          string
	goos            string
	hide            map[string]struct{} // -ccgo-hide
	hostConfigCmd   string              // -ccgo-host-config-cmd
	hostConfigOpts  string              // -ccgo-host-config-opts
	ignoredIncludes string              // -ccgo-ignored-includes
	imported        []*imported
	l               []string // -l
	o               string   // -o
	out             io.Writer
	pkgName         string // -ccgo-pkgname
	sources         []cc.Source
	stderr          io.Writer
	stdout          io.Writer
	symSearchOrder  []int // >= 0: asts[i], < 0 : imported[-i-1]

	E                     bool // -E
	allErrors             bool // -ccgo-all-errors
	cover                 bool // -ccgo-cover-instrumentation
	coverC                bool // -ccgo-cover-instrumentation-c
	exportDefinesValid    bool // -ccgo-export-defines present
	exportEnumsValid      bool // -ccgo-export-enums present
	exportExternsValid    bool // -ccgo-export-externs present
	exportFieldsValid     bool // -ccgo-export-fields present
	exportStructsValid    bool // -ccgo-export-structs present
	exportTypedefsValid   bool // -ccgo-export-typedefs present
	fullPathComments      bool // -ccgo-full-path-comments
	header                bool // -ccgo-header
	libc                  bool // -ccgo-libc
	mingw                 bool
	nostdinc              bool // -nostdinc
	traceTranslationUnits bool // -ccgo-trace-translation-units
	verifyStructs         bool // -ccgo-verify-structs
	watch                 bool // -ccgo-watch-instrumentation
	windows               bool // -ccgo-windows
}

func newTask(args []string, stdout, stderr io.Writer) *task {
	if stdout == nil {
		stdout = os.Stdout
	}
	if stderr == nil {
		stderr = os.Stderr
	}
	return &task{
		args: args,
		cfg: &cc.Config{
			DoNotTypecheckAsm:         true,
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
func (t *task) capi(path string) (pkgName string, exports map[string]struct{}, err error) {
	defer func() {
		if err != nil {
			err = fmt.Errorf("loading C exports from %s (GOPATH=%v GO111MODULE=%v): %v", path, os.Getenv("GOPATH"), os.Getenv("GO111MODULE"), err)
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

func (t *task) capi2(files []string) (pkgName string, exports map[string]struct{}, err error) {
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
	if err := newTask(os.Args, os.Stdout, os.Stderr).main(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func (t *task) main() (err error) {
	opts := opt.NewSet()
	opts.Arg("D", true, func(arg, value string) error { t.D = append(t.D, value); return nil })
	opts.Arg("I", true, func(opt, arg string) error { t.I = append(t.I, arg); return nil })
	opts.Arg("U", true, func(arg, value string) error { t.U = append(t.U, value); return nil })
	opts.Arg("ccgo-crt-import-path", false, func(arg, value string) error { t.crtImportPath = value; return nil })
	opts.Arg("ccgo-export-defines", false, func(arg, value string) error { t.exportDefines = value; t.exportDefinesValid = true; return nil })
	opts.Arg("ccgo-export-enums", false, func(arg, value string) error { t.exportEnums = value; t.exportEnumsValid = true; return nil })
	opts.Arg("ccgo-export-externs", false, func(arg, value string) error { t.exportExterns = value; t.exportExternsValid = true; return nil })
	opts.Arg("ccgo-export-fields", false, func(arg, value string) error { t.exportFields = value; t.exportFieldsValid = true; return nil })
	opts.Arg("ccgo-export-structs", false, func(arg, value string) error { t.exportStructs = value; t.exportStructsValid = true; return nil })
	opts.Arg("ccgo-export-typedefs", false, func(arg, value string) error { t.exportTypedefs = value; t.exportTypedefsValid = true; return nil })
	opts.Arg("ccgo-host-config-cmd", false, func(arg, value string) error { t.hostConfigCmd = value; return nil })
	opts.Arg("ccgo-host-config-opts", false, func(arg, value string) error { t.hostConfigOpts = value; return nil })
	opts.Arg("ccgo-ignored-includes", false, func(arg, value string) error { t.ignoredIncludes = value; return nil })
	opts.Arg("ccgo-pkgname", false, func(arg, value string) error { t.pkgName = value; return nil })
	opts.Opt("E", func(opt string) error { t.E = true; return nil })
	opts.Opt("ccgo-all-errors", func(opt string) error { t.allErrors = true; return nil })
	opts.Opt("ccgo-cover-instrumentation", func(opt string) error { t.cover = true; return nil })
	opts.Opt("ccgo-cover-instrumentation-c", func(opt string) error { t.coverC = true; return nil })
	opts.Opt("ccgo-full-path-comments", func(opt string) error { t.fullPathComments = true; return nil })
	opts.Opt("ccgo-header", func(opt string) error { t.header = true; return nil })
	opts.Opt("ccgo-long-double-is-double", func(opt string) error { t.cfg.LongDoubleIsDouble = true; return nil })
	opts.Opt("ccgo-trace-translation-units", func(opt string) error { t.traceTranslationUnits = true; return nil })
	opts.Opt("ccgo-verify-structs", func(opt string) error { t.verifyStructs = true; return nil })
	opts.Opt("ccgo-watch-instrumentation", func(opt string) error { t.watch = true; return nil })
	opts.Opt("ccgo-windows", func(opt string) error { t.windows = true; return nil })
	opts.Opt("nostdinc", func(opt string) error { t.nostdinc = true; return nil })
	opts.Opt("ccgo-libc", func(opt string) error {
		t.libc = true
		t.crt = ""
		t.crtImportPath = ""
		return nil
	})
	opts.Arg("ccgo-hide", false, func(arg, value string) error {
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

	t.mingw = detectMingw(hostPredefined)
	if t.mingw {
		t.windows = true
	}
	if !t.mingw {
		a := strings.Split(hostPredefined, "\n")
		wi := 0
		for _, v0 := range a {
			v := strings.TrimSpace(strings.ToLower(v0))
			if !strings.HasPrefix(v, "#define __gnu") && !strings.HasPrefix(v, "#define __gcc") {
				a[wi] = v0
				wi++
			}
		}
		hostPredefined = strings.Join(a[:wi], "\n")
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
			if err := cc.Preprocess(t.cfg, includePaths, sysIncludePaths, tuSources, os.Stdout); err != nil {
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
		memGuard(i)
	}
	if t.E {
		return nil
	}

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

func detectMingw(s string) bool {
	return strings.Contains(s, "#define __MINGW")
}

func memGuard(i int) {
	if totalRam == 0 || totalRam > 64e9 {
		return
	}

	var ms runtime.MemStats
	runtime.ReadMemStats(&ms)
	switch {
	case ms.Alloc < totalRam/2:
		return
	case ms.Alloc < (8*totalRam)/10:
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
