// Copyright 2020 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/ccgo/v3"

import (
	"archive/tar"
	"archive/zip"
	"bufio"
	"bytes"
	"compress/bzip2"
	"compress/gzip"
	"context"
	"encoding/hex"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"reflect"
	"regexp"
	"runtime"
	"runtime/debug"
	"sort"
	"strings"
	"testing"
	"time"
	"unsafe"

	"github.com/dustin/go-humanize"
)

func caller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "# caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	_, fn, fl, _ = runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "# \tcallee: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func dbg(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "# dbg %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func TODO(...interface{}) string { //TODOOK
	_, fn, fl, _ := runtime.Caller(1)
	return fmt.Sprintf("# TODO: %s:%d:\n", path.Base(fn), fl) //TODOOK
}

func stack() string { return string(debug.Stack()) }

func use(...interface{}) {}

func init() {
	use(caller, dbg, TODO, stack) //TODOOK
}

// ----------------------------------------------------------------------------

var (
	oBlackBox   = flag.String("blackbox", "", "Record CSmith file to this file")
	oCSmith     = flag.Duration("csmith", 2*time.Minute, "")
	oDebug      = flag.Bool("debug", false, "")
	oDev        = flag.Bool("dev", false, "Enable developer tests/downloads.")
	oDownload   = flag.Bool("download", false, "Download missing testdata. Add -dev to download also 100+ MB of developer resources.")
	oMem        = flag.Bool("mem", false, "")
	oRE         = flag.String("re", "", "")
	oStackTrace = flag.Bool("trcstack", false, "")
	oTrace      = flag.Bool("trc", false, "Print tested paths.")
	oTraceF     = flag.Bool("trcf", false, "Print test file content")
	oTraceO     = flag.Bool("trco", false, "Print test output")
	oXTags      = flag.String("xtags", "", "passed to go build of TestSQLite")

	gccDir    = filepath.FromSlash("testdata/gcc-9.1.0")
	gpsdDir   = filepath.FromSlash("testdata/gpsd-3.20/")
	ntpsecDir = filepath.FromSlash("testdata/ntpsec-master")
	sqliteDir = filepath.FromSlash("testdata/sqlite-amalgamation-3330000")
	tccDir    = filepath.FromSlash("testdata/tcc-0.9.27")
	mjsonDir  = filepath.FromSlash("testdata/microjson-1.5")

	testWD string

	csmithDefaultArgs = strings.Join([]string{
		"--bitfields",                     // --bitfields | --no-bitfields: enable | disable full-bitfields structs (disabled by default).
		"--max-nested-struct-level", "10", // --max-nested-struct-level <num>: limit maximum nested level of structs to <num>(default 0). Only works in the exhaustive mode.
		"--no-const-pointers",    // --const-pointers | --no-const-pointers: enable | disable const pointers (enabled by default).
		"--no-consts",            // --consts | --no-consts: enable | disable const qualifier (enabled by default).
		"--no-packed-struct",     // --packed-struct | --no-packed-struct: enable | disable packed structs by adding #pragma pack(1) before struct definition (disabled by default).
		"--no-volatile-pointers", // --volatile-pointers | --no-volatile-pointers: enable | disable volatile pointers (enabled by default).
		"--no-volatiles",         // --volatiles | --no-volatiles: enable | disable volatiles (enabled by default).
		"--paranoid",             // --paranoid | --no-paranoid: enable | disable pointer-related assertions (disabled by default).
	}, " ")

	downloads = []struct {
		dir, url string
		sz       int
		dev      bool
	}{
		{gccDir, "http://mirror.koddos.net/gcc/releases/gcc-9.1.0/gcc-9.1.0.tar.gz", 118000, true},
		{gpsdDir, "http://download-mirror.savannah.gnu.org/releases/gpsd/gpsd-3.20.tar.gz", 3600, false},
		{mjsonDir, "https://gitlab.com/esr/microjson/-/archive/1.5/microjson-1.5.tar.gz", 22, false},
		{ntpsecDir, "https://gitlab.com/NTPsec/ntpsec/-/archive/master/ntpsec-master.tar.gz", 2600, false},
		{sqliteDir, "https://www.sqlite.org/2020/sqlite-amalgamation-3330000.zip", 2400, false},
		{tccDir, "http://download.savannah.gnu.org/releases/tinycc/tcc-0.9.27.tar.bz2", 620, false},
	}
)

func TestMain(m *testing.M) {
	defer func() {
		os.Exit(m.Run())
	}()

	fmt.Printf("test binary compiled for %s/%s\n", runtime.GOOS, runtime.GOARCH)
	fmt.Printf("temp dir: %s\n", os.TempDir()) //TODO-
	if s := os.Getenv("CCGO_CPP"); s != "" {
		fmt.Printf("CCGO_CPP=%s\n", os.Getenv("CCGO_CPP"))
	}

	flag.BoolVar(&oTraceW, "trcw", false, "Print generator writes")
	flag.BoolVar(&oTraceG, "trcg", false, "Print generator output")
	flag.Parse()
	var err error
	if testWD, err = os.Getwd(); err != nil {
		panic("Cannot determine working dir: " + err.Error())
	}

	if *oDownload {
		download()
	}
}

func download() {
	tmp, err := ioutil.TempDir("", "")
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		return
	}

	defer os.RemoveAll(tmp)

	for _, v := range downloads {
		dir := filepath.FromSlash(v.dir)
		root := filepath.Dir(v.dir)
		fi, err := os.Stat(dir)
		switch {
		case err == nil:
			if !fi.IsDir() {
				fmt.Fprintf(os.Stderr, "expected %s to be a directory\n", dir)
			}
			continue
		default:
			if !os.IsNotExist(err) {
				fmt.Fprintf(os.Stderr, "%s", err)
				continue
			}

			if v.dev && !*oDev {
				fmt.Printf("Not downloading (no -dev) %v MB from %s\n", float64(v.sz)/1000, v.url)
				continue
			}

		}

		if err := func() error {
			fmt.Printf("Downloading %v MB from %s\n", float64(v.sz)/1000, v.url)
			resp, err := http.Get(v.url)
			if err != nil {
				return err
			}

			defer resp.Body.Close()

			base := filepath.Base(v.url)
			name := filepath.Join(tmp, base)
			f, err := os.Create(name)
			if err != nil {
				return err
			}

			defer os.Remove(name)

			n, err := io.Copy(f, resp.Body)
			if err != nil {
				return err
			}

			if _, err := f.Seek(0, io.SeekStart); err != nil {
				return err
			}

			switch {
			case strings.HasSuffix(base, ".tar.bz2"):
				b2r := bzip2.NewReader(bufio.NewReader(f))
				tr := tar.NewReader(b2r)
				for {
					hdr, err := tr.Next()
					if err != nil {
						if err != io.EOF {
							return err
						}

						return nil
					}

					switch hdr.Typeflag {
					case tar.TypeDir:
						if err = os.MkdirAll(filepath.Join(root, hdr.Name), 0770); err != nil {
							return err
						}
					case tar.TypeReg, tar.TypeRegA:
						f, err := os.OpenFile(filepath.Join(root, hdr.Name), os.O_CREATE|os.O_WRONLY, os.FileMode(hdr.Mode))
						if err != nil {
							return err
						}

						w := bufio.NewWriter(f)
						if _, err = io.Copy(w, tr); err != nil {
							return err
						}

						if err := w.Flush(); err != nil {
							return err
						}

						if err := f.Close(); err != nil {
							return err
						}
					default:
						return fmt.Errorf("unexpected tar header typeflag %#02x", hdr.Typeflag)
					}
				}
			case strings.HasSuffix(base, ".tar.gz"):
				return untar(root, bufio.NewReader(f))
			case strings.HasSuffix(base, ".zip"):
				r, err := zip.NewReader(f, n)
				if err != nil {
					return err
				}

				for _, f := range r.File {
					fi := f.FileInfo()
					if fi.IsDir() {
						if err := os.MkdirAll(filepath.Join(root, f.Name), 0770); err != nil {
							return err
						}

						continue
					}

					if err := func() error {
						rc, err := f.Open()
						if err != nil {
							return err
						}

						defer rc.Close()

						dname := filepath.Join(root, f.Name)
						g, err := os.Create(dname)
						if err != nil {
							return err
						}

						defer g.Close()

						n, err = io.Copy(g, rc)
						return err
					}(); err != nil {
						return err
					}
				}
				return nil
			}
			panic("internal error") //TODOOK
		}(); err != nil {
			fmt.Fprintln(os.Stderr, err)
		}
	}
}

func untar(root string, r io.Reader) error {
	gr, err := gzip.NewReader(r)
	if err != nil {
		return err
	}

	tr := tar.NewReader(gr)
	for {
		hdr, err := tr.Next()
		if err != nil {
			if err != io.EOF {
				return err
			}

			return nil
		}

		switch hdr.Typeflag {
		case tar.TypeDir:
			if err = os.MkdirAll(filepath.Join(root, hdr.Name), 0770); err != nil {
				return err
			}
		case tar.TypeSymlink, tar.TypeXGlobalHeader:
			// skip
		case tar.TypeReg, tar.TypeRegA:
			dir := filepath.Dir(filepath.Join(root, hdr.Name))
			if _, err := os.Stat(dir); err != nil {
				if !os.IsNotExist(err) {
					return err
				}

				if err = os.MkdirAll(dir, 0770); err != nil {
					return err
				}
			}

			f, err := os.OpenFile(filepath.Join(root, hdr.Name), os.O_CREATE|os.O_WRONLY, os.FileMode(hdr.Mode))
			if err != nil {
				return err
			}

			w := bufio.NewWriter(f)
			if _, err = io.Copy(w, tr); err != nil {
				return err
			}

			if err := w.Flush(); err != nil {
				return err
			}

			if err := f.Close(); err != nil {
				return err
			}
		default:
			return fmt.Errorf("unexpected tar header typeflag %#02x", hdr.Typeflag)
		}
	}
}

type golden struct {
	t *testing.T
	f *os.File
	w *bufio.Writer
}

func newGolden(t *testing.T, fn string) *golden {
	if *oRE != "" {
		return &golden{w: bufio.NewWriter(ioutil.Discard)}
	}

	f, err := os.Create(filepath.FromSlash(fn))
	if err != nil { // Possibly R/O fs in a VM
		base := filepath.Base(filepath.FromSlash(fn))
		f, err = ioutil.TempFile("", base)
		if err != nil {
			t.Fatal(err)
		}

		t.Logf("writing results to %s\n", f.Name())
	}

	w := bufio.NewWriter(f)
	return &golden{t, f, w}
}

func (g *golden) close() {
	if g.f == nil {
		return
	}

	if err := g.w.Flush(); err != nil {
		g.t.Fatal(err)
	}

	if err := g.f.Close(); err != nil {
		g.t.Fatal(err)
	}
}

func h(v interface{}) string {
	switch x := v.(type) {
	case int:
		return humanize.Comma(int64(x))
	case int64:
		return humanize.Comma(x)
	case uint64:
		return humanize.Comma(int64(x))
	case float64:
		return humanize.CommafWithDigits(x, 0)
	default:
		panic(fmt.Errorf("%T", x)) //TODOOK
	}
}

func TestTCC(t *testing.T) {
	root := filepath.Join(testWD, filepath.FromSlash(tccDir))
	if _, err := os.Stat(root); err != nil {
		t.Fatalf("Missing resources in %s. Please run 'go test -download' to fix.", root)
	}

	g := newGolden(t, fmt.Sprintf("testdata/tcc_%s_%s.golden", runtime.GOOS, runtime.GOARCH))

	defer g.close()

	var files, ok int
	const dir = "tests/tests2"
	f, o := testTCCExec(g.w, t, filepath.Join(root, filepath.FromSlash(dir)))
	files += f
	ok += o
	t.Logf("files %s, ok %s", h(files), h(ok))
}

func testTCCExec(w io.Writer, t *testing.T, dir string) (files, ok int) {
	const main = "main.go"
	blacklist := map[string]struct{}{
		"34_array_assignment.c":       {}, // gcc: 16:6: error: assignment to expression with array type
		"60_errors_and_warnings.c":    {}, // Not a standalone test. gcc fails.
		"76_dollars_in_identifiers.c": {}, // `int $ = 10;` etc.
		"77_push_pop_macro.c":         {}, //
		"81_types.c":                  {}, // invalid function type cast
		"86_memory-model.c":           {},
		"93_integer_promotion.c":      {}, // The expected output does not agree with gcc.
		"95_bitfields.c":              {}, // Included from 95_bitfields_ms.c
		"95_bitfields_ms.c":           {}, // The expected output does not agree with gcc.
		"96_nodata_wanted.c":          {}, // Not a standalone test. gcc fails.
		"99_fastcall.c":               {}, // 386 only

		"40_stdio.c":                {}, //TODO
		"42_function_pointer.c":     {}, //TODO
		"46_grep.c":                 {}, //TODO
		"73_arm64.c":                {}, //TODO struct varargs
		"75_array_in_struct_init.c": {}, //TODO flat struct initializer
		"78_vla_label.c":            {}, //TODO VLA
		"79_vla_continue.c":         {}, //TODO VLA
		"80_flexarray.c":            {}, //TODO Flexible array member
		"85_asm-outside-function.c": {}, //TODO
		"87_dead_code.c":            {}, //TODO expression statement
		"88_codeopt.c":              {}, //TODO expression statement
		"89_nocode_wanted.c":        {}, //TODO expression statement
		"90_struct-init.c":          {}, //TODO cc ... in designator
		"92_enum_bitfield.c":        {}, //TODO bit fields
		"94_generic.c":              {}, //TODO cc _Generic
		"98_al_ax_extend.c":         {}, //TODO
	}
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(wd)

	temp, err := ioutil.TempDir("", "ccgo-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(temp)

	if err := os.Chdir(temp); err != nil {
		t.Fatal(err)
	}

	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	if err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				err = nil
			}
			return err
		}

		if info.IsDir() {
			return skipDir(path)
		}

		if filepath.Ext(path) != ".c" || info.Mode()&os.ModeType != 0 {
			return nil
		}

		if _, ok := blacklist[filepath.Base(path)]; ok {
			return nil
		}

		files++

		if re != nil && !re.MatchString(path) {
			return nil
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, files, ok, path)
		}

		if err := os.Remove(main); err != nil && !os.IsNotExist(err) {
			return err
		}

		ccgoArgs := []string{
			"ccgo",
			"-o", main,
			"-ccgo-all-errors",
			"-ccgo-long-double-is-double",
			"-ccgo-verify-structs",
		}
		var args []string
		switch base := filepath.Base(path); base {
		case "31_args.c":
			args = []string{"arg1", "arg2", "arg3", "arg4", "arg5"}
		case "46_grep.c":
			if err := copyFile(path, filepath.Join(temp, base)); err != nil {
				return err
			}

			args = []string{`[^* ]*[:a:d: ]+\:\*-/: $`, base}
		}
		if !func() (r bool) {
			defer func() {
				if err := recover(); err != nil {
					if *oStackTrace {
						fmt.Printf("%s\n", stack())
					}
					if *oTrace {
						fmt.Println(err)
					}
					t.Errorf("%s: %v", path, err)
					r = false
				}
			}()

			ccgoArgs = append(ccgoArgs, path)
			if err := newTask(ccgoArgs, nil, nil).main(); err != nil {
				if *oTrace {
					fmt.Println(err)
				}
				t.Errorf("%s: %v", path, err)
				return false
			}

			return true
		}() {
			return nil
		}

		out, err := exec.Command("go", append([]string{"run", main}, args...)...).CombinedOutput()
		if err != nil {
			if *oTrace {
				fmt.Println(err)
			}
			b, _ := ioutil.ReadFile(main)
			t.Errorf("\n%s\n%v: %s\n%v", b, path, out, err)
			return nil
		}

		if *oTraceF {
			b, _ := ioutil.ReadFile(main)
			fmt.Printf("\n----\n%s\n----\n", b)
		}
		if *oTraceO {
			fmt.Printf("%s\n", out)
		}
		exp, err := ioutil.ReadFile(noExt(path) + ".expect")
		if err != nil {
			if os.IsNotExist(err) {
				fmt.Fprintln(w, filepath.Base(path))
				ok++
				return nil
			}

			return err
		}

		// trc("out\n%s\nexp\n%s", hex.Dump(out), hex.Dump(exp))
		out = trim(out)
		exp = trim(exp)
		// trc("out\n%s\nexp\n%s", hex.Dump(out), hex.Dump(exp))

		switch base := filepath.Base(path); base {
		case "70_floating_point_literals.c": //TODO TCC binary extension
			a := strings.Split(string(exp), "\n")
			exp = []byte(strings.Join(a[:35], "\n"))
		}

		if !bytes.Equal(out, exp) {
			if *oTrace {
				fmt.Println(err)
			}
			t.Errorf("%v: out\n%s\nexp\n%s\nout\n%s\nexp\n%s", path, out, exp, hex.Dump(out), hex.Dump(exp))
			return nil
		}

		fmt.Fprintln(w, filepath.Base(path))
		ok++
		return nil
	}); err != nil {
		t.Errorf("%v", err)
	}
	return files, ok
}

func trim(b []byte) (r []byte) {
	b = bytes.ReplaceAll(b, []byte{'\r'}, nil)
	b = bytes.TrimLeft(b, "\n")
	b = bytes.TrimRight(b, "\n")
	a := bytes.Split(b, []byte("\n"))
	for i, v := range a {
		a[i] = bytes.TrimRight(v, " ")
	}
	return bytes.Join(a, []byte("\n"))
}

func noExt(s string) string {
	ext := filepath.Ext(s)
	if ext == "" {
		panic("internal error") //TODOOK
	}
	return s[:len(s)-len(ext)]
}

func copyFile(src, dst string) error {
	b, err := ioutil.ReadFile(src)
	if err != nil {
		return err
	}

	return ioutil.WriteFile(dst, b, 0660)
}

func skipDir(path string) error {
	sp := filepath.ToSlash(path)
	if strings.Contains(sp, "/.") {
		return filepath.SkipDir
	}

	return nil
}

func TestCAPI(t *testing.T) {
	task := newTask(nil, nil, nil)
	pkgName, capi, err := task.capi("modernc.org/libc")
	if err != nil {
		t.Fatal(err)
	}

	if _, ok := capi["printf"]; !ok {
		t.Fatal("default libc does not export printf")
	}

	t.Log(pkgName, capi)
}

const text = "abcd\nefgh\x00ijkl"

var (
	text1 = text
	text2 = (*reflect.StringHeader)(unsafe.Pointer(&text1)).Data
)

func TestText(t *testing.T) {
	p := text2
	var b []byte
	for i := 0; i < len(text); i++ {
		b = append(b, *(*byte)(unsafe.Pointer(p)))
		p++
	}
	if g, e := string(b), text; g != e {
		t.Fatalf("%q %q", g, e)
	}
}

func TestGCCExec(t *testing.T) {
	root := filepath.Join(testWD, filepath.FromSlash(gccDir))
	if _, err := os.Stat(root); err != nil {
		t.Fatalf("Missing resources in %s. Please run 'go test -download -dev' to fix.", root)
	}

	g := newGolden(t, fmt.Sprintf("testdata/gcc_exec_%s_%s.golden", runtime.GOOS, runtime.GOARCH))

	defer g.close()

	var files, ok int
	const dir = "gcc/testsuite/gcc.c-torture/execute"
	f, o := testGCCExec(g.w, t, filepath.Join(root, filepath.FromSlash(dir)), false)
	files += f
	ok += o
	t.Logf("files %s, ok %s", h(files), h(ok))
}

func testGCCExec(w io.Writer, t *testing.T, dir string, opt bool) (files, ok int) {
	const main = "main.go"
	blacklist := map[string]struct{}{
		"20000822-1.c": {}, // nested func
		"20001009-2.c": {}, // asm
		"20010122-1.c": {}, // __builtin_return_address
		"20010904-1.c": {}, // __attribute__((aligned(32)))
		"20010904-2.c": {}, // __attribute__((aligned(32)))
		"20021127-1.c": {}, // gcc specific optimization
		"20030323-1.c": {}, // __builtin_return_address
		"20101011-1.c": {}, // sigfpe
		"960830-1.c":   {}, // assembler statements not supported
		"991014-1.c":   {}, // Struct type too big
		"align-3.c":    {}, // __attribute__((aligned(256)))
		"eeprof-1.c":   {}, // requires instrumentation
		"fp-cmp-1.c":   {}, // sigfpe
		"fp-cmp-2.c":   {}, // sigfpe
		"fp-cmp-3.c":   {}, // sigfpe
		"pr15296.c":    {}, // union initializer designates non-first field (gcc extension)
		"rbug.c":       {}, // cannot pass on 386

		"20000113-1.c":    {}, //TODO non-const bitfield initializer
		"20000703-1.c":    {}, //TODO statement expression
		"20000722-1.c":    {}, //TODO composite literal
		"20000801-3.c":    {}, //TODO designators
		"20000917-1.c":    {}, //TODO composite literal
		"20001203-2.c":    {}, //TODO statement expression
		"20010123-1.c":    {}, //TODO composite literal
		"20030714-1.c":    {}, //TODO select nested field
		"20040411-1.c":    {}, //TODO VLA
		"20040423-1.c":    {}, //TODO VLA
		"20050613-1.c":    {}, //TODO nested initailizer designator
		"anon-1.c":        {}, //TODO nested field access
		"pr41317.c":       {}, //TODO nested field access
		"pr41463.c":       {}, //TODO link error (report bug?)
		"pr42570":         {}, //TODO uint8_t foo[1][0];
		"pr88739.c":       {}, //TODO nested initailizer designator
		"pushpop_macro.c": {}, //TODO #pragma push_macro("_")
	}
	if runtime.GOOS == "windows" && runtime.GOARCH == "amd64" {
		blacklist["pr36339.c"] = struct{}{} // typedef unsigned long my_uintptr_t;
	}
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(wd)

	temp, err := ioutil.TempDir("", "ccgo-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(temp)

	if err := os.Chdir(temp); err != nil {
		t.Fatal(err)
	}

	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	if err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				err = nil
			}
			return err
		}

		if info.IsDir() {
			return skipDir(path)
		}

		if strings.Contains(filepath.ToSlash(path), "/builtins/") {
			return nil
		}

		if filepath.Ext(path) != ".c" || info.Mode()&os.ModeType != 0 {
			return nil
		}

		if _, ok := blacklist[filepath.Base(path)]; ok {
			return nil
		}

		files++

		if re != nil && !re.MatchString(path) {
			return nil
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, files, ok, path)
		}

		if err := os.Remove(main); err != nil && !os.IsNotExist(err) {
			return err
		}

		ccgoArgs := []string{
			"ccgo",
			"-D__FUNCTION__=__func__",
			"-o", main,
			"-ccgo-verify-structs",
		}
		if !func() (r bool) {
			defer func() {
				if err := recover(); err != nil {
					if *oStackTrace {
						fmt.Printf("%s\n", stack())
					}
					if *oTrace {
						fmt.Println(err)
					}
					t.Errorf("%s: %v", path, err)
					r = false
				}
			}()

			ccgoArgs = append(ccgoArgs, path, "-ccgo-long-double-is-double")
			if err := newTask(ccgoArgs, nil, nil).main(); err != nil {
				if *oTrace {
					fmt.Println(err)
				}
				t.Errorf("%s: %v", path, err)
				return false
			}

			return true
		}() {
			return nil
		}

		out, err := exec.Command("go", "run", main).CombinedOutput()
		if err != nil {
			if *oTrace {
				fmt.Println(err)
			}
			b, _ := ioutil.ReadFile(main)
			t.Errorf("\n%s\n%v: %s\n%v", b, path, out, err)
			return nil
		}

		if *oTraceF {
			b, _ := ioutil.ReadFile(main)
			fmt.Printf("\n----\n%s\n----\n", b)
		}
		if *oTraceO {
			fmt.Printf("%s\n", out)
		}
		exp, err := ioutil.ReadFile(noExt(path) + ".expect")
		if err != nil {
			if os.IsNotExist(err) {
				fmt.Fprintln(w, filepath.Base(path))
				ok++
				return nil
			}

			return err
		}

		out = trim(out)
		exp = trim(exp)

		if !bytes.Equal(out, exp) {
			if *oTrace {
				fmt.Println(err)
			}
			t.Errorf("%v: out\n%s\nexp\n%s", path, out, exp)
			return nil
		}

		fmt.Fprintln(w, filepath.Base(path))
		ok++
		return nil
	}); err != nil {
		t.Errorf("%v", err)
	}
	return files, ok
}

func TestSQLite(t *testing.T) {
	root := filepath.Join(testWD, filepath.FromSlash(sqliteDir))
	if _, err := os.Stat(root); err != nil {
		t.Fatalf("Missing resources in %s. Please run 'go test -download' to fix.", root)
	}

	testSQLite(t, root)
}

func testSQLite(t *testing.T, dir string) {
	const main = "main.go"
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(wd)

	temp, err := ioutil.TempDir("", "ccgo-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(temp)

	if err := os.Chdir(temp); err != nil {
		t.Fatal(err)
	}

	ccgoArgs := []string{
		"ccgo",
		"-DHAVE_USLEEP",
		"-DLONGDOUBLE_TYPE=double",
		"-DSQLITE_DEBUG",
		"-DSQLITE_DEFAULT_MEMSTATUS=0",
		"-DSQLITE_ENABLE_DBPAGE_VTAB",
		"-DSQLITE_LIKE_DOESNT_MATCH_BLOBS",
		"-DSQLITE_MEMDEBUG",
		"-DSQLITE_THREADSAFE=0",
		"-ccgo-all-errors",
		"-ccgo-long-double-is-double", // stddef.h
		"-ccgo-verify-structs",
		"-o", main,
		filepath.Join(dir, "shell.c"),
		filepath.Join(dir, "sqlite3.c"),
	}
	if *oDebug {
		ccgoArgs = append(ccgoArgs, "-DSQLITE_DEBUG_OS_TRACE", "-DSQLITE_FORCE_OS_TRACE")
	}
	if !func() (r bool) {
		defer func() {
			if err := recover(); err != nil {
				if *oStackTrace {
					fmt.Printf("%s\n", stack())
				}
				if *oTrace {
					fmt.Println(err)
				}
				t.Errorf("%v", err)
				r = false
			}
			if *oTraceF {
				b, _ := ioutil.ReadFile(main)
				fmt.Printf("\n----\n%s\n----\n", b)
			}
		}()

		if err := newTask(ccgoArgs, nil, nil).main(); err != nil {
			if *oTrace {
				fmt.Println(err)
			}
			t.Errorf("%v", err)
			return false
		}

		return true
	}() {
		return
	}
	shell := "./shell"
	if runtime.GOOS == "windows" {
		shell = "shell.exe"
	}
	args := []string{"build"}
	if s := *oXTags; s != "" {
		args = append(args, "-tags", s)
	}
	args = append(args, "-o", shell, main)
	if out, err := exec.Command("go", args...).CombinedOutput(); err != nil {
		s := strings.TrimSpace(string(out))
		if s != "" {
			s += "\n"
		}
		t.Errorf("%s%v", s, err)
		return
	}

	var out []byte
	switch {
	case *oDebug:
		out, err = exec.Command(shell, "tmp", ".log stdout", "create table t(i); insert into t values(42); select 11*i from t;").CombinedOutput()
	default:
		out, err = exec.Command(shell, "tmp", "create table t(i); insert into t values(42); select 11*i from t;").CombinedOutput()
	}
	if err != nil {
		if *oTrace {
			fmt.Printf("%s\n%s\n", out, err)
		}
		t.Errorf("%s\n%v", out, err)
		return
	}

	if g, e := strings.TrimSpace(string(out)), "462"; g != e {
		t.Errorf("got: %s\nexp: %s", g, e)
	}
	if *oTraceO {
		fmt.Printf("%s\n", out)
	}

	if out, err = exec.Command(shell, "tmp", "select 13*i from t;").CombinedOutput(); err != nil {
		if *oTrace {
			fmt.Printf("%s\n%s\n", out, err)
		}
		t.Errorf("%v", err)
		return
	}

	if g, e := strings.TrimSpace(string(out)), "546"; g != e {
		t.Errorf("got: %s\nexp: %s", g, e)
	}
	if *oTraceO {
		fmt.Printf("%s\n", out)
	}
}

func TestMjson(t *testing.T) {
	root := filepath.Join(testWD, filepath.FromSlash(mjsonDir))
	if _, err := os.Stat(root); err != nil {
		t.Fatalf("Missing resources in %s. Please run 'go test -download' to fix.", root)
	}

	testMjson(t, root)
}

func testMjson(t *testing.T, dir string) {
	const main = "main.go"
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(wd)

	temp, err := ioutil.TempDir("", "ccgo-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(temp)

	if err := os.Chdir(temp); err != nil {
		t.Fatal(err)
	}

	ccgoArgs := []string{
		"ccgo",
		"-o", main,
		"-ccgo-long-double-is-double",
		"-ccgo-verify-structs",
		filepath.Join(dir, "mjson.c"), filepath.Join(dir, "test_microjson.c"),
	}
	if !func() (r bool) {
		defer func() {
			if err := recover(); err != nil {
				if *oStackTrace {
					fmt.Printf("%s\n", stack())
				}
				if *oTrace {
					fmt.Println(err)
				}
				t.Errorf("%v", err)
				r = false
			}
		}()

		if err := newTask(ccgoArgs, nil, nil).main(); err != nil {
			if *oTrace {
				fmt.Println(err)
			}
			t.Errorf("%v", err)
			return false
		}

		return true
	}() {
		return
	}
}

type compCertResult struct {
	compiler string
	test     string
	run      time.Duration
	k        float64

	compileOK bool
	execOK    bool
	resultOK  bool
}

func (r *compCertResult) String() string {
	var s string
	if r.k != 0 {
		s = fmt.Sprintf("%8.3f", r.k)
		if r.k == 1 {
			s += " *"
		}
	}
	return fmt.Sprintf("%10v%15v%10.3fms%6v%6v%6v%s", r.compiler, r.test, float64(r.run)/float64(time.Millisecond), r.compileOK, r.execOK, r.resultOK, s)
}

func TestCompCert(t *testing.T) {
	const root = "testdata/CompCert-3.6/test/c"

	b, err := ioutil.ReadFile(filepath.FromSlash(root + "/Results/knucleotide-input.txt"))
	if err != nil {
		t.Fatal(err)
	}

	dir := filepath.FromSlash(root)
	m, err := filepath.Glob(filepath.Join(dir, "*.c"))
	if err != nil {
		t.Fatal(err)
	}

	for i, v := range m {
		v, err := filepath.Abs(v)
		if err != nil {
			t.Fatal(err)
		}

		m[i] = v
	}

	rdir, err := filepath.Abs(filepath.FromSlash(root + "/Results"))
	if err != nil {
		t.Fatal(err)
	}

	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(wd)

	temp, err := ioutil.TempDir("", "ccgo-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(temp)

	if err := os.Chdir(temp); err != nil {
		t.Fatal(err)
	}

	if err := os.Mkdir("Results", 0770); err != nil {
		t.Fatal(err)
	}

	if err := ioutil.WriteFile(filepath.FromSlash("Results/knucleotide-input.txt"), b, 0660); err != nil {
		t.Fatal(err)
	}

	var r []*compCertResult
	t.Run("gcc", func(t *testing.T) { r = append(r, testCompCertGcc(t, m, 5, rdir)...) })
	t.Run("ccgo", func(t *testing.T) { r = append(r, testCompCertCcgo(t, m, 5, rdir)...) })
	consider := map[string]struct{}{}
	for _, v := range r {
		consider[v.test] = struct{}{}
	}
	for _, v := range r {
		if ok := v.compileOK && v.execOK && v.resultOK; !ok {
			delete(consider, v.test)
		}
	}
	times := map[string]time.Duration{}
	tests := map[string][]*compCertResult{}
	for _, v := range r {
		if _, ok := consider[v.test]; !ok {
			continue
		}

		times[v.compiler] += v.run
		tests[v.test] = append(tests[v.test], v)
	}
	for _, a := range tests {
		if len(a) < 2 {
			continue
		}
		min := time.Duration(-1)
		for _, v := range a {
			if min < 0 || v.run < min {
				min = v.run
			}
		}
		for _, v := range a {
			v.k = float64(v.run) / float64(min)
		}
	}
	t.Log("  compiler           test    T         comp  exec match    coef")
	for _, v := range r {
		t.Log(v)
	}
	var a []string
	for k := range times {
		a = append(a, k)
	}
	sort.Strings(a)
	t.Logf("Considered tests: %d/%d", len(consider), len(m))
	min := time.Duration(-1)
	for _, v := range times {
		if min < 0 || v < min {
			min = v
		}
	}
	for _, k := range a {
		t.Logf("%10s%15v %6.3f", k, times[k], float64(times[k])/float64(min))
	}
}

func testCompCertGcc(t *testing.T, files []string, N int, rdir string) (r []*compCertResult) {
	const nm = "gcc"
	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}
next:
	for _, fn := range files {
		base := filepath.Base(fn)
		if *oTrace {
			fmt.Println(base)
		}
		if re != nil && !re.MatchString(base) {
			continue
		}

		bin := nm + "-" + base + ".out"
		out, err := exec.Command("gcc", "-O", "-o", bin, fn, "-lm").CombinedOutput()
		if err != nil {
			t.Errorf("%s: %s:\n%s", base, err, out)
			r = append(r, &compCertResult{nm, base, 0, 0, false, false, false})
			continue
		}

		t0 := time.Now()
		for i := 0; i < N; i++ {
			if out, err = exec.Command("./" + bin).CombinedOutput(); err != nil {
				t.Errorf("%s: %s:\n%s", base, err, out)
				r = append(r, &compCertResult{nm, base, 0, 0, true, false, false})
				continue next
			}
		}
		d := time.Since(t0) / time.Duration(N)
		if base != "mandelbrot.c" {
			out = bytes.ReplaceAll(out, []byte{'\r'}, nil)
		}
		r = append(r, &compCertResult{nm, base, d, 0, true, true, checkResult(t, out, base, rdir)})
	}
	return r
}

func checkResult(t *testing.T, out []byte, base, rdir string) bool {
	base = base[:len(base)-len(filepath.Ext(base))]
	fn := filepath.Join(rdir, base)
	b, err := ioutil.ReadFile(fn)
	if err != nil {
		t.Errorf("%v: %v", base, err)
		return false
	}

	if bytes.Equal(out, b) {
		return true
	}

	fn2 := fn + "." + runtime.GOOS
	b2, err := ioutil.ReadFile(fn2)
	if err == nil {
		switch {
		case bytes.Equal(out, b2):
			return true
		default:
			t.Logf("got\n%s", hex.Dump(out))
			t.Logf("exp\n%s", hex.Dump(b2))
			t.Errorf("%v: result differs", base)
			return false
		}
	}

	t.Logf("got\n%s", hex.Dump(out))
	t.Logf("exp\n%s", hex.Dump(b))
	t.Errorf("%v: result differs", base)
	return false
}

func testCompCertCcgo(t *testing.T, files []string, N int, rdir string) (r []*compCertResult) {
	const nm = "ccgo"
	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}
next:
	for _, fn := range files {
		base := filepath.Base(fn)
		if *oTrace {
			fmt.Println(base)
		}
		if re != nil && !re.MatchString(base) {
			continue
		}
		src := nm + "-" + base + ".go"
		bin := nm + "-" + base + ".out"
		if err := func() (err error) {
			defer func() {
				if e := recover(); e != nil && err == nil {
					if *oStackTrace {
						fmt.Printf("%s\n", stack())
					}
					err = fmt.Errorf("%v", e)
				}
			}()
			return newTask([]string{
				"ccgo",
				"-o", src,
				fn,
				"-ccgo-long-double-is-double",
			}, nil, nil).main()
		}(); err != nil {
			t.Errorf("%s: %s:", base, err)
			r = append(r, &compCertResult{nm, base, 0, 0, false, false, false})
			continue
		}
		if *oTraceF {
			b, _ := ioutil.ReadFile(src)
			fmt.Printf("\n----\n%s\n----\n", b)
		}

		if out, err := exec.Command("go", "build", "-o", bin, src).CombinedOutput(); err != nil {
			t.Errorf("%s: %s:\n%s", base, err, out)
			r = append(r, &compCertResult{nm, base, 0, 0, false, false, false})
			continue next
		}

		var out []byte
		t0 := time.Now()
		for i := 0; i < N; i++ {
			var err error
			if out, err = exec.Command("./" + bin).CombinedOutput(); err != nil {
				t.Errorf("%s: %s:\n%s", base, err, out)
				r = append(r, &compCertResult{nm, base, 0, 0, true, false, false})
				continue next
			}
		}
		d := time.Since(t0) / time.Duration(N)
		if base != "mandelbrot.c" {
			out = bytes.ReplaceAll(out, []byte{'\r'}, nil)
		}
		r = append(r, &compCertResult{nm, base, d, 0, true, true, checkResult(t, out, base, rdir)})
	}
	return r
}

func TestBug(t *testing.T) {
	g := newGolden(t, fmt.Sprintf("testdata/bug_%s_%s.golden", runtime.GOOS, runtime.GOARCH))

	defer g.close()

	var files, ok int
	const dir = "tests/tests2"
	f, o := testBugExec(g.w, t, filepath.Join(testWD, filepath.FromSlash("testdata/bug")))
	files += f
	ok += o
	t.Logf("files %s, ok %s", h(files), h(ok))
}

func testBugExec(w io.Writer, t *testing.T, dir string) (files, ok int) {
	const main = "main.go"
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(wd)

	temp, err := ioutil.TempDir("", "ccgo-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(temp)

	if err := os.Chdir(temp); err != nil {
		t.Fatal(err)
	}

	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	if err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			if os.IsNotExist(err) {
				err = nil
			}
			return err
		}

		if info.IsDir() {
			return skipDir(path)
		}

		if filepath.Ext(path) != ".c" || info.Mode()&os.ModeType != 0 {
			return nil
		}

		files++

		if re != nil && !re.MatchString(path) {
			return nil
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, files, ok, path)
		}

		if err := os.Remove(main); err != nil && !os.IsNotExist(err) {
			return err
		}

		ccgoArgs := []string{
			"ccgo",
			"-o", main,
			"-ccgo-export-defines", "",
			"-ccgo-verify-structs",
		}
		var args []string
		if !func() (r bool) {
			defer func() {
				if err := recover(); err != nil {
					if *oStackTrace {
						fmt.Printf("%s\n", stack())
					}
					if *oTrace {
						fmt.Println(err)
					}
					t.Errorf("%s: %v", path, err)
					r = false
				}
			}()

			ccgoArgs = append(ccgoArgs, path)
			if err := newTask(ccgoArgs, nil, nil).main(); err != nil {
				if *oTrace {
					fmt.Println(err)
				}
				t.Errorf("%s: %v", path, err)
				return false
			}

			return true
		}() {
			return nil
		}

		out, err := exec.Command("go", append([]string{"run", main}, args...)...).CombinedOutput()
		if err != nil {
			if *oTrace {
				fmt.Println(err)
			}
			b, _ := ioutil.ReadFile(main)
			t.Errorf("\n%s\n%v: %s\n%v", b, path, out, err)
			return nil
		}

		if *oTraceF {
			b, _ := ioutil.ReadFile(main)
			fmt.Printf("\n----\n%s\n----\n", b)
		}
		if *oTraceO {
			fmt.Printf("%s\n", out)
		}
		exp, err := ioutil.ReadFile(noExt(path) + ".expect")
		if err != nil {
			if os.IsNotExist(err) {
				fmt.Fprintln(w, filepath.Base(path))
				ok++
				return nil
			}

			return err
		}

		out = trim(out)
		exp = trim(exp)

		if !bytes.Equal(out, exp) {
			if *oTrace {
				fmt.Println(err)
			}
			t.Errorf("%v: out\n%s\nexp\n%s", path, out, exp)
			return nil
		}

		fmt.Fprintln(w, filepath.Base(path))
		ok++
		return nil
	}); err != nil {
		t.Errorf("%v", err)
	}
	return files, ok
}

func TestCSmith(t *testing.T) {
	gcc, err := exec.LookPath("gcc")
	if err != nil {
		t.Fatalf("%v", err)
		return
	}

	csmith, err := exec.LookPath("csmith")
	if err != nil {
		t.Fatalf("%v", err)
		return
	}
	binaryName := filepath.FromSlash("./a.out")
	mainName := filepath.FromSlash("main.go")
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(wd)

	temp, err := ioutil.TempDir("", "gocc-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(temp)

	if err := os.Chdir(temp); err != nil {
		t.Fatal(err)
	}

	fixedBugs := []string{
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid --max-nested-struct-level 10 -s 1906742816",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid --max-nested-struct-level 10 -s 612971101",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid --max-nested-struct-level 10 -s 3629008936",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 4130344133",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3130410542",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 1833258637",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3126091077",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2205128324",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3043990076",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2517344771",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 56498550",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3645367888",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 169375684",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3578720023",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 1885311141",
		"--no-bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3720922579",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 241244373",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 517639208",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2205128324",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2876930815",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3365074920",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3329111231",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2648215054",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 3919255949",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 890611563",
		"--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 4101947480",
		//TODO "--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 4058772172",
		//TODO "--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2273393378",
	}
	ch := time.After(*oCSmith)
	t0 := time.Now()
	var files, ok int
	var size int64
out:
	for i := 0; ; i++ {
		extra := ""
		var args string
		switch {
		case i < len(fixedBugs):
			args += fixedBugs[i]
			a := strings.Split(fixedBugs[i], " ")
			extra = strings.Join(a[len(a)-2:], " ")
			t.Log(args)
		default:
			select {
			case <-ch:
				break out
			default:
			}

			args += csmithDefaultArgs
		}
		csOut, err := exec.Command(csmith, strings.Split(args, " ")...).Output()
		if err != nil {
			t.Fatalf("%v\n%s", err, csOut)
		}

		if fn := *oBlackBox; fn != "" {
			if err := ioutil.WriteFile(fn, csOut, 0660); err != nil {
				t.Fatal(err)
			}
		}

		if err := ioutil.WriteFile("main.c", csOut, 0660); err != nil {
			t.Fatal(err)
		}

		csp := fmt.Sprintf("-I%s", filepath.FromSlash("/usr/include/csmith"))
		ccOut, err := exec.Command(gcc, "-o", binaryName, "main.c", csp).CombinedOutput()
		if err != nil {
			t.Fatalf("%s\n%s\ncc: %v", extra, ccOut, err)
		}

		binOutA, err := func() ([]byte, error) {
			ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()

			return exec.CommandContext(ctx, binaryName).CombinedOutput()
		}()
		if err != nil {
			continue
		}

		size += int64(len(csOut))

		if err := os.Remove(binaryName); err != nil {
			t.Fatal(err)
		}

		files++
		var stdout, stderr bytes.Buffer
		j := newTask([]string{
			"ccgo",
			"-o", mainName,
			csp,
			"-ccgo-long-double-is-double",
			"-ccgo-verify-structs",
			"main.c",
		}, &stdout, &stderr)

		func() {

			defer func() {
				if err := recover(); err != nil {
					t.Errorf("%s\n%s\nccgo: %s\n%s\n%s", extra, csOut, stdout.Bytes(), stderr.Bytes(), debug.Stack())
					t.Fatal(err)
				}
			}()

			if err := j.main(); err != nil || stdout.Len() != 0 {
				t.Errorf("%s\n%s\nccgo: %s\n%s", extra, csOut, stdout.Bytes(), stderr.Bytes())
				t.Fatal(err)
			}
		}()

		binOutB, err := func() ([]byte, error) {
			ctx, cancel := context.WithTimeout(context.Background(), 100*time.Second)
			defer cancel()

			return exec.CommandContext(ctx, "go", "run", mainName).CombinedOutput()
		}()
		if err != nil {
			t.Errorf("%s\n%s\n%s\nccgo: %v", extra, csOut, binOutB, err)
			break
		}

		if g, e := binOutB, binOutA; !bytes.Equal(g, e) {
			t.Errorf("%s\n%s\nccgo: %v\ngot: %s\nexp: %s", extra, csOut, err, g, e)
			break
		}

		ok++
		if *oTrace {
			fmt.Fprintln(os.Stderr, time.Since(t0), files, ok)
		}

		if err := os.Remove(mainName); err != nil {
			t.Fatal(err)
		}
	}
	d := time.Since(t0)
	t.Logf("files %v, bytes %v, ok %v in %v", h(files), h(size), h(ok), d)
}

func TestMem(t *testing.T) {
	if !*oMem {
		t.Skip("not enabled")
		return
	}

	const args0 = `ccgo
-D__printf__=printf
-ccgo-export-defines

-ccgo-export-enums

-ccgo-export-externs
X
-ccgo-export-fields
F
-ccgo-export-structs

-ccgo-export-typedefs

-ccgo-hide
TclpCreateProcess
-ccgo-long-double-is-double
-ccgo-pkgname
tcl
-ccgo-trace-translation-units
-o
{{0}}
../compat/zlib/adler32.c
../compat/zlib/compress.c
../compat/zlib/crc32.c
../compat/zlib/deflate.c
../compat/zlib/infback.c
../compat/zlib/inffast.c
../compat/zlib/inflate.c
../compat/zlib/inftrees.c
../compat/zlib/trees.c
../compat/zlib/uncompr.c
../compat/zlib/zutil.c
-DBUILD_tcl
-I.
-I{{1}}/testdata/tcl8.6.10/unix
-I{{1}}/testdata/tcl8.6.10/generic
-I{{1}}/testdata/tcl8.6.10/libtommath
-DPACKAGE_NAME="tcl"
-DPACKAGE_TARNAME="tcl"
-DPACKAGE_VERSION="8.6"
-DPACKAGE_STRING="tcl 8.6"
-DPACKAGE_BUGREPORT=""
-DSTDC_HEADERS=1
-DHAVE_SYS_TYPES_H=1
-DHAVE_SYS_STAT_H=1
-DHAVE_STDLIB_H=1
-DHAVE_STRING_H=1
-DHAVE_MEMORY_H=1
-DHAVE_STRINGS_H=1
-DHAVE_INTTYPES_H=1
-DHAVE_STDINT_H=1
-DHAVE_UNISTD_H=1
-DHAVE_SYS_PARAM_H=1
-DTCL_CFGVAL_ENCODING="iso8859-1"
-DHAVE_ZLIB=1
-DMODULE_SCOPE=extern __attribute__((__visibility__("hidden")))
-DHAVE_HIDDEN=1
-DHAVE_CAST_TO_UNION=1
-DTCL_SHLIB_EXT=""
-DNDEBUG=1
-DTCL_CFG_OPTIMIZED=1
-DTCL_TOMMATH=1
-DMP_PREC=4
-D_LARGEFILE64_SOURCE=1
-DTCL_WIDE_INT_IS_LONG=1
-DHAVE_GETCWD=1
-DHAVE_MKSTEMP=1
-DHAVE_OPENDIR=1
-DHAVE_STRTOL=1
-DHAVE_WAITPID=1
-DHAVE_GETNAMEINFO=1
-DHAVE_GETADDRINFO=1
-DHAVE_FREEADDRINFO=1
-DHAVE_GAI_STRERROR=1
-DHAVE_STRUCT_ADDRINFO=1
-DHAVE_STRUCT_IN6_ADDR=1
-DHAVE_STRUCT_SOCKADDR_IN6=1
-DHAVE_STRUCT_SOCKADDR_STORAGE=1
-DHAVE_TERMIOS_H=1
-DHAVE_SYS_IOCTL_H=1
-DHAVE_SYS_TIME_H=1
-DTIME_WITH_SYS_TIME=1
-DHAVE_GMTIME_R=1
-DHAVE_LOCALTIME_R=1
-DHAVE_MKTIME=1
-DHAVE_TM_GMTOFF=1
-DHAVE_TIMEZONE_VAR=1
-DHAVE_STRUCT_STAT_ST_BLOCKS=1
-DHAVE_STRUCT_STAT_ST_BLKSIZE=1
-DHAVE_BLKCNT_T=1
-DHAVE_INTPTR_T=1
-DHAVE_UINTPTR_T=1
-DNO_UNION_WAIT=1
-DHAVE_SIGNED_CHAR=1
-DHAVE_LANGINFO=1
-DHAVE_MKSTEMPS=1
-DHAVE_FTS=1
-DTCL_UNLOAD_DLLS=1
-DSTATIC_BUILD
-DMP_FIXED_CUTOFFS
-DMP_NO_STDINT
-DCFG_INSTALL_LIBDIR="/usr/local/lib"
-DCFG_INSTALL_BINDIR="/usr/local/bin"
-DCFG_INSTALL_SCRDIR="/usr/local/lib/tcl8.6"
-DCFG_INSTALL_INCDIR="/usr/local/include"
-DCFG_INSTALL_DOCDIR="/usr/local/man"
-DCFG_RUNTIME_LIBDIR="/usr/local/lib"
-DCFG_RUNTIME_BINDIR="/usr/local/bin"
-DCFG_RUNTIME_SCRDIR="/usr/local/lib/tcl8.6"
-DCFG_RUNTIME_INCDIR="/usr/local/include"
-DCFG_RUNTIME_DOCDIR="/usr/local/man"
-DTCL_LIBRARY="/usr/local/lib/tcl8.6"
-DTCL_PACKAGE_PATH="/usr/local/lib "
-UHAVE_CAST_TO_UNION
{{1}}/testdata/tcl8.6.10/generic/regcomp.c
{{1}}/testdata/tcl8.6.10/generic/regexec.c
{{1}}/testdata/tcl8.6.10/generic/regfree.c
{{1}}/testdata/tcl8.6.10/generic/regerror.c
{{1}}/testdata/tcl8.6.10/generic/tclAlloc.c
{{1}}/testdata/tcl8.6.10/generic/tclAssembly.c
{{1}}/testdata/tcl8.6.10/generic/tclAsync.c
{{1}}/testdata/tcl8.6.10/generic/tclBasic.c
{{1}}/testdata/tcl8.6.10/generic/tclBinary.c
{{1}}/testdata/tcl8.6.10/generic/tclCkalloc.c
{{1}}/testdata/tcl8.6.10/generic/tclClock.c
{{1}}/testdata/tcl8.6.10/generic/tclCmdAH.c
{{1}}/testdata/tcl8.6.10/generic/tclCmdIL.c
{{1}}/testdata/tcl8.6.10/generic/tclCmdMZ.c
{{1}}/testdata/tcl8.6.10/generic/tclCompCmds.c
{{1}}/testdata/tcl8.6.10/generic/tclCompCmdsGR.c
{{1}}/testdata/tcl8.6.10/generic/tclCompCmdsSZ.c
{{1}}/testdata/tcl8.6.10/generic/tclCompExpr.c
{{1}}/testdata/tcl8.6.10/generic/tclCompile.c
{{1}}/testdata/tcl8.6.10/generic/tclConfig.c
{{1}}/testdata/tcl8.6.10/generic/tclDate.c
{{1}}/testdata/tcl8.6.10/generic/tclDictObj.c
{{1}}/testdata/tcl8.6.10/generic/tclDisassemble.c
{{1}}/testdata/tcl8.6.10/generic/tclEncoding.c
{{1}}/testdata/tcl8.6.10/generic/tclEnsemble.c
{{1}}/testdata/tcl8.6.10/generic/tclEnv.c
{{1}}/testdata/tcl8.6.10/generic/tclEvent.c
{{1}}/testdata/tcl8.6.10/generic/tclExecute.c
{{1}}/testdata/tcl8.6.10/generic/tclFCmd.c
{{1}}/testdata/tcl8.6.10/generic/tclFileName.c
{{1}}/testdata/tcl8.6.10/generic/tclGet.c
{{1}}/testdata/tcl8.6.10/generic/tclHash.c
{{1}}/testdata/tcl8.6.10/generic/tclHistory.c
{{1}}/testdata/tcl8.6.10/generic/tclIndexObj.c
{{1}}/testdata/tcl8.6.10/generic/tclInterp.c
{{1}}/testdata/tcl8.6.10/generic/tclIO.c
{{1}}/testdata/tcl8.6.10/generic/tclIOCmd.c
{{1}}/testdata/tcl8.6.10/generic/tclIORChan.c
{{1}}/testdata/tcl8.6.10/generic/tclIORTrans.c
{{1}}/testdata/tcl8.6.10/generic/tclIOGT.c
{{1}}/testdata/tcl8.6.10/generic/tclIOSock.c
{{1}}/testdata/tcl8.6.10/generic/tclIOUtil.c
{{1}}/testdata/tcl8.6.10/generic/tclLink.c
{{1}}/testdata/tcl8.6.10/generic/tclListObj.c
{{1}}/testdata/tcl8.6.10/generic/tclLiteral.c
{{1}}/testdata/tcl8.6.10/generic/tclLoad.c
{{1}}/testdata/tcl8.6.10/generic/tclMain.c
{{1}}/testdata/tcl8.6.10/generic/tclNamesp.c
{{1}}/testdata/tcl8.6.10/generic/tclNotify.c
{{1}}/testdata/tcl8.6.10/generic/tclObj.c
{{1}}/testdata/tcl8.6.10/generic/tclOptimize.c
{{1}}/testdata/tcl8.6.10/generic/tclPanic.c
{{1}}/testdata/tcl8.6.10/generic/tclParse.c
{{1}}/testdata/tcl8.6.10/generic/tclPathObj.c
{{1}}/testdata/tcl8.6.10/generic/tclPipe.c
{{1}}/testdata/tcl8.6.10/generic/tclPkg.c
{{1}}/testdata/tcl8.6.10/generic/tclPkgConfig.c
{{1}}/testdata/tcl8.6.10/generic/tclPosixStr.c
{{1}}/testdata/tcl8.6.10/generic/tclPreserve.c
{{1}}/testdata/tcl8.6.10/generic/tclProc.c
{{1}}/testdata/tcl8.6.10/generic/tclRegexp.c
{{1}}/testdata/tcl8.6.10/generic/tclResolve.c
{{1}}/testdata/tcl8.6.10/generic/tclResult.c
{{1}}/testdata/tcl8.6.10/generic/tclScan.c
{{1}}/testdata/tcl8.6.10/generic/tclStringObj.c
{{1}}/testdata/tcl8.6.10/generic/tclStrToD.c
{{1}}/testdata/tcl8.6.10/generic/tclThread.c
{{1}}/testdata/tcl8.6.10/generic/tclThreadAlloc.c
{{1}}/testdata/tcl8.6.10/generic/tclThreadJoin.c
{{1}}/testdata/tcl8.6.10/generic/tclThreadStorage.c
{{1}}/testdata/tcl8.6.10/generic/tclStubInit.c
{{1}}/testdata/tcl8.6.10/generic/tclTimer.c
{{1}}/testdata/tcl8.6.10/generic/tclTrace.c
{{1}}/testdata/tcl8.6.10/generic/tclUtf.c
{{1}}/testdata/tcl8.6.10/generic/tclUtil.c
{{1}}/testdata/tcl8.6.10/generic/tclVar.c
{{1}}/testdata/tcl8.6.10/generic/tclZlib.c
{{1}}/testdata/tcl8.6.10/generic/tclTomMathInterface.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixChan.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixEvent.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixFCmd.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixFile.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixPipe.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixSock.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixTime.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixInit.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixThrd.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixCompat.c
{{1}}/testdata/tcl8.6.10/unix/tclUnixNotfy.c
{{1}}/testdata/tcl8.6.10/generic/tclOO.c
{{1}}/testdata/tcl8.6.10/generic/tclOOBasic.c
{{1}}/testdata/tcl8.6.10/generic/tclOOCall.c
{{1}}/testdata/tcl8.6.10/generic/tclOODefineCmds.c
{{1}}/testdata/tcl8.6.10/generic/tclOOInfo.c
{{1}}/testdata/tcl8.6.10/generic/tclOOMethod.c
{{1}}/testdata/tcl8.6.10/generic/tclOOStubInit.c
{{1}}/testdata/tcl8.6.10/generic/tclLoadNone.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_reverse.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_mul_digs_fast.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_sqr_fast.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_add.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_and.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_add_d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_clamp.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_clear.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_clear_multi.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_cmp.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_cmp_d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_cmp_mag.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_cnt_lsb.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_copy.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_count_bits.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_div.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_div_d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_div_2.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_div_2d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_div_3.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_exch.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_expt_u32.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_grow.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_init.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_init_copy.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_init_multi.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_init_set.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_init_size.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_karatsuba_mul.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_karatsuba_sqr.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_balance_mul.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_lshd.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_mod.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_mod_2d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_mul.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_mul_2.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_mul_2d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_mul_d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_neg.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_or.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_radix_size.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_radix_smap.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_read_radix.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_rshd.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_set.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_shrink.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_sqr.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_sqrt.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_sub.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_sub_d.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_signed_rsh.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_to_ubin.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_toom_mul.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_toom_sqr.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_to_radix.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_ubin_size.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_xor.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_mp_zero.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_add.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_mul_digs.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_sqr.c
{{1}}/testdata/tcl8.6.10/libtommath/bn_s_mp_sub.c`

	dir, err := ioutil.TempDir("", "tcl-test-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(dir)

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	defer os.Chdir(cwd)

	out := filepath.Join(dir, "x.go")
	s := strings.Replace(args0, "{{0}}", out, 1)
	s = strings.ReplaceAll(s, "{{1}}", cwd)
	args := strings.Split(s, "\n")

	if err := os.Chdir(filepath.FromSlash("testdata/tcl8.6.10/unix")); err != nil {
		t.Fatal(err)
	}

	if err := newTask(args, nil, nil).main(); err != nil {
		t.Fatalf("%s\n", err)
	}
}

func newCmd(stdout, stderr io.Writer, bin string, args ...string) *exec.Cmd {
	r := exec.Command(bin, args...)
	r.Stdout = multiWriter(os.Stdout, stdout)
	r.Stderr = multiWriter(os.Stderr, stderr)
	return r
}

func multiWriter(w ...io.Writer) io.Writer {
	var a []io.Writer
	for _, v := range w {
		if v != nil {
			a = append(a, v)
		}
	}
	switch len(a) {
	case 0:
		panic("internal error")
	case 1:
		return a[0]
	default:
		return io.MultiWriter(a...)
	}
}
