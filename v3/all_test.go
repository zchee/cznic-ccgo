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
	oDev        = flag.Bool("dev", false, "Enable developer tests/downloads.")
	oDownload   = flag.Bool("download", false, "Download missing testdata. Add -dev to download also 100+ MB of developer resources.")
	oRE         = flag.String("re", "", "")
	oStackTrace = flag.Bool("trcstack", false, "")
	oTrace      = flag.Bool("trc", false, "Print tested paths.")
	oTraceF     = flag.Bool("trcf", false, "Print test file content")
	oTraceO     = flag.Bool("trco", false, "Print test output")

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
	fmt.Printf("CCGO_CPP=%q\n", os.Getenv("CCGO_CPP"))

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

		ccgoArgs := []string{"ccgo", "-o", main, "-ccgo-verify-structs", "-ccgo-long-double-is-double"}
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

		out = trim(out)
		exp = trim(exp)

		switch base := filepath.Base(path); base {
		case "70_floating_point_literals.c": //TODO TCC binary extension
			a := strings.Split(string(exp), "\n")
			exp = []byte(strings.Join(a[:35], "\n"))
		}

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

func trim(b []byte) (r []byte) {
	b = bytes.TrimLeft(b, "\n")
	b = bytes.TrimRight(b, "\n")
	a := bytes.Split(b, []byte("\n"))
	for i, v := range a {
		a[i] = bytes.TrimSpace(v)
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
		"-DSQLITE_DEFAULT_MEMSTATUS=0",
		"-DSQLITE_ENABLE_DBPAGE_VTAB",
		"-DSQLITE_LIKE_DOESNT_MATCH_BLOBS",
		"-DSQLITE_THREADSAFE=0",
		"-ccgo-long-double-is-double", // stddef.h
		"-ccgo-verify-structs",
		"-o", main,
		filepath.Join(dir, "shell.c"),
		filepath.Join(dir, "sqlite3.c"),
	}
	if runtime.GOOS == "windows" { // mingw specials or TODOs
		ccgoArgs = append(
			ccgoArgs,
			"-ccgo-hide", "__debugbreak",
			"-ccgo-hide", "_abs64",
			"-ccgo-hide", "wcsnlen_s",
			"-ccgo-hide", "localtime_s",
			"-ccgo-hide", "gmtime_s",
			"-ccgo-hide", "ctime_s",
			"-ccgo-hide", "daylight",
			"-ccgo-hide", "timezone",
			"-ccgo-hide", "tzname",
			"-ccgo-hide", "__faststorefence",
			"-ccgo-hide", "__stosq",
			"-ccgo-hide", "_interlockedbittestandset64",
			"-ccgo-hide", "_interlockedbittestandreset64",
			"-ccgo-hide", "_interlockedbittestandcomplement64",
			"-ccgo-hide", "InterlockedBitTestAndSet64",
			"-ccgo-hide", "InterlockedBitTestAndReset64",
			"-ccgo-hide", "InterlockedBitTestAndComplement64",
			"-ccgo-hide", "_InterlockedAnd64",
			"-ccgo-hide", "_InterlockedOr64",
			"-ccgo-hide", "_InterlockedXor64",
			"-ccgo-hide", "_InterlockedIncrement64",
			"-ccgo-hide", "_InterlockedDecrement64",
			"-ccgo-hide", "_InterlockedExchange64",
			"-ccgo-hide", "_InterlockedExchangeAdd64",
			"-ccgo-hide", "__readgsbyte",
			"-ccgo-hide", "__readgsword",
			"-ccgo-hide", "__readgsdword",
			"-ccgo-hide", "__readgsqword",
			"-ccgo-hide", "__writegsbyte",
			"-ccgo-hide", "__writegsword",
			"-ccgo-hide", "__writegsdword",
			"-ccgo-hide", "__writegsqword",
			"-ccgo-hide", "_BitScanForward64",
			"-ccgo-hide", "_BitScanReverse64",
			"-ccgo-hide", "_bittest64",
			"-ccgo-hide", "_bittestandset64",
			"-ccgo-hide", "_bittestandreset64",
			"-ccgo-hide", "_bittestandcomplement64",
			"-ccgo-hide", "__movsq",
			"-ccgo-hide", "_InterlockedAnd",
			"-ccgo-hide", "_InterlockedOr",
			"-ccgo-hide", "_InterlockedXor",
			"-ccgo-hide", "_InterlockedIncrement16",
			"-ccgo-hide", "_InterlockedDecrement16",
			"-ccgo-hide", "_InterlockedCompareExchange16",
			"-ccgo-hide", "_InterlockedExchangeAdd",
			"-ccgo-hide", "_InterlockedCompareExchange",
			"-ccgo-hide", "_InterlockedIncrement",
			"-ccgo-hide", "_InterlockedDecrement",
			"-ccgo-hide", "_InterlockedAdd",
			"-ccgo-hide", "_InterlockedAdd64",
			"-ccgo-hide", "_InterlockedExchange",
			"-ccgo-hide", "_InterlockedCompareExchange64",
			"-ccgo-hide", "_InterlockedCompareExchangePointer",
			"-ccgo-hide", "_InterlockedExchangePointer",
			"-ccgo-hide", "__int2c",
			"-ccgo-hide", "__stosb",
			"-ccgo-hide", "__stosw",
			"-ccgo-hide", "__stosd",
			"-ccgo-hide", "_interlockedbittestandset",
			"-ccgo-hide", "_interlockedbittestandreset",
			"-ccgo-hide", "_interlockedbittestandcomplement",
			"-ccgo-hide", "InterlockedBitTestAndSet",
			"-ccgo-hide", "InterlockedBitTestAndReset",
			"-ccgo-hide", "InterlockedBitTestAndComplement",
			"-ccgo-hide", "_BitScanForward",
			"-ccgo-hide", "_BitScanReverse",
			"-ccgo-hide", "_bittest",
			"-ccgo-hide", "_bittestandset",
			"-ccgo-hide", "_bittestandreset",
			"-ccgo-hide", "GetCurrentFiber",
			"-ccgo-hide", "GetFiberData",
			"-ccgo-hide", "NtCurrentTeb",
			"-ccgo-hide", "TpDestroyCallbackEnviron",
			"-ccgo-hide", "TpInitializeCallbackEnviron",
			"-ccgo-hide", "TpSetCallbackActivationContext",
			"-ccgo-hide", "TpSetCallbackCleanupGroup",
			"-ccgo-hide", "TpSetCallbackFinalizationCallback",
			"-ccgo-hide", "TpSetCallbackLongFunction",
			"-ccgo-hide", "TpSetCallbackNoActivationContext",
			"-ccgo-hide", "TpSetCallbackPersistent",
			"-ccgo-hide", "TpSetCallbackRaceWithDll",
			"-ccgo-hide", "TpSetCallbackThreadpool",
			"-ccgo-hide", "_MM_GET_EXCEPTION_MASK",
			"-ccgo-hide", "_MM_GET_EXCEPTION_STATE",
			"-ccgo-hide", "_MM_GET_FLUSH_ZERO_MODE",
			"-ccgo-hide", "_MM_GET_ROUNDING_MODE",
			"-ccgo-hide", "_MM_SET_EXCEPTION_MASK",
			"-ccgo-hide", "_MM_SET_EXCEPTION_STATE",
			"-ccgo-hide", "_MM_SET_FLUSH_ZERO_MODE",
			"-ccgo-hide", "_MM_SET_ROUNDING_MODE",
			"-ccgo-hide", "__andn_u32",
			"-ccgo-hide", "__andn_u64",
			"-ccgo-hide", "__bextr_u32",
			"-ccgo-hide", "__bextr_u64",
			"-ccgo-hide", "__blcfill_u32",
			"-ccgo-hide", "__blcfill_u64",
			"-ccgo-hide", "__blci_u32",
			"-ccgo-hide", "__blci_u64",
			"-ccgo-hide", "__blcic_u32",
			"-ccgo-hide", "__blcic_u64",
			"-ccgo-hide", "__blcmsk_u32",
			"-ccgo-hide", "__blcmsk_u64",
			"-ccgo-hide", "__blcs_u32",
			"-ccgo-hide", "__blcs_u64",
			"-ccgo-hide", "__blsfill_u32",
			"-ccgo-hide", "__blsfill_u64",
			"-ccgo-hide", "__blsi_u32",
			"-ccgo-hide", "__blsi_u64",
			"-ccgo-hide", "__blsic_u32",
			"-ccgo-hide", "__blsic_u64",
			"-ccgo-hide", "__blsmsk_u32",
			"-ccgo-hide", "__blsmsk_u64",
			"-ccgo-hide", "__blsr_u32",
			"-ccgo-hide", "__blsr_u64",
			"-ccgo-hide", "__bsfd",
			"-ccgo-hide", "__bsfq",
			"-ccgo-hide", "__bsrd",
			"-ccgo-hide", "__bsrq",
			"-ccgo-hide", "__bswapd",
			"-ccgo-hide", "__bswapq",
			"-ccgo-hide", "__crc32b",
			"-ccgo-hide", "__crc32d",
			"-ccgo-hide", "__crc32q",
			"-ccgo-hide", "__crc32w",
			"-ccgo-hide", "__llwpcb",
			"-ccgo-hide", "__lzcnt16",
			"-ccgo-hide", "__lzcnt32",
			"-ccgo-hide", "__lzcnt64",
			"-ccgo-hide", "__movsb",
			"-ccgo-hide", "__movsd",
			"-ccgo-hide", "__movsw",
			"-ccgo-hide", "__pause",
			"-ccgo-hide", "__popcntd",
			"-ccgo-hide", "__popcntq",
			"-ccgo-hide", "__rdpmc",
			"-ccgo-hide", "__rdtsc",
			"-ccgo-hide", "__rdtscp",
			"-ccgo-hide", "__readeflags",
			"-ccgo-hide", "__rolb",
			"-ccgo-hide", "__rold",
			"-ccgo-hide", "__rolq",
			"-ccgo-hide", "__rolw",
			"-ccgo-hide", "__rorb",
			"-ccgo-hide", "__rord",
			"-ccgo-hide", "__rorq",
			"-ccgo-hide", "__rorw",
			"-ccgo-hide", "__slwpcb",
			"-ccgo-hide", "__t1mskc_u32",
			"-ccgo-hide", "__t1mskc_u64",
			"-ccgo-hide", "__tzcnt_u16",
			"-ccgo-hide", "__tzcnt_u32",
			"-ccgo-hide", "__tzcnt_u64",
			"-ccgo-hide", "__tzmsk_u32",
			"-ccgo-hide", "__tzmsk_u64",
			"-ccgo-hide", "__writeeflags",
			"-ccgo-hide", "_addcarry_u32",
			"-ccgo-hide", "_addcarry_u64",
			"-ccgo-hide", "_addcarryx_u32",
			"-ccgo-hide", "_addcarryx_u64",
			"-ccgo-hide", "_bextr_u32",
			"-ccgo-hide", "_bextr_u64",
			"-ccgo-hide", "_bittestandcomplement",
			"-ccgo-hide", "_blsi_u32",
			"-ccgo-hide", "_blsi_u64",
			"-ccgo-hide", "_blsmsk_u32",
			"-ccgo-hide", "_blsmsk_u64",
			"-ccgo-hide", "_blsr_u32",
			"-ccgo-hide", "_blsr_u64",
			"-ccgo-hide", "_bzhi_u32",
			"-ccgo-hide", "_bzhi_u64",
			"-ccgo-hide", "_clrssbsy",
			"-ccgo-hide", "_cvtmask16_u32",
			"-ccgo-hide", "_cvtmask32_u32",
			"-ccgo-hide", "_cvtmask64_u64",
			"-ccgo-hide", "_cvtmask8_u32",
			"-ccgo-hide", "_cvtsh_ss",
			"-ccgo-hide", "_cvtu32_mask16",
			"-ccgo-hide", "_cvtu32_mask32",
			"-ccgo-hide", "_cvtu32_mask8",
			"-ccgo-hide", "_cvtu64_mask64",
			"-ccgo-hide", "_directstoreu_u32",
			"-ccgo-hide", "_directstoreu_u64",
			"-ccgo-hide", "_encls_u32",
			"-ccgo-hide", "_enclu_u32",
			"-ccgo-hide", "_enclv_u32",
			"-ccgo-hide", "_fxrstor",
			"-ccgo-hide", "_fxrstor64",
			"-ccgo-hide", "_fxsave",
			"-ccgo-hide", "_fxsave64",
			"-ccgo-hide", "_get_ssp",
			"-ccgo-hide", "_inc_ssp",
			"-ccgo-hide", "_kadd_mask16",
			"-ccgo-hide", "_kadd_mask32",
			"-ccgo-hide", "_kadd_mask64",
			"-ccgo-hide", "_kadd_mask8",
			"-ccgo-hide", "_kand_mask32",
			"-ccgo-hide", "_kand_mask64",
			"-ccgo-hide", "_kand_mask8",
			"-ccgo-hide", "_kandn_mask32",
			"-ccgo-hide", "_kandn_mask64",
			"-ccgo-hide", "_kandn_mask8",
			"-ccgo-hide", "_knot_mask32",
			"-ccgo-hide", "_knot_mask64",
			"-ccgo-hide", "_knot_mask8",
			"-ccgo-hide", "_kor_mask32",
			"-ccgo-hide", "_kor_mask64",
			"-ccgo-hide", "_kor_mask8",
			"-ccgo-hide", "_kortest_mask16_u8",
			"-ccgo-hide", "_kortest_mask32_u8",
			"-ccgo-hide", "_kortest_mask64_u8",
			"-ccgo-hide", "_kortest_mask8_u8",
			"-ccgo-hide", "_kortestc_mask16_u8",
			"-ccgo-hide", "_kortestc_mask32_u8",
			"-ccgo-hide", "_kortestc_mask64_u8",
			"-ccgo-hide", "_kortestc_mask8_u8",
			"-ccgo-hide", "_kortestz_mask16_u8",
			"-ccgo-hide", "_kortestz_mask32_u8",
			"-ccgo-hide", "_kortestz_mask64_u8",
			"-ccgo-hide", "_kortestz_mask8_u8",
			"-ccgo-hide", "_ktest_mask16_u8",
			"-ccgo-hide", "_ktest_mask32_u8",
			"-ccgo-hide", "_ktest_mask64_u8",
			"-ccgo-hide", "_ktest_mask8_u8",
			"-ccgo-hide", "_ktestc_mask16_u8",
			"-ccgo-hide", "_ktestc_mask32_u8",
			"-ccgo-hide", "_ktestc_mask64_u8",
			"-ccgo-hide", "_ktestc_mask8_u8",
			"-ccgo-hide", "_ktestz_mask16_u8",
			"-ccgo-hide", "_ktestz_mask32_u8",
			"-ccgo-hide", "_ktestz_mask64_u8",
			"-ccgo-hide", "_ktestz_mask8_u8",
			"-ccgo-hide", "_kunpackb_mask16",
			"-ccgo-hide", "_kunpackd_mask64",
			"-ccgo-hide", "_kunpackw_mask32",
			"-ccgo-hide", "_kxnor_mask32",
			"-ccgo-hide", "_kxnor_mask64",
			"-ccgo-hide", "_kxnor_mask8",
			"-ccgo-hide", "_kxor_mask32",
			"-ccgo-hide", "_kxor_mask64",
			"-ccgo-hide", "_kxor_mask8",
			"-ccgo-hide", "_load_mask16",
			"-ccgo-hide", "_load_mask32",
			"-ccgo-hide", "_load_mask64",
			"-ccgo-hide", "_load_mask8",
			"-ccgo-hide", "_lzcnt_u32",
			"-ccgo-hide", "_lzcnt_u64",
			"-ccgo-hide", "_m_empty",
			"-ccgo-hide", "_m_femms",
			"-ccgo-hide", "_m_from_float",
			"-ccgo-hide", "_m_from_int",
			"-ccgo-hide", "_m_from_int64",
			"-ccgo-hide", "_m_maskmovq",
			"-ccgo-hide", "_m_packssdw",
			"-ccgo-hide", "_m_packsswb",
			"-ccgo-hide", "_m_packuswb",
			"-ccgo-hide", "_m_paddb",
			"-ccgo-hide", "_m_paddd",
			"-ccgo-hide", "_m_paddsb",
			"-ccgo-hide", "_m_paddsw",
			"-ccgo-hide", "_m_paddusb",
			"-ccgo-hide", "_m_paddusw",
			"-ccgo-hide", "_m_paddw",
			"-ccgo-hide", "_m_pand",
			"-ccgo-hide", "_m_pandn",
			"-ccgo-hide", "_m_pavgb",
			"-ccgo-hide", "_m_pavgusb",
			"-ccgo-hide", "_m_pavgw",
			"-ccgo-hide", "_m_pcmpeqb",
			"-ccgo-hide", "_m_pcmpeqd",
			"-ccgo-hide", "_m_pcmpeqw",
			"-ccgo-hide", "_m_pcmpgtb",
			"-ccgo-hide", "_m_pcmpgtd",
			"-ccgo-hide", "_m_pcmpgtw",
			"-ccgo-hide", "_m_pf2id",
			"-ccgo-hide", "_m_pf2iw",
			"-ccgo-hide", "_m_pfacc",
			"-ccgo-hide", "_m_pfadd",
			"-ccgo-hide", "_m_pfcmpeq",
			"-ccgo-hide", "_m_pfcmpge",
			"-ccgo-hide", "_m_pfcmpgt",
			"-ccgo-hide", "_m_pfmax",
			"-ccgo-hide", "_m_pfmin",
			"-ccgo-hide", "_m_pfmul",
			"-ccgo-hide", "_m_pfnacc",
			"-ccgo-hide", "_m_pfpnacc",
			"-ccgo-hide", "_m_pfrcp",
			"-ccgo-hide", "_m_pfrcpit1",
			"-ccgo-hide", "_m_pfrcpit2",
			"-ccgo-hide", "_m_pfrsqit1",
			"-ccgo-hide", "_m_pfrsqrt",
			"-ccgo-hide", "_m_pfsub",
			"-ccgo-hide", "_m_pfsubr",
			"-ccgo-hide", "_m_pi2fd",
			"-ccgo-hide", "_m_pi2fw",
			"-ccgo-hide", "_m_pmaddwd",
			"-ccgo-hide", "_m_pmaxsw",
			"-ccgo-hide", "_m_pmaxub",
			"-ccgo-hide", "_m_pminsw",
			"-ccgo-hide", "_m_pminub",
			"-ccgo-hide", "_m_pmovmskb",
			"-ccgo-hide", "_m_pmulhrw",
			"-ccgo-hide", "_m_pmulhuw",
			"-ccgo-hide", "_m_pmulhw",
			"-ccgo-hide", "_m_pmullw",
			"-ccgo-hide", "_m_por",
			"-ccgo-hide", "_m_prefetch",
			"-ccgo-hide", "_m_prefetchw",
			"-ccgo-hide", "_m_psadbw",
			"-ccgo-hide", "_m_pslld",
			"-ccgo-hide", "_m_pslldi",
			"-ccgo-hide", "_m_psllq",
			"-ccgo-hide", "_m_psllqi",
			"-ccgo-hide", "_m_psllw",
			"-ccgo-hide", "_m_psllwi",
			"-ccgo-hide", "_m_psrad",
			"-ccgo-hide", "_m_psradi",
			"-ccgo-hide", "_m_psraw",
			"-ccgo-hide", "_m_psrawi",
			"-ccgo-hide", "_m_psrld",
			"-ccgo-hide", "_m_psrldi",
			"-ccgo-hide", "_m_psrlq",
			"-ccgo-hide", "_m_psrlqi",
			"-ccgo-hide", "_m_psrlw",
			"-ccgo-hide", "_m_psrlwi",
			"-ccgo-hide", "_m_psubb",
			"-ccgo-hide", "_m_psubd",
			"-ccgo-hide", "_m_psubsb",
			"-ccgo-hide", "_m_psubsw",
			"-ccgo-hide", "_m_psubusb",
			"-ccgo-hide", "_m_psubusw",
			"-ccgo-hide", "_m_psubw",
			"-ccgo-hide", "_m_pswapd",
			"-ccgo-hide", "_m_punpckhbw",
			"-ccgo-hide", "_m_punpckhdq",
			"-ccgo-hide", "_m_punpckhwd",
			"-ccgo-hide", "_m_punpcklbw",
			"-ccgo-hide", "_m_punpckldq",
			"-ccgo-hide", "_m_punpcklwd",
			"-ccgo-hide", "_m_pxor",
			"-ccgo-hide", "_m_to_float",
			"-ccgo-hide", "_m_to_int",
			"-ccgo-hide", "_m_to_int64",
			"-ccgo-hide", "_mm256_abs_epi16",
			"-ccgo-hide", "_mm256_abs_epi32",
			"-ccgo-hide", "_mm256_abs_epi64",
			"-ccgo-hide", "_mm256_abs_epi8",
			"-ccgo-hide", "_mm256_add_epi16",
			"-ccgo-hide", "_mm256_add_epi32",
			"-ccgo-hide", "_mm256_add_epi64",
			"-ccgo-hide", "_mm256_add_epi8",
			"-ccgo-hide", "_mm256_add_pd",
			"-ccgo-hide", "_mm256_add_ps",
			"-ccgo-hide", "_mm256_adds_epi16",
			"-ccgo-hide", "_mm256_adds_epi8",
			"-ccgo-hide", "_mm256_adds_epu16",
			"-ccgo-hide", "_mm256_adds_epu8",
			"-ccgo-hide", "_mm256_addsub_pd",
			"-ccgo-hide", "_mm256_addsub_ps",
			"-ccgo-hide", "_mm256_aesdec_epi128",
			"-ccgo-hide", "_mm256_aesdeclast_epi128",
			"-ccgo-hide", "_mm256_aesenc_epi128",
			"-ccgo-hide", "_mm256_aesenclast_epi128",
			"-ccgo-hide", "_mm256_and_pd",
			"-ccgo-hide", "_mm256_and_ps",
			"-ccgo-hide", "_mm256_and_si256",
			"-ccgo-hide", "_mm256_andnot_pd",
			"-ccgo-hide", "_mm256_andnot_ps",
			"-ccgo-hide", "_mm256_andnot_si256",
			"-ccgo-hide", "_mm256_avg_epu16",
			"-ccgo-hide", "_mm256_avg_epu8",
			"-ccgo-hide", "_mm256_bitshuffle_epi64_mask",
			"-ccgo-hide", "_mm256_blendv_epi8",
			"-ccgo-hide", "_mm256_blendv_pd",
			"-ccgo-hide", "_mm256_blendv_ps",
			"-ccgo-hide", "_mm256_broadcast_f32x2",
			"-ccgo-hide", "_mm256_broadcast_f32x4",
			"-ccgo-hide", "_mm256_broadcast_f64x2",
			"-ccgo-hide", "_mm256_broadcast_i32x2",
			"-ccgo-hide", "_mm256_broadcast_i32x4",
			"-ccgo-hide", "_mm256_broadcast_i64x2",
			"-ccgo-hide", "_mm256_broadcast_pd",
			"-ccgo-hide", "_mm256_broadcast_ps",
			"-ccgo-hide", "_mm256_broadcast_sd",
			"-ccgo-hide", "_mm256_broadcast_ss",
			"-ccgo-hide", "_mm256_broadcastb_epi8",
			"-ccgo-hide", "_mm256_broadcastd_epi32",
			"-ccgo-hide", "_mm256_broadcastmb_epi64",
			"-ccgo-hide", "_mm256_broadcastmw_epi32",
			"-ccgo-hide", "_mm256_broadcastq_epi64",
			"-ccgo-hide", "_mm256_broadcastsd_pd",
			"-ccgo-hide", "_mm256_broadcastsi128_si256",
			"-ccgo-hide", "_mm256_broadcastss_ps",
			"-ccgo-hide", "_mm256_broadcastw_epi16",
			"-ccgo-hide", "_mm256_castpd128_pd256",
			"-ccgo-hide", "_mm256_castpd256_pd128",
			"-ccgo-hide", "_mm256_castpd_ps",
			"-ccgo-hide", "_mm256_castpd_si256",
			"-ccgo-hide", "_mm256_castps128_ps256",
			"-ccgo-hide", "_mm256_castps256_ps128",
			"-ccgo-hide", "_mm256_castps_pd",
			"-ccgo-hide", "_mm256_castps_si256",
			"-ccgo-hide", "_mm256_castsi128_si256",
			"-ccgo-hide", "_mm256_castsi256_pd",
			"-ccgo-hide", "_mm256_castsi256_ps",
			"-ccgo-hide", "_mm256_castsi256_si128",
			"-ccgo-hide", "_mm256_cmpeq_epi16",
			"-ccgo-hide", "_mm256_cmpeq_epi16_mask",
			"-ccgo-hide", "_mm256_cmpeq_epi32",
			"-ccgo-hide", "_mm256_cmpeq_epi32_mask",
			"-ccgo-hide", "_mm256_cmpeq_epi64",
			"-ccgo-hide", "_mm256_cmpeq_epi64_mask",
			"-ccgo-hide", "_mm256_cmpeq_epi8",
			"-ccgo-hide", "_mm256_cmpeq_epi8_mask",
			"-ccgo-hide", "_mm256_cmpeq_epu16_mask",
			"-ccgo-hide", "_mm256_cmpeq_epu32_mask",
			"-ccgo-hide", "_mm256_cmpeq_epu64_mask",
			"-ccgo-hide", "_mm256_cmpeq_epu8_mask",
			"-ccgo-hide", "_mm256_cmpge_epi16_mask",
			"-ccgo-hide", "_mm256_cmpge_epi32_mask",
			"-ccgo-hide", "_mm256_cmpge_epi64_mask",
			"-ccgo-hide", "_mm256_cmpge_epi8_mask",
			"-ccgo-hide", "_mm256_cmpge_epu16_mask",
			"-ccgo-hide", "_mm256_cmpge_epu32_mask",
			"-ccgo-hide", "_mm256_cmpge_epu64_mask",
			"-ccgo-hide", "_mm256_cmpge_epu8_mask",
			"-ccgo-hide", "_mm256_cmpgt_epi16",
			"-ccgo-hide", "_mm256_cmpgt_epi16_mask",
			"-ccgo-hide", "_mm256_cmpgt_epi32",
			"-ccgo-hide", "_mm256_cmpgt_epi32_mask",
			"-ccgo-hide", "_mm256_cmpgt_epi64",
			"-ccgo-hide", "_mm256_cmpgt_epi64_mask",
			"-ccgo-hide", "_mm256_cmpgt_epi8",
			"-ccgo-hide", "_mm256_cmpgt_epi8_mask",
			"-ccgo-hide", "_mm256_cmpgt_epu16_mask",
			"-ccgo-hide", "_mm256_cmpgt_epu32_mask",
			"-ccgo-hide", "_mm256_cmpgt_epu64_mask",
			"-ccgo-hide", "_mm256_cmpgt_epu8_mask",
			"-ccgo-hide", "_mm256_cmple_epi16_mask",
			"-ccgo-hide", "_mm256_cmple_epi32_mask",
			"-ccgo-hide", "_mm256_cmple_epi64_mask",
			"-ccgo-hide", "_mm256_cmple_epi8_mask",
			"-ccgo-hide", "_mm256_cmple_epu16_mask",
			"-ccgo-hide", "_mm256_cmple_epu32_mask",
			"-ccgo-hide", "_mm256_cmple_epu64_mask",
			"-ccgo-hide", "_mm256_cmple_epu8_mask",
			"-ccgo-hide", "_mm256_cmplt_epi16_mask",
			"-ccgo-hide", "_mm256_cmplt_epi32_mask",
			"-ccgo-hide", "_mm256_cmplt_epi64_mask",
			"-ccgo-hide", "_mm256_cmplt_epi8_mask",
			"-ccgo-hide", "_mm256_cmplt_epu16_mask",
			"-ccgo-hide", "_mm256_cmplt_epu32_mask",
			"-ccgo-hide", "_mm256_cmplt_epu64_mask",
			"-ccgo-hide", "_mm256_cmplt_epu8_mask",
			"-ccgo-hide", "_mm256_cmpneq_epi16_mask",
			"-ccgo-hide", "_mm256_cmpneq_epi32_mask",
			"-ccgo-hide", "_mm256_cmpneq_epi64_mask",
			"-ccgo-hide", "_mm256_cmpneq_epi8_mask",
			"-ccgo-hide", "_mm256_cmpneq_epu16_mask",
			"-ccgo-hide", "_mm256_cmpneq_epu32_mask",
			"-ccgo-hide", "_mm256_cmpneq_epu64_mask",
			"-ccgo-hide", "_mm256_cmpneq_epu8_mask",
			"-ccgo-hide", "_mm256_conflict_epi32",
			"-ccgo-hide", "_mm256_conflict_epi64",
			"-ccgo-hide", "_mm256_cvtepi16_epi32",
			"-ccgo-hide", "_mm256_cvtepi16_epi64",
			"-ccgo-hide", "_mm256_cvtepi16_epi8",
			"-ccgo-hide", "_mm256_cvtepi32_epi16",
			"-ccgo-hide", "_mm256_cvtepi32_epi64",
			"-ccgo-hide", "_mm256_cvtepi32_epi8",
			"-ccgo-hide", "_mm256_cvtepi32_pd",
			"-ccgo-hide", "_mm256_cvtepi32_ps",
			"-ccgo-hide", "_mm256_cvtepi64_epi16",
			"-ccgo-hide", "_mm256_cvtepi64_epi32",
			"-ccgo-hide", "_mm256_cvtepi64_epi8",
			"-ccgo-hide", "_mm256_cvtepi64_pd",
			"-ccgo-hide", "_mm256_cvtepi64_ps",
			"-ccgo-hide", "_mm256_cvtepi8_epi16",
			"-ccgo-hide", "_mm256_cvtepi8_epi32",
			"-ccgo-hide", "_mm256_cvtepi8_epi64",
			"-ccgo-hide", "_mm256_cvtepu16_epi32",
			"-ccgo-hide", "_mm256_cvtepu16_epi64",
			"-ccgo-hide", "_mm256_cvtepu32_epi64",
			"-ccgo-hide", "_mm256_cvtepu32_pd",
			"-ccgo-hide", "_mm256_cvtepu32_ps",
			"-ccgo-hide", "_mm256_cvtepu64_pd",
			"-ccgo-hide", "_mm256_cvtepu64_ps",
			"-ccgo-hide", "_mm256_cvtepu8_epi16",
			"-ccgo-hide", "_mm256_cvtepu8_epi32",
			"-ccgo-hide", "_mm256_cvtepu8_epi64",
			"-ccgo-hide", "_mm256_cvtpd_epi32",
			"-ccgo-hide", "_mm256_cvtpd_epi64",
			"-ccgo-hide", "_mm256_cvtpd_epu32",
			"-ccgo-hide", "_mm256_cvtpd_epu64",
			"-ccgo-hide", "_mm256_cvtpd_ps",
			"-ccgo-hide", "_mm256_cvtph_ps",
			"-ccgo-hide", "_mm256_cvtps_epi32",
			"-ccgo-hide", "_mm256_cvtps_epi64",
			"-ccgo-hide", "_mm256_cvtps_epu32",
			"-ccgo-hide", "_mm256_cvtps_epu64",
			"-ccgo-hide", "_mm256_cvtps_pd",
			"-ccgo-hide", "_mm256_cvtsd_f64",
			"-ccgo-hide", "_mm256_cvtsepi16_epi8",
			"-ccgo-hide", "_mm256_cvtsepi32_epi16",
			"-ccgo-hide", "_mm256_cvtsepi32_epi8",
			"-ccgo-hide", "_mm256_cvtsepi64_epi16",
			"-ccgo-hide", "_mm256_cvtsepi64_epi32",
			"-ccgo-hide", "_mm256_cvtsepi64_epi8",
			"-ccgo-hide", "_mm256_cvtss_f32",
			"-ccgo-hide", "_mm256_cvttpd_epi32",
			"-ccgo-hide", "_mm256_cvttpd_epi64",
			"-ccgo-hide", "_mm256_cvttpd_epu32",
			"-ccgo-hide", "_mm256_cvttpd_epu64",
			"-ccgo-hide", "_mm256_cvttps_epi32",
			"-ccgo-hide", "_mm256_cvttps_epi64",
			"-ccgo-hide", "_mm256_cvttps_epu32",
			"-ccgo-hide", "_mm256_cvttps_epu64",
			"-ccgo-hide", "_mm256_cvtusepi16_epi8",
			"-ccgo-hide", "_mm256_cvtusepi32_epi16",
			"-ccgo-hide", "_mm256_cvtusepi32_epi8",
			"-ccgo-hide", "_mm256_cvtusepi64_epi16",
			"-ccgo-hide", "_mm256_cvtusepi64_epi32",
			"-ccgo-hide", "_mm256_cvtusepi64_epi8",
			"-ccgo-hide", "_mm256_div_pd",
			"-ccgo-hide", "_mm256_div_ps",
			"-ccgo-hide", "_mm256_dpbusd_epi32",
			"-ccgo-hide", "_mm256_dpbusds_epi32",
			"-ccgo-hide", "_mm256_dpwssd_epi32",
			"-ccgo-hide", "_mm256_dpwssds_epi32",
			"-ccgo-hide", "_mm256_fmadd_pd",
			"-ccgo-hide", "_mm256_fmadd_ps",
			"-ccgo-hide", "_mm256_fmaddsub_pd",
			"-ccgo-hide", "_mm256_fmaddsub_ps",
			"-ccgo-hide", "_mm256_fmsub_pd",
			"-ccgo-hide", "_mm256_fmsub_ps",
			"-ccgo-hide", "_mm256_fmsubadd_pd",
			"-ccgo-hide", "_mm256_fmsubadd_ps",
			"-ccgo-hide", "_mm256_fnmadd_pd",
			"-ccgo-hide", "_mm256_fnmadd_ps",
			"-ccgo-hide", "_mm256_fnmsub_pd",
			"-ccgo-hide", "_mm256_fnmsub_ps",
			"-ccgo-hide", "_mm256_frcz_pd",
			"-ccgo-hide", "_mm256_frcz_ps",
			"-ccgo-hide", "_mm256_getexp_pd",
			"-ccgo-hide", "_mm256_getexp_ps",
			"-ccgo-hide", "_mm256_gf2p8mul_epi8",
			"-ccgo-hide", "_mm256_hadd_epi16",
			"-ccgo-hide", "_mm256_hadd_epi32",
			"-ccgo-hide", "_mm256_hadd_pd",
			"-ccgo-hide", "_mm256_hadd_ps",
			"-ccgo-hide", "_mm256_hadds_epi16",
			"-ccgo-hide", "_mm256_hsub_epi16",
			"-ccgo-hide", "_mm256_hsub_epi32",
			"-ccgo-hide", "_mm256_hsub_pd",
			"-ccgo-hide", "_mm256_hsub_ps",
			"-ccgo-hide", "_mm256_hsubs_epi16",
			"-ccgo-hide", "_mm256_lddqu_si256",
			"-ccgo-hide", "_mm256_load_pd",
			"-ccgo-hide", "_mm256_load_ps",
			"-ccgo-hide", "_mm256_load_si256",
			"-ccgo-hide", "_mm256_loadu_pd",
			"-ccgo-hide", "_mm256_loadu_ps",
			"-ccgo-hide", "_mm256_loadu_si256",
			"-ccgo-hide", "_mm256_lzcnt_epi32",
			"-ccgo-hide", "_mm256_lzcnt_epi64",
			"-ccgo-hide", "_mm256_macc_pd",
			"-ccgo-hide", "_mm256_macc_ps",
			"-ccgo-hide", "_mm256_madd52hi_epu64",
			"-ccgo-hide", "_mm256_madd52lo_epu64",
			"-ccgo-hide", "_mm256_madd_epi16",
			"-ccgo-hide", "_mm256_maddsub_pd",
			"-ccgo-hide", "_mm256_maddsub_ps",
			"-ccgo-hide", "_mm256_maddubs_epi16",
			"-ccgo-hide", "_mm256_mask2_permutex2var_epi16",
			"-ccgo-hide", "_mm256_mask2_permutex2var_epi32",
			"-ccgo-hide", "_mm256_mask2_permutex2var_epi64",
			"-ccgo-hide", "_mm256_mask2_permutex2var_epi8",
			"-ccgo-hide", "_mm256_mask2_permutex2var_pd",
			"-ccgo-hide", "_mm256_mask2_permutex2var_ps",
			"-ccgo-hide", "_mm256_mask3_fmadd_pd",
			"-ccgo-hide", "_mm256_mask3_fmadd_ps",
			"-ccgo-hide", "_mm256_mask3_fmaddsub_pd",
			"-ccgo-hide", "_mm256_mask3_fmaddsub_ps",
			"-ccgo-hide", "_mm256_mask3_fmsub_pd",
			"-ccgo-hide", "_mm256_mask3_fmsub_ps",
			"-ccgo-hide", "_mm256_mask3_fmsubadd_pd",
			"-ccgo-hide", "_mm256_mask3_fmsubadd_ps",
			"-ccgo-hide", "_mm256_mask3_fnmadd_pd",
			"-ccgo-hide", "_mm256_mask3_fnmadd_ps",
			"-ccgo-hide", "_mm256_mask3_fnmsub_pd",
			"-ccgo-hide", "_mm256_mask3_fnmsub_ps",
			"-ccgo-hide", "_mm256_mask_abs_epi16",
			"-ccgo-hide", "_mm256_mask_abs_epi32",
			"-ccgo-hide", "_mm256_mask_abs_epi64",
			"-ccgo-hide", "_mm256_mask_abs_epi8",
			"-ccgo-hide", "_mm256_mask_add_epi16",
			"-ccgo-hide", "_mm256_mask_add_epi32",
			"-ccgo-hide", "_mm256_mask_add_epi64",
			"-ccgo-hide", "_mm256_mask_add_epi8",
			"-ccgo-hide", "_mm256_mask_add_pd",
			"-ccgo-hide", "_mm256_mask_add_ps",
			"-ccgo-hide", "_mm256_mask_adds_epi16",
			"-ccgo-hide", "_mm256_mask_adds_epi8",
			"-ccgo-hide", "_mm256_mask_adds_epu16",
			"-ccgo-hide", "_mm256_mask_adds_epu8",
			"-ccgo-hide", "_mm256_mask_and_epi32",
			"-ccgo-hide", "_mm256_mask_and_epi64",
			"-ccgo-hide", "_mm256_mask_and_pd",
			"-ccgo-hide", "_mm256_mask_and_ps",
			"-ccgo-hide", "_mm256_mask_andnot_epi32",
			"-ccgo-hide", "_mm256_mask_andnot_epi64",
			"-ccgo-hide", "_mm256_mask_andnot_pd",
			"-ccgo-hide", "_mm256_mask_andnot_ps",
			"-ccgo-hide", "_mm256_mask_avg_epu16",
			"-ccgo-hide", "_mm256_mask_avg_epu8",
			"-ccgo-hide", "_mm256_mask_bitshuffle_epi64_mask",
			"-ccgo-hide", "_mm256_mask_broadcast_f32x2",
			"-ccgo-hide", "_mm256_mask_broadcast_f32x4",
			"-ccgo-hide", "_mm256_mask_broadcast_f64x2",
			"-ccgo-hide", "_mm256_mask_broadcast_i32x2",
			"-ccgo-hide", "_mm256_mask_broadcast_i32x4",
			"-ccgo-hide", "_mm256_mask_broadcast_i64x2",
			"-ccgo-hide", "_mm256_mask_broadcastb_epi8",
			"-ccgo-hide", "_mm256_mask_broadcastd_epi32",
			"-ccgo-hide", "_mm256_mask_broadcastq_epi64",
			"-ccgo-hide", "_mm256_mask_broadcastsd_pd",
			"-ccgo-hide", "_mm256_mask_broadcastss_ps",
			"-ccgo-hide", "_mm256_mask_broadcastw_epi16",
			"-ccgo-hide", "_mm256_mask_cmpeq_epi16_mask",
			"-ccgo-hide", "_mm256_mask_cmpeq_epi32_mask",
			"-ccgo-hide", "_mm256_mask_cmpeq_epi64_mask",
			"-ccgo-hide", "_mm256_mask_cmpeq_epi8_mask",
			"-ccgo-hide", "_mm256_mask_cmpeq_epu16_mask",
			"-ccgo-hide", "_mm256_mask_cmpeq_epu32_mask",
			"-ccgo-hide", "_mm256_mask_cmpeq_epu64_mask",
			"-ccgo-hide", "_mm256_mask_cmpeq_epu8_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epi16_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epi32_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epi64_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epi8_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epu16_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epu32_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epu64_mask",
			"-ccgo-hide", "_mm256_mask_cmpge_epu8_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epi16_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epi32_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epi64_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epi8_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epu16_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epu32_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epu64_mask",
			"-ccgo-hide", "_mm256_mask_cmpgt_epu8_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epi16_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epi32_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epi64_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epi8_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epu16_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epu32_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epu64_mask",
			"-ccgo-hide", "_mm256_mask_cmple_epu8_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epi16_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epi32_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epi64_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epi8_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epu16_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epu32_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epu64_mask",
			"-ccgo-hide", "_mm256_mask_cmplt_epu8_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epi16_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epi32_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epi64_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epi8_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epu16_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epu32_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epu64_mask",
			"-ccgo-hide", "_mm256_mask_cmpneq_epu8_mask",
			"-ccgo-hide", "_mm256_mask_compress_epi16",
			"-ccgo-hide", "_mm256_mask_compress_epi32",
			"-ccgo-hide", "_mm256_mask_compress_epi64",
			"-ccgo-hide", "_mm256_mask_compress_epi8",
			"-ccgo-hide", "_mm256_mask_compress_pd",
			"-ccgo-hide", "_mm256_mask_compress_ps",
			"-ccgo-hide", "_mm256_mask_compressstoreu_epi16",
			"-ccgo-hide", "_mm256_mask_compressstoreu_epi32",
			"-ccgo-hide", "_mm256_mask_compressstoreu_epi64",
			"-ccgo-hide", "_mm256_mask_compressstoreu_epi8",
			"-ccgo-hide", "_mm256_mask_compressstoreu_pd",
			"-ccgo-hide", "_mm256_mask_compressstoreu_ps",
			"-ccgo-hide", "_mm256_mask_conflict_epi32",
			"-ccgo-hide", "_mm256_mask_conflict_epi64",
			"-ccgo-hide", "_mm256_mask_cvtepi16_epi32",
			"-ccgo-hide", "_mm256_mask_cvtepi16_epi64",
			"-ccgo-hide", "_mm256_mask_cvtepi16_epi8",
			"-ccgo-hide", "_mm256_mask_cvtepi16_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvtepi32_epi16",
			"-ccgo-hide", "_mm256_mask_cvtepi32_epi64",
			"-ccgo-hide", "_mm256_mask_cvtepi32_epi8",
			"-ccgo-hide", "_mm256_mask_cvtepi32_pd",
			"-ccgo-hide", "_mm256_mask_cvtepi32_ps",
			"-ccgo-hide", "_mm256_mask_cvtepi32_storeu_epi16",
			"-ccgo-hide", "_mm256_mask_cvtepi32_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvtepi64_epi16",
			"-ccgo-hide", "_mm256_mask_cvtepi64_epi32",
			"-ccgo-hide", "_mm256_mask_cvtepi64_epi8",
			"-ccgo-hide", "_mm256_mask_cvtepi64_pd",
			"-ccgo-hide", "_mm256_mask_cvtepi64_ps",
			"-ccgo-hide", "_mm256_mask_cvtepi64_storeu_epi16",
			"-ccgo-hide", "_mm256_mask_cvtepi64_storeu_epi32",
			"-ccgo-hide", "_mm256_mask_cvtepi64_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvtepi8_epi16",
			"-ccgo-hide", "_mm256_mask_cvtepi8_epi32",
			"-ccgo-hide", "_mm256_mask_cvtepi8_epi64",
			"-ccgo-hide", "_mm256_mask_cvtepu16_epi32",
			"-ccgo-hide", "_mm256_mask_cvtepu16_epi64",
			"-ccgo-hide", "_mm256_mask_cvtepu32_epi64",
			"-ccgo-hide", "_mm256_mask_cvtepu32_pd",
			"-ccgo-hide", "_mm256_mask_cvtepu32_ps",
			"-ccgo-hide", "_mm256_mask_cvtepu64_pd",
			"-ccgo-hide", "_mm256_mask_cvtepu64_ps",
			"-ccgo-hide", "_mm256_mask_cvtepu8_epi16",
			"-ccgo-hide", "_mm256_mask_cvtepu8_epi32",
			"-ccgo-hide", "_mm256_mask_cvtepu8_epi64",
			"-ccgo-hide", "_mm256_mask_cvtpd_epi32",
			"-ccgo-hide", "_mm256_mask_cvtpd_epi64",
			"-ccgo-hide", "_mm256_mask_cvtpd_epu32",
			"-ccgo-hide", "_mm256_mask_cvtpd_epu64",
			"-ccgo-hide", "_mm256_mask_cvtpd_ps",
			"-ccgo-hide", "_mm256_mask_cvtph_ps",
			"-ccgo-hide", "_mm256_mask_cvtps_epi32",
			"-ccgo-hide", "_mm256_mask_cvtps_epi64",
			"-ccgo-hide", "_mm256_mask_cvtps_epu32",
			"-ccgo-hide", "_mm256_mask_cvtps_epu64",
			"-ccgo-hide", "_mm256_mask_cvtps_pd",
			"-ccgo-hide", "_mm256_mask_cvtsepi16_epi8",
			"-ccgo-hide", "_mm256_mask_cvtsepi16_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvtsepi32_epi16",
			"-ccgo-hide", "_mm256_mask_cvtsepi32_epi8",
			"-ccgo-hide", "_mm256_mask_cvtsepi32_storeu_epi16",
			"-ccgo-hide", "_mm256_mask_cvtsepi32_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvtsepi64_epi16",
			"-ccgo-hide", "_mm256_mask_cvtsepi64_epi32",
			"-ccgo-hide", "_mm256_mask_cvtsepi64_epi8",
			"-ccgo-hide", "_mm256_mask_cvtsepi64_storeu_epi16",
			"-ccgo-hide", "_mm256_mask_cvtsepi64_storeu_epi32",
			"-ccgo-hide", "_mm256_mask_cvtsepi64_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvttpd_epi32",
			"-ccgo-hide", "_mm256_mask_cvttpd_epi64",
			"-ccgo-hide", "_mm256_mask_cvttpd_epu32",
			"-ccgo-hide", "_mm256_mask_cvttpd_epu64",
			"-ccgo-hide", "_mm256_mask_cvttps_epi32",
			"-ccgo-hide", "_mm256_mask_cvttps_epi64",
			"-ccgo-hide", "_mm256_mask_cvttps_epu32",
			"-ccgo-hide", "_mm256_mask_cvttps_epu64",
			"-ccgo-hide", "_mm256_mask_cvtusepi16_epi8",
			"-ccgo-hide", "_mm256_mask_cvtusepi16_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvtusepi32_epi16",
			"-ccgo-hide", "_mm256_mask_cvtusepi32_epi8",
			"-ccgo-hide", "_mm256_mask_cvtusepi32_storeu_epi16",
			"-ccgo-hide", "_mm256_mask_cvtusepi32_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_cvtusepi64_epi16",
			"-ccgo-hide", "_mm256_mask_cvtusepi64_epi32",
			"-ccgo-hide", "_mm256_mask_cvtusepi64_epi8",
			"-ccgo-hide", "_mm256_mask_cvtusepi64_storeu_epi16",
			"-ccgo-hide", "_mm256_mask_cvtusepi64_storeu_epi32",
			"-ccgo-hide", "_mm256_mask_cvtusepi64_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_div_pd",
			"-ccgo-hide", "_mm256_mask_div_ps",
			"-ccgo-hide", "_mm256_mask_dpbusd_epi32",
			"-ccgo-hide", "_mm256_mask_dpbusds_epi32",
			"-ccgo-hide", "_mm256_mask_dpwssd_epi32",
			"-ccgo-hide", "_mm256_mask_dpwssds_epi32",
			"-ccgo-hide", "_mm256_mask_expand_epi16",
			"-ccgo-hide", "_mm256_mask_expand_epi32",
			"-ccgo-hide", "_mm256_mask_expand_epi64",
			"-ccgo-hide", "_mm256_mask_expand_epi8",
			"-ccgo-hide", "_mm256_mask_expand_pd",
			"-ccgo-hide", "_mm256_mask_expand_ps",
			"-ccgo-hide", "_mm256_mask_expandloadu_epi16",
			"-ccgo-hide", "_mm256_mask_expandloadu_epi32",
			"-ccgo-hide", "_mm256_mask_expandloadu_epi64",
			"-ccgo-hide", "_mm256_mask_expandloadu_epi8",
			"-ccgo-hide", "_mm256_mask_expandloadu_pd",
			"-ccgo-hide", "_mm256_mask_expandloadu_ps",
			"-ccgo-hide", "_mm256_mask_fmadd_pd",
			"-ccgo-hide", "_mm256_mask_fmadd_ps",
			"-ccgo-hide", "_mm256_mask_fmaddsub_pd",
			"-ccgo-hide", "_mm256_mask_fmaddsub_ps",
			"-ccgo-hide", "_mm256_mask_fmsub_pd",
			"-ccgo-hide", "_mm256_mask_fmsub_ps",
			"-ccgo-hide", "_mm256_mask_fmsubadd_pd",
			"-ccgo-hide", "_mm256_mask_fmsubadd_ps",
			"-ccgo-hide", "_mm256_mask_fnmadd_pd",
			"-ccgo-hide", "_mm256_mask_fnmadd_ps",
			"-ccgo-hide", "_mm256_mask_fnmsub_pd",
			"-ccgo-hide", "_mm256_mask_fnmsub_ps",
			"-ccgo-hide", "_mm256_mask_getexp_pd",
			"-ccgo-hide", "_mm256_mask_getexp_ps",
			"-ccgo-hide", "_mm256_mask_gf2p8mul_epi8",
			"-ccgo-hide", "_mm256_mask_load_epi32",
			"-ccgo-hide", "_mm256_mask_load_epi64",
			"-ccgo-hide", "_mm256_mask_load_pd",
			"-ccgo-hide", "_mm256_mask_load_ps",
			"-ccgo-hide", "_mm256_mask_loadu_epi16",
			"-ccgo-hide", "_mm256_mask_loadu_epi32",
			"-ccgo-hide", "_mm256_mask_loadu_epi64",
			"-ccgo-hide", "_mm256_mask_loadu_epi8",
			"-ccgo-hide", "_mm256_mask_loadu_pd",
			"-ccgo-hide", "_mm256_mask_loadu_ps",
			"-ccgo-hide", "_mm256_mask_lzcnt_epi32",
			"-ccgo-hide", "_mm256_mask_lzcnt_epi64",
			"-ccgo-hide", "_mm256_mask_madd52hi_epu64",
			"-ccgo-hide", "_mm256_mask_madd52lo_epu64",
			"-ccgo-hide", "_mm256_mask_madd_epi16",
			"-ccgo-hide", "_mm256_mask_maddubs_epi16",
			"-ccgo-hide", "_mm256_mask_max_epi16",
			"-ccgo-hide", "_mm256_mask_max_epi32",
			"-ccgo-hide", "_mm256_mask_max_epi64",
			"-ccgo-hide", "_mm256_mask_max_epi8",
			"-ccgo-hide", "_mm256_mask_max_epu16",
			"-ccgo-hide", "_mm256_mask_max_epu32",
			"-ccgo-hide", "_mm256_mask_max_epu64",
			"-ccgo-hide", "_mm256_mask_max_epu8",
			"-ccgo-hide", "_mm256_mask_max_pd",
			"-ccgo-hide", "_mm256_mask_max_ps",
			"-ccgo-hide", "_mm256_mask_min_epi16",
			"-ccgo-hide", "_mm256_mask_min_epi32",
			"-ccgo-hide", "_mm256_mask_min_epi64",
			"-ccgo-hide", "_mm256_mask_min_epi8",
			"-ccgo-hide", "_mm256_mask_min_epu16",
			"-ccgo-hide", "_mm256_mask_min_epu32",
			"-ccgo-hide", "_mm256_mask_min_epu64",
			"-ccgo-hide", "_mm256_mask_min_epu8",
			"-ccgo-hide", "_mm256_mask_min_pd",
			"-ccgo-hide", "_mm256_mask_min_ps",
			"-ccgo-hide", "_mm256_mask_mov_epi16",
			"-ccgo-hide", "_mm256_mask_mov_epi32",
			"-ccgo-hide", "_mm256_mask_mov_epi64",
			"-ccgo-hide", "_mm256_mask_mov_epi8",
			"-ccgo-hide", "_mm256_mask_mov_pd",
			"-ccgo-hide", "_mm256_mask_mov_ps",
			"-ccgo-hide", "_mm256_mask_movedup_pd",
			"-ccgo-hide", "_mm256_mask_movehdup_ps",
			"-ccgo-hide", "_mm256_mask_moveldup_ps",
			"-ccgo-hide", "_mm256_mask_mul_epi32",
			"-ccgo-hide", "_mm256_mask_mul_epu32",
			"-ccgo-hide", "_mm256_mask_mul_pd",
			"-ccgo-hide", "_mm256_mask_mul_ps",
			"-ccgo-hide", "_mm256_mask_mulhi_epi16",
			"-ccgo-hide", "_mm256_mask_mulhi_epu16",
			"-ccgo-hide", "_mm256_mask_mulhrs_epi16",
			"-ccgo-hide", "_mm256_mask_mullo_epi16",
			"-ccgo-hide", "_mm256_mask_mullo_epi32",
			"-ccgo-hide", "_mm256_mask_mullo_epi64",
			"-ccgo-hide", "_mm256_mask_multishift_epi64_epi8",
			"-ccgo-hide", "_mm256_mask_or_epi32",
			"-ccgo-hide", "_mm256_mask_or_epi64",
			"-ccgo-hide", "_mm256_mask_or_pd",
			"-ccgo-hide", "_mm256_mask_or_ps",
			"-ccgo-hide", "_mm256_mask_packs_epi16",
			"-ccgo-hide", "_mm256_mask_packs_epi32",
			"-ccgo-hide", "_mm256_mask_packus_epi16",
			"-ccgo-hide", "_mm256_mask_packus_epi32",
			"-ccgo-hide", "_mm256_mask_permutevar_pd",
			"-ccgo-hide", "_mm256_mask_permutevar_ps",
			"-ccgo-hide", "_mm256_mask_permutex2var_epi16",
			"-ccgo-hide", "_mm256_mask_permutex2var_epi32",
			"-ccgo-hide", "_mm256_mask_permutex2var_epi64",
			"-ccgo-hide", "_mm256_mask_permutex2var_epi8",
			"-ccgo-hide", "_mm256_mask_permutex2var_pd",
			"-ccgo-hide", "_mm256_mask_permutex2var_ps",
			"-ccgo-hide", "_mm256_mask_permutexvar_epi16",
			"-ccgo-hide", "_mm256_mask_permutexvar_epi32",
			"-ccgo-hide", "_mm256_mask_permutexvar_epi64",
			"-ccgo-hide", "_mm256_mask_permutexvar_epi8",
			"-ccgo-hide", "_mm256_mask_permutexvar_pd",
			"-ccgo-hide", "_mm256_mask_permutexvar_ps",
			"-ccgo-hide", "_mm256_mask_popcnt_epi16",
			"-ccgo-hide", "_mm256_mask_popcnt_epi32",
			"-ccgo-hide", "_mm256_mask_popcnt_epi64",
			"-ccgo-hide", "_mm256_mask_popcnt_epi8",
			"-ccgo-hide", "_mm256_mask_rcp14_pd",
			"-ccgo-hide", "_mm256_mask_rcp14_ps",
			"-ccgo-hide", "_mm256_mask_rolv_epi32",
			"-ccgo-hide", "_mm256_mask_rolv_epi64",
			"-ccgo-hide", "_mm256_mask_rorv_epi32",
			"-ccgo-hide", "_mm256_mask_rorv_epi64",
			"-ccgo-hide", "_mm256_mask_rsqrt14_pd",
			"-ccgo-hide", "_mm256_mask_rsqrt14_ps",
			"-ccgo-hide", "_mm256_mask_scalef_pd",
			"-ccgo-hide", "_mm256_mask_scalef_ps",
			"-ccgo-hide", "_mm256_mask_set1_epi16",
			"-ccgo-hide", "_mm256_mask_set1_epi32",
			"-ccgo-hide", "_mm256_mask_set1_epi64",
			"-ccgo-hide", "_mm256_mask_set1_epi8",
			"-ccgo-hide", "_mm256_mask_shldv_epi16",
			"-ccgo-hide", "_mm256_mask_shldv_epi32",
			"-ccgo-hide", "_mm256_mask_shldv_epi64",
			"-ccgo-hide", "_mm256_mask_shrdv_epi16",
			"-ccgo-hide", "_mm256_mask_shrdv_epi32",
			"-ccgo-hide", "_mm256_mask_shrdv_epi64",
			"-ccgo-hide", "_mm256_mask_shuffle_epi8",
			"-ccgo-hide", "_mm256_mask_sll_epi16",
			"-ccgo-hide", "_mm256_mask_sll_epi32",
			"-ccgo-hide", "_mm256_mask_sll_epi64",
			"-ccgo-hide", "_mm256_mask_sllv_epi16",
			"-ccgo-hide", "_mm256_mask_sllv_epi32",
			"-ccgo-hide", "_mm256_mask_sllv_epi64",
			"-ccgo-hide", "_mm256_mask_sqrt_pd",
			"-ccgo-hide", "_mm256_mask_sqrt_ps",
			"-ccgo-hide", "_mm256_mask_sra_epi16",
			"-ccgo-hide", "_mm256_mask_sra_epi32",
			"-ccgo-hide", "_mm256_mask_sra_epi64",
			"-ccgo-hide", "_mm256_mask_srav_epi16",
			"-ccgo-hide", "_mm256_mask_srav_epi32",
			"-ccgo-hide", "_mm256_mask_srav_epi64",
			"-ccgo-hide", "_mm256_mask_srl_epi16",
			"-ccgo-hide", "_mm256_mask_srl_epi32",
			"-ccgo-hide", "_mm256_mask_srl_epi64",
			"-ccgo-hide", "_mm256_mask_srlv_epi16",
			"-ccgo-hide", "_mm256_mask_srlv_epi32",
			"-ccgo-hide", "_mm256_mask_srlv_epi64",
			"-ccgo-hide", "_mm256_mask_store_epi32",
			"-ccgo-hide", "_mm256_mask_store_epi64",
			"-ccgo-hide", "_mm256_mask_store_pd",
			"-ccgo-hide", "_mm256_mask_store_ps",
			"-ccgo-hide", "_mm256_mask_storeu_epi16",
			"-ccgo-hide", "_mm256_mask_storeu_epi32",
			"-ccgo-hide", "_mm256_mask_storeu_epi64",
			"-ccgo-hide", "_mm256_mask_storeu_epi8",
			"-ccgo-hide", "_mm256_mask_storeu_pd",
			"-ccgo-hide", "_mm256_mask_storeu_ps",
			"-ccgo-hide", "_mm256_mask_sub_epi16",
			"-ccgo-hide", "_mm256_mask_sub_epi32",
			"-ccgo-hide", "_mm256_mask_sub_epi64",
			"-ccgo-hide", "_mm256_mask_sub_epi8",
			"-ccgo-hide", "_mm256_mask_sub_pd",
			"-ccgo-hide", "_mm256_mask_sub_ps",
			"-ccgo-hide", "_mm256_mask_subs_epi16",
			"-ccgo-hide", "_mm256_mask_subs_epi8",
			"-ccgo-hide", "_mm256_mask_subs_epu16",
			"-ccgo-hide", "_mm256_mask_subs_epu8",
			"-ccgo-hide", "_mm256_mask_test_epi16_mask",
			"-ccgo-hide", "_mm256_mask_test_epi32_mask",
			"-ccgo-hide", "_mm256_mask_test_epi64_mask",
			"-ccgo-hide", "_mm256_mask_test_epi8_mask",
			"-ccgo-hide", "_mm256_mask_testn_epi16_mask",
			"-ccgo-hide", "_mm256_mask_testn_epi32_mask",
			"-ccgo-hide", "_mm256_mask_testn_epi64_mask",
			"-ccgo-hide", "_mm256_mask_testn_epi8_mask",
			"-ccgo-hide", "_mm256_mask_unpackhi_epi16",
			"-ccgo-hide", "_mm256_mask_unpackhi_epi32",
			"-ccgo-hide", "_mm256_mask_unpackhi_epi64",
			"-ccgo-hide", "_mm256_mask_unpackhi_epi8",
			"-ccgo-hide", "_mm256_mask_unpackhi_pd",
			"-ccgo-hide", "_mm256_mask_unpackhi_ps",
			"-ccgo-hide", "_mm256_mask_unpacklo_epi16",
			"-ccgo-hide", "_mm256_mask_unpacklo_epi32",
			"-ccgo-hide", "_mm256_mask_unpacklo_epi64",
			"-ccgo-hide", "_mm256_mask_unpacklo_epi8",
			"-ccgo-hide", "_mm256_mask_unpacklo_pd",
			"-ccgo-hide", "_mm256_mask_unpacklo_ps",
			"-ccgo-hide", "_mm256_mask_xor_epi32",
			"-ccgo-hide", "_mm256_mask_xor_epi64",
			"-ccgo-hide", "_mm256_mask_xor_pd",
			"-ccgo-hide", "_mm256_mask_xor_ps",
			"-ccgo-hide", "_mm256_maskload_epi32",
			"-ccgo-hide", "_mm256_maskload_epi64",
			"-ccgo-hide", "_mm256_maskload_pd",
			"-ccgo-hide", "_mm256_maskload_ps",
			"-ccgo-hide", "_mm256_maskstore_epi32",
			"-ccgo-hide", "_mm256_maskstore_epi64",
			"-ccgo-hide", "_mm256_maskstore_pd",
			"-ccgo-hide", "_mm256_maskstore_ps",
			"-ccgo-hide", "_mm256_maskz_abs_epi16",
			"-ccgo-hide", "_mm256_maskz_abs_epi32",
			"-ccgo-hide", "_mm256_maskz_abs_epi64",
			"-ccgo-hide", "_mm256_maskz_abs_epi8",
			"-ccgo-hide", "_mm256_maskz_add_epi16",
			"-ccgo-hide", "_mm256_maskz_add_epi32",
			"-ccgo-hide", "_mm256_maskz_add_epi64",
			"-ccgo-hide", "_mm256_maskz_add_epi8",
			"-ccgo-hide", "_mm256_maskz_add_pd",
			"-ccgo-hide", "_mm256_maskz_add_ps",
			"-ccgo-hide", "_mm256_maskz_adds_epi16",
			"-ccgo-hide", "_mm256_maskz_adds_epi8",
			"-ccgo-hide", "_mm256_maskz_adds_epu16",
			"-ccgo-hide", "_mm256_maskz_adds_epu8",
			"-ccgo-hide", "_mm256_maskz_and_epi32",
			"-ccgo-hide", "_mm256_maskz_and_epi64",
			"-ccgo-hide", "_mm256_maskz_and_pd",
			"-ccgo-hide", "_mm256_maskz_and_ps",
			"-ccgo-hide", "_mm256_maskz_andnot_epi32",
			"-ccgo-hide", "_mm256_maskz_andnot_epi64",
			"-ccgo-hide", "_mm256_maskz_andnot_pd",
			"-ccgo-hide", "_mm256_maskz_andnot_ps",
			"-ccgo-hide", "_mm256_maskz_avg_epu16",
			"-ccgo-hide", "_mm256_maskz_avg_epu8",
			"-ccgo-hide", "_mm256_maskz_broadcast_f32x2",
			"-ccgo-hide", "_mm256_maskz_broadcast_f32x4",
			"-ccgo-hide", "_mm256_maskz_broadcast_f64x2",
			"-ccgo-hide", "_mm256_maskz_broadcast_i32x2",
			"-ccgo-hide", "_mm256_maskz_broadcast_i32x4",
			"-ccgo-hide", "_mm256_maskz_broadcast_i64x2",
			"-ccgo-hide", "_mm256_maskz_broadcastb_epi8",
			"-ccgo-hide", "_mm256_maskz_broadcastd_epi32",
			"-ccgo-hide", "_mm256_maskz_broadcastq_epi64",
			"-ccgo-hide", "_mm256_maskz_broadcastsd_pd",
			"-ccgo-hide", "_mm256_maskz_broadcastss_ps",
			"-ccgo-hide", "_mm256_maskz_broadcastw_epi16",
			"-ccgo-hide", "_mm256_maskz_compress_epi16",
			"-ccgo-hide", "_mm256_maskz_compress_epi32",
			"-ccgo-hide", "_mm256_maskz_compress_epi64",
			"-ccgo-hide", "_mm256_maskz_compress_epi8",
			"-ccgo-hide", "_mm256_maskz_compress_pd",
			"-ccgo-hide", "_mm256_maskz_compress_ps",
			"-ccgo-hide", "_mm256_maskz_conflict_epi32",
			"-ccgo-hide", "_mm256_maskz_conflict_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtepi16_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtepi16_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtepi16_epi8",
			"-ccgo-hide", "_mm256_maskz_cvtepi32_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtepi32_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtepi32_epi8",
			"-ccgo-hide", "_mm256_maskz_cvtepi32_pd",
			"-ccgo-hide", "_mm256_maskz_cvtepi32_ps",
			"-ccgo-hide", "_mm256_maskz_cvtepi64_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtepi64_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtepi64_epi8",
			"-ccgo-hide", "_mm256_maskz_cvtepi64_pd",
			"-ccgo-hide", "_mm256_maskz_cvtepi64_ps",
			"-ccgo-hide", "_mm256_maskz_cvtepi8_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtepi8_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtepi8_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtepu16_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtepu16_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtepu32_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtepu32_pd",
			"-ccgo-hide", "_mm256_maskz_cvtepu32_ps",
			"-ccgo-hide", "_mm256_maskz_cvtepu64_pd",
			"-ccgo-hide", "_mm256_maskz_cvtepu64_ps",
			"-ccgo-hide", "_mm256_maskz_cvtepu8_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtepu8_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtepu8_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtpd_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtpd_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtpd_epu32",
			"-ccgo-hide", "_mm256_maskz_cvtpd_epu64",
			"-ccgo-hide", "_mm256_maskz_cvtpd_ps",
			"-ccgo-hide", "_mm256_maskz_cvtph_ps",
			"-ccgo-hide", "_mm256_maskz_cvtps_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtps_epi64",
			"-ccgo-hide", "_mm256_maskz_cvtps_epu32",
			"-ccgo-hide", "_mm256_maskz_cvtps_epu64",
			"-ccgo-hide", "_mm256_maskz_cvtps_pd",
			"-ccgo-hide", "_mm256_maskz_cvtsepi16_epi8",
			"-ccgo-hide", "_mm256_maskz_cvtsepi32_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtsepi32_epi8",
			"-ccgo-hide", "_mm256_maskz_cvtsepi64_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtsepi64_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtsepi64_epi8",
			"-ccgo-hide", "_mm256_maskz_cvttpd_epi32",
			"-ccgo-hide", "_mm256_maskz_cvttpd_epi64",
			"-ccgo-hide", "_mm256_maskz_cvttpd_epu32",
			"-ccgo-hide", "_mm256_maskz_cvttpd_epu64",
			"-ccgo-hide", "_mm256_maskz_cvttps_epi32",
			"-ccgo-hide", "_mm256_maskz_cvttps_epi64",
			"-ccgo-hide", "_mm256_maskz_cvttps_epu32",
			"-ccgo-hide", "_mm256_maskz_cvttps_epu64",
			"-ccgo-hide", "_mm256_maskz_cvtusepi16_epi8",
			"-ccgo-hide", "_mm256_maskz_cvtusepi32_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtusepi32_epi8",
			"-ccgo-hide", "_mm256_maskz_cvtusepi64_epi16",
			"-ccgo-hide", "_mm256_maskz_cvtusepi64_epi32",
			"-ccgo-hide", "_mm256_maskz_cvtusepi64_epi8",
			"-ccgo-hide", "_mm256_maskz_div_pd",
			"-ccgo-hide", "_mm256_maskz_div_ps",
			"-ccgo-hide", "_mm256_maskz_dpbusd_epi32",
			"-ccgo-hide", "_mm256_maskz_dpbusds_epi32",
			"-ccgo-hide", "_mm256_maskz_dpwssd_epi32",
			"-ccgo-hide", "_mm256_maskz_dpwssds_epi32",
			"-ccgo-hide", "_mm256_maskz_expand_epi16",
			"-ccgo-hide", "_mm256_maskz_expand_epi32",
			"-ccgo-hide", "_mm256_maskz_expand_epi64",
			"-ccgo-hide", "_mm256_maskz_expand_epi8",
			"-ccgo-hide", "_mm256_maskz_expand_pd",
			"-ccgo-hide", "_mm256_maskz_expand_ps",
			"-ccgo-hide", "_mm256_maskz_expandloadu_epi16",
			"-ccgo-hide", "_mm256_maskz_expandloadu_epi32",
			"-ccgo-hide", "_mm256_maskz_expandloadu_epi64",
			"-ccgo-hide", "_mm256_maskz_expandloadu_epi8",
			"-ccgo-hide", "_mm256_maskz_expandloadu_pd",
			"-ccgo-hide", "_mm256_maskz_expandloadu_ps",
			"-ccgo-hide", "_mm256_maskz_fmadd_pd",
			"-ccgo-hide", "_mm256_maskz_fmadd_ps",
			"-ccgo-hide", "_mm256_maskz_fmaddsub_pd",
			"-ccgo-hide", "_mm256_maskz_fmaddsub_ps",
			"-ccgo-hide", "_mm256_maskz_fmsub_pd",
			"-ccgo-hide", "_mm256_maskz_fmsub_ps",
			"-ccgo-hide", "_mm256_maskz_fmsubadd_pd",
			"-ccgo-hide", "_mm256_maskz_fmsubadd_ps",
			"-ccgo-hide", "_mm256_maskz_fnmadd_pd",
			"-ccgo-hide", "_mm256_maskz_fnmadd_ps",
			"-ccgo-hide", "_mm256_maskz_fnmsub_pd",
			"-ccgo-hide", "_mm256_maskz_fnmsub_ps",
			"-ccgo-hide", "_mm256_maskz_getexp_pd",
			"-ccgo-hide", "_mm256_maskz_getexp_ps",
			"-ccgo-hide", "_mm256_maskz_gf2p8mul_epi8",
			"-ccgo-hide", "_mm256_maskz_load_epi32",
			"-ccgo-hide", "_mm256_maskz_load_epi64",
			"-ccgo-hide", "_mm256_maskz_load_pd",
			"-ccgo-hide", "_mm256_maskz_load_ps",
			"-ccgo-hide", "_mm256_maskz_loadu_epi16",
			"-ccgo-hide", "_mm256_maskz_loadu_epi32",
			"-ccgo-hide", "_mm256_maskz_loadu_epi64",
			"-ccgo-hide", "_mm256_maskz_loadu_epi8",
			"-ccgo-hide", "_mm256_maskz_loadu_pd",
			"-ccgo-hide", "_mm256_maskz_loadu_ps",
			"-ccgo-hide", "_mm256_maskz_lzcnt_epi32",
			"-ccgo-hide", "_mm256_maskz_lzcnt_epi64",
			"-ccgo-hide", "_mm256_maskz_madd52hi_epu64",
			"-ccgo-hide", "_mm256_maskz_madd52lo_epu64",
			"-ccgo-hide", "_mm256_maskz_madd_epi16",
			"-ccgo-hide", "_mm256_maskz_maddubs_epi16",
			"-ccgo-hide", "_mm256_maskz_max_epi16",
			"-ccgo-hide", "_mm256_maskz_max_epi32",
			"-ccgo-hide", "_mm256_maskz_max_epi64",
			"-ccgo-hide", "_mm256_maskz_max_epi8",
			"-ccgo-hide", "_mm256_maskz_max_epu16",
			"-ccgo-hide", "_mm256_maskz_max_epu32",
			"-ccgo-hide", "_mm256_maskz_max_epu64",
			"-ccgo-hide", "_mm256_maskz_max_epu8",
			"-ccgo-hide", "_mm256_maskz_max_pd",
			"-ccgo-hide", "_mm256_maskz_max_ps",
			"-ccgo-hide", "_mm256_maskz_min_epi16",
			"-ccgo-hide", "_mm256_maskz_min_epi32",
			"-ccgo-hide", "_mm256_maskz_min_epi64",
			"-ccgo-hide", "_mm256_maskz_min_epi8",
			"-ccgo-hide", "_mm256_maskz_min_epu16",
			"-ccgo-hide", "_mm256_maskz_min_epu32",
			"-ccgo-hide", "_mm256_maskz_min_epu64",
			"-ccgo-hide", "_mm256_maskz_min_epu8",
			"-ccgo-hide", "_mm256_maskz_min_pd",
			"-ccgo-hide", "_mm256_maskz_min_ps",
			"-ccgo-hide", "_mm256_maskz_mov_epi16",
			"-ccgo-hide", "_mm256_maskz_mov_epi32",
			"-ccgo-hide", "_mm256_maskz_mov_epi64",
			"-ccgo-hide", "_mm256_maskz_mov_epi8",
			"-ccgo-hide", "_mm256_maskz_mov_pd",
			"-ccgo-hide", "_mm256_maskz_mov_ps",
			"-ccgo-hide", "_mm256_maskz_movedup_pd",
			"-ccgo-hide", "_mm256_maskz_movehdup_ps",
			"-ccgo-hide", "_mm256_maskz_moveldup_ps",
			"-ccgo-hide", "_mm256_maskz_mul_epi32",
			"-ccgo-hide", "_mm256_maskz_mul_epu32",
			"-ccgo-hide", "_mm256_maskz_mul_pd",
			"-ccgo-hide", "_mm256_maskz_mul_ps",
			"-ccgo-hide", "_mm256_maskz_mulhi_epi16",
			"-ccgo-hide", "_mm256_maskz_mulhi_epu16",
			"-ccgo-hide", "_mm256_maskz_mulhrs_epi16",
			"-ccgo-hide", "_mm256_maskz_mullo_epi16",
			"-ccgo-hide", "_mm256_maskz_mullo_epi32",
			"-ccgo-hide", "_mm256_maskz_mullo_epi64",
			"-ccgo-hide", "_mm256_maskz_multishift_epi64_epi8",
			"-ccgo-hide", "_mm256_maskz_or_epi32",
			"-ccgo-hide", "_mm256_maskz_or_epi64",
			"-ccgo-hide", "_mm256_maskz_or_pd",
			"-ccgo-hide", "_mm256_maskz_or_ps",
			"-ccgo-hide", "_mm256_maskz_packs_epi16",
			"-ccgo-hide", "_mm256_maskz_packs_epi32",
			"-ccgo-hide", "_mm256_maskz_packus_epi16",
			"-ccgo-hide", "_mm256_maskz_packus_epi32",
			"-ccgo-hide", "_mm256_maskz_permutevar_pd",
			"-ccgo-hide", "_mm256_maskz_permutevar_ps",
			"-ccgo-hide", "_mm256_maskz_permutex2var_epi16",
			"-ccgo-hide", "_mm256_maskz_permutex2var_epi32",
			"-ccgo-hide", "_mm256_maskz_permutex2var_epi64",
			"-ccgo-hide", "_mm256_maskz_permutex2var_epi8",
			"-ccgo-hide", "_mm256_maskz_permutex2var_pd",
			"-ccgo-hide", "_mm256_maskz_permutex2var_ps",
			"-ccgo-hide", "_mm256_maskz_permutexvar_epi16",
			"-ccgo-hide", "_mm256_maskz_permutexvar_epi32",
			"-ccgo-hide", "_mm256_maskz_permutexvar_epi64",
			"-ccgo-hide", "_mm256_maskz_permutexvar_epi8",
			"-ccgo-hide", "_mm256_maskz_permutexvar_pd",
			"-ccgo-hide", "_mm256_maskz_permutexvar_ps",
			"-ccgo-hide", "_mm256_maskz_popcnt_epi16",
			"-ccgo-hide", "_mm256_maskz_popcnt_epi32",
			"-ccgo-hide", "_mm256_maskz_popcnt_epi64",
			"-ccgo-hide", "_mm256_maskz_popcnt_epi8",
			"-ccgo-hide", "_mm256_maskz_rcp14_pd",
			"-ccgo-hide", "_mm256_maskz_rcp14_ps",
			"-ccgo-hide", "_mm256_maskz_rolv_epi32",
			"-ccgo-hide", "_mm256_maskz_rolv_epi64",
			"-ccgo-hide", "_mm256_maskz_rorv_epi32",
			"-ccgo-hide", "_mm256_maskz_rorv_epi64",
			"-ccgo-hide", "_mm256_maskz_rsqrt14_pd",
			"-ccgo-hide", "_mm256_maskz_rsqrt14_ps",
			"-ccgo-hide", "_mm256_maskz_scalef_pd",
			"-ccgo-hide", "_mm256_maskz_scalef_ps",
			"-ccgo-hide", "_mm256_maskz_set1_epi16",
			"-ccgo-hide", "_mm256_maskz_set1_epi32",
			"-ccgo-hide", "_mm256_maskz_set1_epi64",
			"-ccgo-hide", "_mm256_maskz_set1_epi8",
			"-ccgo-hide", "_mm256_maskz_shldv_epi16",
			"-ccgo-hide", "_mm256_maskz_shldv_epi32",
			"-ccgo-hide", "_mm256_maskz_shldv_epi64",
			"-ccgo-hide", "_mm256_maskz_shrdv_epi16",
			"-ccgo-hide", "_mm256_maskz_shrdv_epi32",
			"-ccgo-hide", "_mm256_maskz_shrdv_epi64",
			"-ccgo-hide", "_mm256_maskz_shuffle_epi8",
			"-ccgo-hide", "_mm256_maskz_sll_epi16",
			"-ccgo-hide", "_mm256_maskz_sll_epi32",
			"-ccgo-hide", "_mm256_maskz_sll_epi64",
			"-ccgo-hide", "_mm256_maskz_sllv_epi16",
			"-ccgo-hide", "_mm256_maskz_sllv_epi32",
			"-ccgo-hide", "_mm256_maskz_sllv_epi64",
			"-ccgo-hide", "_mm256_maskz_sqrt_pd",
			"-ccgo-hide", "_mm256_maskz_sqrt_ps",
			"-ccgo-hide", "_mm256_maskz_sra_epi16",
			"-ccgo-hide", "_mm256_maskz_sra_epi32",
			"-ccgo-hide", "_mm256_maskz_sra_epi64",
			"-ccgo-hide", "_mm256_maskz_srav_epi16",
			"-ccgo-hide", "_mm256_maskz_srav_epi32",
			"-ccgo-hide", "_mm256_maskz_srav_epi64",
			"-ccgo-hide", "_mm256_maskz_srl_epi16",
			"-ccgo-hide", "_mm256_maskz_srl_epi32",
			"-ccgo-hide", "_mm256_maskz_srl_epi64",
			"-ccgo-hide", "_mm256_maskz_srlv_epi16",
			"-ccgo-hide", "_mm256_maskz_srlv_epi32",
			"-ccgo-hide", "_mm256_maskz_srlv_epi64",
			"-ccgo-hide", "_mm256_maskz_sub_epi16",
			"-ccgo-hide", "_mm256_maskz_sub_epi32",
			"-ccgo-hide", "_mm256_maskz_sub_epi64",
			"-ccgo-hide", "_mm256_maskz_sub_epi8",
			"-ccgo-hide", "_mm256_maskz_sub_pd",
			"-ccgo-hide", "_mm256_maskz_sub_ps",
			"-ccgo-hide", "_mm256_maskz_subs_epi16",
			"-ccgo-hide", "_mm256_maskz_subs_epi8",
			"-ccgo-hide", "_mm256_maskz_subs_epu16",
			"-ccgo-hide", "_mm256_maskz_subs_epu8",
			"-ccgo-hide", "_mm256_maskz_unpackhi_epi16",
			"-ccgo-hide", "_mm256_maskz_unpackhi_epi32",
			"-ccgo-hide", "_mm256_maskz_unpackhi_epi64",
			"-ccgo-hide", "_mm256_maskz_unpackhi_epi8",
			"-ccgo-hide", "_mm256_maskz_unpackhi_pd",
			"-ccgo-hide", "_mm256_maskz_unpackhi_ps",
			"-ccgo-hide", "_mm256_maskz_unpacklo_epi16",
			"-ccgo-hide", "_mm256_maskz_unpacklo_epi32",
			"-ccgo-hide", "_mm256_maskz_unpacklo_epi64",
			"-ccgo-hide", "_mm256_maskz_unpacklo_epi8",
			"-ccgo-hide", "_mm256_maskz_unpacklo_pd",
			"-ccgo-hide", "_mm256_maskz_unpacklo_ps",
			"-ccgo-hide", "_mm256_maskz_xor_epi32",
			"-ccgo-hide", "_mm256_maskz_xor_epi64",
			"-ccgo-hide", "_mm256_maskz_xor_pd",
			"-ccgo-hide", "_mm256_maskz_xor_ps",
			"-ccgo-hide", "_mm256_max_epi16",
			"-ccgo-hide", "_mm256_max_epi32",
			"-ccgo-hide", "_mm256_max_epi64",
			"-ccgo-hide", "_mm256_max_epi8",
			"-ccgo-hide", "_mm256_max_epu16",
			"-ccgo-hide", "_mm256_max_epu32",
			"-ccgo-hide", "_mm256_max_epu64",
			"-ccgo-hide", "_mm256_max_epu8",
			"-ccgo-hide", "_mm256_max_pd",
			"-ccgo-hide", "_mm256_max_ps",
			"-ccgo-hide", "_mm256_min_epi16",
			"-ccgo-hide", "_mm256_min_epi32",
			"-ccgo-hide", "_mm256_min_epi64",
			"-ccgo-hide", "_mm256_min_epi8",
			"-ccgo-hide", "_mm256_min_epu16",
			"-ccgo-hide", "_mm256_min_epu32",
			"-ccgo-hide", "_mm256_min_epu64",
			"-ccgo-hide", "_mm256_min_epu8",
			"-ccgo-hide", "_mm256_min_pd",
			"-ccgo-hide", "_mm256_min_ps",
			"-ccgo-hide", "_mm256_movedup_pd",
			"-ccgo-hide", "_mm256_movehdup_ps",
			"-ccgo-hide", "_mm256_moveldup_ps",
			"-ccgo-hide", "_mm256_movemask_epi8",
			"-ccgo-hide", "_mm256_movemask_pd",
			"-ccgo-hide", "_mm256_movemask_ps",
			"-ccgo-hide", "_mm256_movepi16_mask",
			"-ccgo-hide", "_mm256_movepi32_mask",
			"-ccgo-hide", "_mm256_movepi64_mask",
			"-ccgo-hide", "_mm256_movepi8_mask",
			"-ccgo-hide", "_mm256_movm_epi16",
			"-ccgo-hide", "_mm256_movm_epi32",
			"-ccgo-hide", "_mm256_movm_epi64",
			"-ccgo-hide", "_mm256_movm_epi8",
			"-ccgo-hide", "_mm256_msub_pd",
			"-ccgo-hide", "_mm256_msub_ps",
			"-ccgo-hide", "_mm256_msubadd_pd",
			"-ccgo-hide", "_mm256_msubadd_ps",
			"-ccgo-hide", "_mm256_mul_epi32",
			"-ccgo-hide", "_mm256_mul_epu32",
			"-ccgo-hide", "_mm256_mul_pd",
			"-ccgo-hide", "_mm256_mul_ps",
			"-ccgo-hide", "_mm256_mulhi_epi16",
			"-ccgo-hide", "_mm256_mulhi_epu16",
			"-ccgo-hide", "_mm256_mulhrs_epi16",
			"-ccgo-hide", "_mm256_mullo_epi16",
			"-ccgo-hide", "_mm256_mullo_epi32",
			"-ccgo-hide", "_mm256_mullo_epi64",
			"-ccgo-hide", "_mm256_multishift_epi64_epi8",
			"-ccgo-hide", "_mm256_nmacc_pd",
			"-ccgo-hide", "_mm256_nmacc_ps",
			"-ccgo-hide", "_mm256_nmsub_pd",
			"-ccgo-hide", "_mm256_nmsub_ps",
			"-ccgo-hide", "_mm256_or_pd",
			"-ccgo-hide", "_mm256_or_ps",
			"-ccgo-hide", "_mm256_or_si256",
			"-ccgo-hide", "_mm256_packs_epi16",
			"-ccgo-hide", "_mm256_packs_epi32",
			"-ccgo-hide", "_mm256_packus_epi16",
			"-ccgo-hide", "_mm256_packus_epi32",
			"-ccgo-hide", "_mm256_permutevar8x32_epi32",
			"-ccgo-hide", "_mm256_permutevar8x32_ps",
			"-ccgo-hide", "_mm256_permutevar_pd",
			"-ccgo-hide", "_mm256_permutevar_ps",
			"-ccgo-hide", "_mm256_permutex2var_epi16",
			"-ccgo-hide", "_mm256_permutex2var_epi32",
			"-ccgo-hide", "_mm256_permutex2var_epi64",
			"-ccgo-hide", "_mm256_permutex2var_epi8",
			"-ccgo-hide", "_mm256_permutex2var_pd",
			"-ccgo-hide", "_mm256_permutex2var_ps",
			"-ccgo-hide", "_mm256_permutexvar_epi16",
			"-ccgo-hide", "_mm256_permutexvar_epi32",
			"-ccgo-hide", "_mm256_permutexvar_epi64",
			"-ccgo-hide", "_mm256_permutexvar_epi8",
			"-ccgo-hide", "_mm256_permutexvar_pd",
			"-ccgo-hide", "_mm256_popcnt_epi16",
			"-ccgo-hide", "_mm256_popcnt_epi32",
			"-ccgo-hide", "_mm256_popcnt_epi64",
			"-ccgo-hide", "_mm256_popcnt_epi8",
			"-ccgo-hide", "_mm256_rcp14_pd",
			"-ccgo-hide", "_mm256_rcp14_ps",
			"-ccgo-hide", "_mm256_rcp_ps",
			"-ccgo-hide", "_mm256_rolv_epi32",
			"-ccgo-hide", "_mm256_rolv_epi64",
			"-ccgo-hide", "_mm256_rorv_epi32",
			"-ccgo-hide", "_mm256_rorv_epi64",
			"-ccgo-hide", "_mm256_rsqrt14_pd",
			"-ccgo-hide", "_mm256_rsqrt14_ps",
			"-ccgo-hide", "_mm256_rsqrt_ps",
			"-ccgo-hide", "_mm256_sad_epu8",
			"-ccgo-hide", "_mm256_scalef_pd",
			"-ccgo-hide", "_mm256_scalef_ps",
			"-ccgo-hide", "_mm256_set1_epi16",
			"-ccgo-hide", "_mm256_set1_epi32",
			"-ccgo-hide", "_mm256_set1_epi64x",
			"-ccgo-hide", "_mm256_set1_epi8",
			"-ccgo-hide", "_mm256_set1_pd",
			"-ccgo-hide", "_mm256_set1_ps",
			"-ccgo-hide", "_mm256_set_epi16",
			"-ccgo-hide", "_mm256_set_epi32",
			"-ccgo-hide", "_mm256_set_epi64x",
			"-ccgo-hide", "_mm256_set_epi8",
			"-ccgo-hide", "_mm256_set_m128",
			"-ccgo-hide", "_mm256_set_m128d",
			"-ccgo-hide", "_mm256_set_m128i",
			"-ccgo-hide", "_mm256_set_pd",
			"-ccgo-hide", "_mm256_set_ps",
			"-ccgo-hide", "_mm256_setr_epi16",
			"-ccgo-hide", "_mm256_setr_epi32",
			"-ccgo-hide", "_mm256_setr_epi64x",
			"-ccgo-hide", "_mm256_setr_epi8",
			"-ccgo-hide", "_mm256_setr_m128",
			"-ccgo-hide", "_mm256_setr_m128d",
			"-ccgo-hide", "_mm256_setr_m128i",
			"-ccgo-hide", "_mm256_setr_pd",
			"-ccgo-hide", "_mm256_setr_ps",
			"-ccgo-hide", "_mm256_setzero_pd",
			"-ccgo-hide", "_mm256_setzero_ps",
			"-ccgo-hide", "_mm256_setzero_si256",
			"-ccgo-hide", "_mm256_shldv_epi16",
			"-ccgo-hide", "_mm256_shldv_epi32",
			"-ccgo-hide", "_mm256_shldv_epi64",
			"-ccgo-hide", "_mm256_shrdv_epi16",
			"-ccgo-hide", "_mm256_shrdv_epi32",
			"-ccgo-hide", "_mm256_shrdv_epi64",
			"-ccgo-hide", "_mm256_shuffle_epi8",
			"-ccgo-hide", "_mm256_sign_epi16",
			"-ccgo-hide", "_mm256_sign_epi32",
			"-ccgo-hide", "_mm256_sign_epi8",
			"-ccgo-hide", "_mm256_sll_epi16",
			"-ccgo-hide", "_mm256_sll_epi32",
			"-ccgo-hide", "_mm256_sll_epi64",
			"-ccgo-hide", "_mm256_slli_epi16",
			"-ccgo-hide", "_mm256_slli_epi32",
			"-ccgo-hide", "_mm256_slli_epi64",
			"-ccgo-hide", "_mm256_sllv_epi16",
			"-ccgo-hide", "_mm256_sllv_epi32",
			"-ccgo-hide", "_mm256_sllv_epi64",
			"-ccgo-hide", "_mm256_sqrt_pd",
			"-ccgo-hide", "_mm256_sqrt_ps",
			"-ccgo-hide", "_mm256_sra_epi16",
			"-ccgo-hide", "_mm256_sra_epi32",
			"-ccgo-hide", "_mm256_sra_epi64",
			"-ccgo-hide", "_mm256_srai_epi16",
			"-ccgo-hide", "_mm256_srai_epi32",
			"-ccgo-hide", "_mm256_srav_epi16",
			"-ccgo-hide", "_mm256_srav_epi32",
			"-ccgo-hide", "_mm256_srav_epi64",
			"-ccgo-hide", "_mm256_srl_epi16",
			"-ccgo-hide", "_mm256_srl_epi32",
			"-ccgo-hide", "_mm256_srl_epi64",
			"-ccgo-hide", "_mm256_srli_epi16",
			"-ccgo-hide", "_mm256_srli_epi32",
			"-ccgo-hide", "_mm256_srli_epi64",
			"-ccgo-hide", "_mm256_srlv_epi16",
			"-ccgo-hide", "_mm256_srlv_epi32",
			"-ccgo-hide", "_mm256_srlv_epi64",
			"-ccgo-hide", "_mm256_store_epi64",
			"-ccgo-hide", "_mm256_store_pd",
			"-ccgo-hide", "_mm256_store_ps",
			"-ccgo-hide", "_mm256_store_si256",
			"-ccgo-hide", "_mm256_storeu_pd",
			"-ccgo-hide", "_mm256_storeu_ps",
			"-ccgo-hide", "_mm256_storeu_si256",
			"-ccgo-hide", "_mm256_stream_load_si256",
			"-ccgo-hide", "_mm256_stream_pd",
			"-ccgo-hide", "_mm256_stream_ps",
			"-ccgo-hide", "_mm256_stream_si256",
			"-ccgo-hide", "_mm256_sub_epi16",
			"-ccgo-hide", "_mm256_sub_epi32",
			"-ccgo-hide", "_mm256_sub_epi64",
			"-ccgo-hide", "_mm256_sub_epi8",
			"-ccgo-hide", "_mm256_sub_pd",
			"-ccgo-hide", "_mm256_sub_ps",
			"-ccgo-hide", "_mm256_subs_epi16",
			"-ccgo-hide", "_mm256_subs_epi8",
			"-ccgo-hide", "_mm256_subs_epu16",
			"-ccgo-hide", "_mm256_subs_epu8",
			"-ccgo-hide", "_mm256_test_epi16_mask",
			"-ccgo-hide", "_mm256_test_epi32_mask",
			"-ccgo-hide", "_mm256_test_epi64_mask",
			"-ccgo-hide", "_mm256_test_epi8_mask",
			"-ccgo-hide", "_mm256_testc_pd",
			"-ccgo-hide", "_mm256_testc_ps",
			"-ccgo-hide", "_mm256_testc_si256",
			"-ccgo-hide", "_mm256_testn_epi16_mask",
			"-ccgo-hide", "_mm256_testn_epi32_mask",
			"-ccgo-hide", "_mm256_testn_epi64_mask",
			"-ccgo-hide", "_mm256_testn_epi8_mask",
			"-ccgo-hide", "_mm256_testnzc_pd",
			"-ccgo-hide", "_mm256_testnzc_ps",
			"-ccgo-hide", "_mm256_testnzc_si256",
			"-ccgo-hide", "_mm256_testz_pd",
			"-ccgo-hide", "_mm256_testz_ps",
			"-ccgo-hide", "_mm256_testz_si256",
			"-ccgo-hide", "_mm256_undefined_pd",
			"-ccgo-hide", "_mm256_undefined_ps",
			"-ccgo-hide", "_mm256_undefined_si256",
			"-ccgo-hide", "_mm256_unpackhi_epi16",
			"-ccgo-hide", "_mm256_unpackhi_epi32",
			"-ccgo-hide", "_mm256_unpackhi_epi64",
			"-ccgo-hide", "_mm256_unpackhi_epi8",
			"-ccgo-hide", "_mm256_unpackhi_pd",
			"-ccgo-hide", "_mm256_unpackhi_ps",
			"-ccgo-hide", "_mm256_unpacklo_epi16",
			"-ccgo-hide", "_mm256_unpacklo_epi32",
			"-ccgo-hide", "_mm256_unpacklo_epi64",
			"-ccgo-hide", "_mm256_unpacklo_epi8",
			"-ccgo-hide", "_mm256_unpacklo_pd",
			"-ccgo-hide", "_mm256_unpacklo_ps",
			"-ccgo-hide", "_mm256_xor_pd",
			"-ccgo-hide", "_mm256_xor_ps",
			"-ccgo-hide", "_mm256_xor_si256",
			"-ccgo-hide", "_mm256_zeroall",
			"-ccgo-hide", "_mm256_zeroupper",
			"-ccgo-hide", "_mm512_4dpwssd_epi32",
			"-ccgo-hide", "_mm512_4dpwssds_epi32",
			"-ccgo-hide", "_mm512_4fmadd_ps",
			"-ccgo-hide", "_mm512_4fnmadd_ps",
			"-ccgo-hide", "_mm512_abs_epi16",
			"-ccgo-hide", "_mm512_abs_epi32",
			"-ccgo-hide", "_mm512_abs_epi64",
			"-ccgo-hide", "_mm512_abs_epi8",
			"-ccgo-hide", "_mm512_abs_pd",
			"-ccgo-hide", "_mm512_abs_ps",
			"-ccgo-hide", "_mm512_add_epi16",
			"-ccgo-hide", "_mm512_add_epi32",
			"-ccgo-hide", "_mm512_add_epi64",
			"-ccgo-hide", "_mm512_add_epi8",
			"-ccgo-hide", "_mm512_add_pd",
			"-ccgo-hide", "_mm512_add_ps",
			"-ccgo-hide", "_mm512_adds_epi16",
			"-ccgo-hide", "_mm512_adds_epi8",
			"-ccgo-hide", "_mm512_adds_epu16",
			"-ccgo-hide", "_mm512_adds_epu8",
			"-ccgo-hide", "_mm512_aesdec_epi128",
			"-ccgo-hide", "_mm512_aesdeclast_epi128",
			"-ccgo-hide", "_mm512_aesenc_epi128",
			"-ccgo-hide", "_mm512_aesenclast_epi128",
			"-ccgo-hide", "_mm512_and_epi32",
			"-ccgo-hide", "_mm512_and_epi64",
			"-ccgo-hide", "_mm512_and_pd",
			"-ccgo-hide", "_mm512_and_ps",
			"-ccgo-hide", "_mm512_and_si512",
			"-ccgo-hide", "_mm512_andnot_epi32",
			"-ccgo-hide", "_mm512_andnot_epi64",
			"-ccgo-hide", "_mm512_andnot_pd",
			"-ccgo-hide", "_mm512_andnot_ps",
			"-ccgo-hide", "_mm512_andnot_si512",
			"-ccgo-hide", "_mm512_avg_epu16",
			"-ccgo-hide", "_mm512_avg_epu8",
			"-ccgo-hide", "_mm512_bitshuffle_epi64_mask",
			"-ccgo-hide", "_mm512_broadcast_f32x2",
			"-ccgo-hide", "_mm512_broadcast_f32x4",
			"-ccgo-hide", "_mm512_broadcast_f32x8",
			"-ccgo-hide", "_mm512_broadcast_f64x2",
			"-ccgo-hide", "_mm512_broadcast_f64x4",
			"-ccgo-hide", "_mm512_broadcast_i32x2",
			"-ccgo-hide", "_mm512_broadcast_i32x4",
			"-ccgo-hide", "_mm512_broadcast_i32x8",
			"-ccgo-hide", "_mm512_broadcast_i64x2",
			"-ccgo-hide", "_mm512_broadcast_i64x4",
			"-ccgo-hide", "_mm512_broadcastb_epi8",
			"-ccgo-hide", "_mm512_broadcastd_epi32",
			"-ccgo-hide", "_mm512_broadcastmb_epi64",
			"-ccgo-hide", "_mm512_broadcastmw_epi32",
			"-ccgo-hide", "_mm512_broadcastq_epi64",
			"-ccgo-hide", "_mm512_broadcastsd_pd",
			"-ccgo-hide", "_mm512_broadcastss_ps",
			"-ccgo-hide", "_mm512_broadcastw_epi16",
			"-ccgo-hide", "_mm512_castpd128_pd512",
			"-ccgo-hide", "_mm512_castpd256_pd512",
			"-ccgo-hide", "_mm512_castpd512_pd128",
			"-ccgo-hide", "_mm512_castpd512_pd256",
			"-ccgo-hide", "_mm512_castpd_ps",
			"-ccgo-hide", "_mm512_castpd_si512",
			"-ccgo-hide", "_mm512_castps128_ps512",
			"-ccgo-hide", "_mm512_castps256_ps512",
			"-ccgo-hide", "_mm512_castps512_ps128",
			"-ccgo-hide", "_mm512_castps512_ps256",
			"-ccgo-hide", "_mm512_castps_pd",
			"-ccgo-hide", "_mm512_castps_si512",
			"-ccgo-hide", "_mm512_castsi128_si512",
			"-ccgo-hide", "_mm512_castsi256_si512",
			"-ccgo-hide", "_mm512_castsi512_pd",
			"-ccgo-hide", "_mm512_castsi512_ps",
			"-ccgo-hide", "_mm512_castsi512_si128",
			"-ccgo-hide", "_mm512_castsi512_si256",
			"-ccgo-hide", "_mm512_ceil_pd",
			"-ccgo-hide", "_mm512_ceil_ps",
			"-ccgo-hide", "_mm512_cmpeq_epi16_mask",
			"-ccgo-hide", "_mm512_cmpeq_epi32_mask",
			"-ccgo-hide", "_mm512_cmpeq_epi64_mask",
			"-ccgo-hide", "_mm512_cmpeq_epi8_mask",
			"-ccgo-hide", "_mm512_cmpeq_epu16_mask",
			"-ccgo-hide", "_mm512_cmpeq_epu32_mask",
			"-ccgo-hide", "_mm512_cmpeq_epu64_mask",
			"-ccgo-hide", "_mm512_cmpeq_epu8_mask",
			"-ccgo-hide", "_mm512_cmpge_epi16_mask",
			"-ccgo-hide", "_mm512_cmpge_epi32_mask",
			"-ccgo-hide", "_mm512_cmpge_epi64_mask",
			"-ccgo-hide", "_mm512_cmpge_epi8_mask",
			"-ccgo-hide", "_mm512_cmpge_epu16_mask",
			"-ccgo-hide", "_mm512_cmpge_epu32_mask",
			"-ccgo-hide", "_mm512_cmpge_epu64_mask",
			"-ccgo-hide", "_mm512_cmpge_epu8_mask",
			"-ccgo-hide", "_mm512_cmpgt_epi16_mask",
			"-ccgo-hide", "_mm512_cmpgt_epi32_mask",
			"-ccgo-hide", "_mm512_cmpgt_epi64_mask",
			"-ccgo-hide", "_mm512_cmpgt_epi8_mask",
			"-ccgo-hide", "_mm512_cmpgt_epu16_mask",
			"-ccgo-hide", "_mm512_cmpgt_epu32_mask",
			"-ccgo-hide", "_mm512_cmpgt_epu64_mask",
			"-ccgo-hide", "_mm512_cmpgt_epu8_mask",
			"-ccgo-hide", "_mm512_cmple_epi16_mask",
			"-ccgo-hide", "_mm512_cmple_epi32_mask",
			"-ccgo-hide", "_mm512_cmple_epi64_mask",
			"-ccgo-hide", "_mm512_cmple_epi8_mask",
			"-ccgo-hide", "_mm512_cmple_epu16_mask",
			"-ccgo-hide", "_mm512_cmple_epu32_mask",
			"-ccgo-hide", "_mm512_cmple_epu64_mask",
			"-ccgo-hide", "_mm512_cmple_epu8_mask",
			"-ccgo-hide", "_mm512_cmplt_epi16_mask",
			"-ccgo-hide", "_mm512_cmplt_epi32_mask",
			"-ccgo-hide", "_mm512_cmplt_epi64_mask",
			"-ccgo-hide", "_mm512_cmplt_epi8_mask",
			"-ccgo-hide", "_mm512_cmplt_epu16_mask",
			"-ccgo-hide", "_mm512_cmplt_epu32_mask",
			"-ccgo-hide", "_mm512_cmplt_epu64_mask",
			"-ccgo-hide", "_mm512_cmplt_epu8_mask",
			"-ccgo-hide", "_mm512_cmpneq_epi16_mask",
			"-ccgo-hide", "_mm512_cmpneq_epi32_mask",
			"-ccgo-hide", "_mm512_cmpneq_epi64_mask",
			"-ccgo-hide", "_mm512_cmpneq_epi8_mask",
			"-ccgo-hide", "_mm512_cmpneq_epu16_mask",
			"-ccgo-hide", "_mm512_cmpneq_epu32_mask",
			"-ccgo-hide", "_mm512_cmpneq_epu64_mask",
			"-ccgo-hide", "_mm512_cmpneq_epu8_mask",
			"-ccgo-hide", "_mm512_conflict_epi32",
			"-ccgo-hide", "_mm512_conflict_epi64",
			"-ccgo-hide", "_mm512_cvtepi16_epi32",
			"-ccgo-hide", "_mm512_cvtepi16_epi64",
			"-ccgo-hide", "_mm512_cvtepi16_epi8",
			"-ccgo-hide", "_mm512_cvtepi32_epi16",
			"-ccgo-hide", "_mm512_cvtepi32_epi64",
			"-ccgo-hide", "_mm512_cvtepi32_epi8",
			"-ccgo-hide", "_mm512_cvtepi32_pd",
			"-ccgo-hide", "_mm512_cvtepi32_ps",
			"-ccgo-hide", "_mm512_cvtepi64_epi16",
			"-ccgo-hide", "_mm512_cvtepi64_epi32",
			"-ccgo-hide", "_mm512_cvtepi64_epi8",
			"-ccgo-hide", "_mm512_cvtepi64_pd",
			"-ccgo-hide", "_mm512_cvtepi64_ps",
			"-ccgo-hide", "_mm512_cvtepi8_epi16",
			"-ccgo-hide", "_mm512_cvtepi8_epi32",
			"-ccgo-hide", "_mm512_cvtepi8_epi64",
			"-ccgo-hide", "_mm512_cvtepu16_epi32",
			"-ccgo-hide", "_mm512_cvtepu16_epi64",
			"-ccgo-hide", "_mm512_cvtepu32_epi64",
			"-ccgo-hide", "_mm512_cvtepu32_pd",
			"-ccgo-hide", "_mm512_cvtepu32_ps",
			"-ccgo-hide", "_mm512_cvtepu64_pd",
			"-ccgo-hide", "_mm512_cvtepu64_ps",
			"-ccgo-hide", "_mm512_cvtepu8_epi16",
			"-ccgo-hide", "_mm512_cvtepu8_epi32",
			"-ccgo-hide", "_mm512_cvtepu8_epi64",
			"-ccgo-hide", "_mm512_cvtpd_epi32",
			"-ccgo-hide", "_mm512_cvtpd_epi64",
			"-ccgo-hide", "_mm512_cvtpd_epu32",
			"-ccgo-hide", "_mm512_cvtpd_epu64",
			"-ccgo-hide", "_mm512_cvtpd_ps",
			"-ccgo-hide", "_mm512_cvtph_ps",
			"-ccgo-hide", "_mm512_cvtps_epi32",
			"-ccgo-hide", "_mm512_cvtps_epi64",
			"-ccgo-hide", "_mm512_cvtps_epu32",
			"-ccgo-hide", "_mm512_cvtps_epu64",
			"-ccgo-hide", "_mm512_cvtps_pd",
			"-ccgo-hide", "_mm512_cvtsd_f64",
			"-ccgo-hide", "_mm512_cvtsepi16_epi8",
			"-ccgo-hide", "_mm512_cvtsepi32_epi16",
			"-ccgo-hide", "_mm512_cvtsepi32_epi8",
			"-ccgo-hide", "_mm512_cvtsepi64_epi16",
			"-ccgo-hide", "_mm512_cvtsepi64_epi32",
			"-ccgo-hide", "_mm512_cvtsepi64_epi8",
			"-ccgo-hide", "_mm512_cvtss_f32",
			"-ccgo-hide", "_mm512_cvttpd_epi32",
			"-ccgo-hide", "_mm512_cvttpd_epi64",
			"-ccgo-hide", "_mm512_cvttpd_epu32",
			"-ccgo-hide", "_mm512_cvttpd_epu64",
			"-ccgo-hide", "_mm512_cvttps_epi32",
			"-ccgo-hide", "_mm512_cvttps_epi64",
			"-ccgo-hide", "_mm512_cvttps_epu32",
			"-ccgo-hide", "_mm512_cvttps_epu64",
			"-ccgo-hide", "_mm512_cvtusepi16_epi8",
			"-ccgo-hide", "_mm512_cvtusepi32_epi16",
			"-ccgo-hide", "_mm512_cvtusepi32_epi8",
			"-ccgo-hide", "_mm512_cvtusepi64_epi16",
			"-ccgo-hide", "_mm512_cvtusepi64_epi32",
			"-ccgo-hide", "_mm512_cvtusepi64_epi8",
			"-ccgo-hide", "_mm512_div_pd",
			"-ccgo-hide", "_mm512_div_ps",
			"-ccgo-hide", "_mm512_dpbusd_epi32",
			"-ccgo-hide", "_mm512_dpbusds_epi32",
			"-ccgo-hide", "_mm512_dpwssd_epi32",
			"-ccgo-hide", "_mm512_dpwssds_epi32",
			"-ccgo-hide", "_mm512_floor_pd",
			"-ccgo-hide", "_mm512_floor_ps",
			"-ccgo-hide", "_mm512_fmadd_pd",
			"-ccgo-hide", "_mm512_fmadd_ps",
			"-ccgo-hide", "_mm512_fmaddsub_pd",
			"-ccgo-hide", "_mm512_fmaddsub_ps",
			"-ccgo-hide", "_mm512_fmsub_pd",
			"-ccgo-hide", "_mm512_fmsub_ps",
			"-ccgo-hide", "_mm512_fmsubadd_pd",
			"-ccgo-hide", "_mm512_fmsubadd_ps",
			"-ccgo-hide", "_mm512_fnmadd_pd",
			"-ccgo-hide", "_mm512_fnmadd_ps",
			"-ccgo-hide", "_mm512_fnmsub_pd",
			"-ccgo-hide", "_mm512_fnmsub_ps",
			"-ccgo-hide", "_mm512_gf2p8mul_epi8",
			"-ccgo-hide", "_mm512_int2mask",
			"-ccgo-hide", "_mm512_kand",
			"-ccgo-hide", "_mm512_kandn",
			"-ccgo-hide", "_mm512_kmov",
			"-ccgo-hide", "_mm512_knot",
			"-ccgo-hide", "_mm512_kor",
			"-ccgo-hide", "_mm512_kortestc",
			"-ccgo-hide", "_mm512_kortestz",
			"-ccgo-hide", "_mm512_kunpackb",
			"-ccgo-hide", "_mm512_kunpackd",
			"-ccgo-hide", "_mm512_kunpackw",
			"-ccgo-hide", "_mm512_kxnor",
			"-ccgo-hide", "_mm512_kxor",
			"-ccgo-hide", "_mm512_load_epi32",
			"-ccgo-hide", "_mm512_load_epi64",
			"-ccgo-hide", "_mm512_load_pd",
			"-ccgo-hide", "_mm512_load_ps",
			"-ccgo-hide", "_mm512_load_si512",
			"-ccgo-hide", "_mm512_loadu_pd",
			"-ccgo-hide", "_mm512_loadu_ps",
			"-ccgo-hide", "_mm512_loadu_si512",
			"-ccgo-hide", "_mm512_lzcnt_epi32",
			"-ccgo-hide", "_mm512_lzcnt_epi64",
			"-ccgo-hide", "_mm512_madd52hi_epu64",
			"-ccgo-hide", "_mm512_madd52lo_epu64",
			"-ccgo-hide", "_mm512_madd_epi16",
			"-ccgo-hide", "_mm512_maddubs_epi16",
			"-ccgo-hide", "_mm512_mask2_permutex2var_epi16",
			"-ccgo-hide", "_mm512_mask2_permutex2var_epi32",
			"-ccgo-hide", "_mm512_mask2_permutex2var_epi64",
			"-ccgo-hide", "_mm512_mask2_permutex2var_epi8",
			"-ccgo-hide", "_mm512_mask2_permutex2var_pd",
			"-ccgo-hide", "_mm512_mask2_permutex2var_ps",
			"-ccgo-hide", "_mm512_mask2int",
			"-ccgo-hide", "_mm512_mask3_fmadd_pd",
			"-ccgo-hide", "_mm512_mask3_fmadd_ps",
			"-ccgo-hide", "_mm512_mask3_fmaddsub_pd",
			"-ccgo-hide", "_mm512_mask3_fmaddsub_ps",
			"-ccgo-hide", "_mm512_mask3_fmsub_pd",
			"-ccgo-hide", "_mm512_mask3_fmsub_ps",
			"-ccgo-hide", "_mm512_mask3_fmsubadd_pd",
			"-ccgo-hide", "_mm512_mask3_fmsubadd_ps",
			"-ccgo-hide", "_mm512_mask3_fnmadd_pd",
			"-ccgo-hide", "_mm512_mask3_fnmadd_ps",
			"-ccgo-hide", "_mm512_mask3_fnmsub_pd",
			"-ccgo-hide", "_mm512_mask3_fnmsub_ps",
			"-ccgo-hide", "_mm512_mask_4dpwssd_epi32",
			"-ccgo-hide", "_mm512_mask_4dpwssds_epi32",
			"-ccgo-hide", "_mm512_mask_4fmadd_ps",
			"-ccgo-hide", "_mm512_mask_4fnmadd_ps",
			"-ccgo-hide", "_mm512_mask_abs_epi16",
			"-ccgo-hide", "_mm512_mask_abs_epi32",
			"-ccgo-hide", "_mm512_mask_abs_epi64",
			"-ccgo-hide", "_mm512_mask_abs_epi8",
			"-ccgo-hide", "_mm512_mask_abs_pd",
			"-ccgo-hide", "_mm512_mask_abs_ps",
			"-ccgo-hide", "_mm512_mask_add_epi16",
			"-ccgo-hide", "_mm512_mask_add_epi32",
			"-ccgo-hide", "_mm512_mask_add_epi64",
			"-ccgo-hide", "_mm512_mask_add_epi8",
			"-ccgo-hide", "_mm512_mask_add_pd",
			"-ccgo-hide", "_mm512_mask_add_ps",
			"-ccgo-hide", "_mm512_mask_adds_epi16",
			"-ccgo-hide", "_mm512_mask_adds_epi8",
			"-ccgo-hide", "_mm512_mask_adds_epu16",
			"-ccgo-hide", "_mm512_mask_adds_epu8",
			"-ccgo-hide", "_mm512_mask_and_epi32",
			"-ccgo-hide", "_mm512_mask_and_epi64",
			"-ccgo-hide", "_mm512_mask_and_pd",
			"-ccgo-hide", "_mm512_mask_and_ps",
			"-ccgo-hide", "_mm512_mask_andnot_epi32",
			"-ccgo-hide", "_mm512_mask_andnot_epi64",
			"-ccgo-hide", "_mm512_mask_andnot_pd",
			"-ccgo-hide", "_mm512_mask_andnot_ps",
			"-ccgo-hide", "_mm512_mask_avg_epu16",
			"-ccgo-hide", "_mm512_mask_avg_epu8",
			"-ccgo-hide", "_mm512_mask_bitshuffle_epi64_mask",
			"-ccgo-hide", "_mm512_mask_blend_epi32",
			"-ccgo-hide", "_mm512_mask_blend_epi64",
			"-ccgo-hide", "_mm512_mask_blend_pd",
			"-ccgo-hide", "_mm512_mask_blend_ps",
			"-ccgo-hide", "_mm512_mask_broadcast_f32x2",
			"-ccgo-hide", "_mm512_mask_broadcast_f32x4",
			"-ccgo-hide", "_mm512_mask_broadcast_f32x8",
			"-ccgo-hide", "_mm512_mask_broadcast_f64x2",
			"-ccgo-hide", "_mm512_mask_broadcast_f64x4",
			"-ccgo-hide", "_mm512_mask_broadcast_i32x2",
			"-ccgo-hide", "_mm512_mask_broadcast_i32x4",
			"-ccgo-hide", "_mm512_mask_broadcast_i32x8",
			"-ccgo-hide", "_mm512_mask_broadcast_i64x2",
			"-ccgo-hide", "_mm512_mask_broadcast_i64x4",
			"-ccgo-hide", "_mm512_mask_broadcastb_epi8",
			"-ccgo-hide", "_mm512_mask_broadcastd_epi32",
			"-ccgo-hide", "_mm512_mask_broadcastq_epi64",
			"-ccgo-hide", "_mm512_mask_broadcastsd_pd",
			"-ccgo-hide", "_mm512_mask_broadcastss_ps",
			"-ccgo-hide", "_mm512_mask_broadcastw_epi16",
			"-ccgo-hide", "_mm512_mask_ceil_pd",
			"-ccgo-hide", "_mm512_mask_ceil_ps",
			"-ccgo-hide", "_mm512_mask_cmpeq_epi16_mask",
			"-ccgo-hide", "_mm512_mask_cmpeq_epi32_mask",
			"-ccgo-hide", "_mm512_mask_cmpeq_epi64_mask",
			"-ccgo-hide", "_mm512_mask_cmpeq_epi8_mask",
			"-ccgo-hide", "_mm512_mask_cmpeq_epu16_mask",
			"-ccgo-hide", "_mm512_mask_cmpeq_epu32_mask",
			"-ccgo-hide", "_mm512_mask_cmpeq_epu64_mask",
			"-ccgo-hide", "_mm512_mask_cmpeq_epu8_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epi16_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epi32_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epi64_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epi8_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epu16_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epu32_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epu64_mask",
			"-ccgo-hide", "_mm512_mask_cmpge_epu8_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epi16_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epi32_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epi64_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epi8_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epu16_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epu32_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epu64_mask",
			"-ccgo-hide", "_mm512_mask_cmpgt_epu8_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epi16_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epi32_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epi64_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epi8_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epu16_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epu32_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epu64_mask",
			"-ccgo-hide", "_mm512_mask_cmple_epu8_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epi16_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epi32_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epi64_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epi8_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epu16_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epu32_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epu64_mask",
			"-ccgo-hide", "_mm512_mask_cmplt_epu8_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epi16_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epi32_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epi64_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epi8_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epu16_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epu32_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epu64_mask",
			"-ccgo-hide", "_mm512_mask_cmpneq_epu8_mask",
			"-ccgo-hide", "_mm512_mask_compress_epi16",
			"-ccgo-hide", "_mm512_mask_compress_epi32",
			"-ccgo-hide", "_mm512_mask_compress_epi64",
			"-ccgo-hide", "_mm512_mask_compress_epi8",
			"-ccgo-hide", "_mm512_mask_compress_pd",
			"-ccgo-hide", "_mm512_mask_compress_ps",
			"-ccgo-hide", "_mm512_mask_compressstoreu_epi16",
			"-ccgo-hide", "_mm512_mask_compressstoreu_epi32",
			"-ccgo-hide", "_mm512_mask_compressstoreu_epi64",
			"-ccgo-hide", "_mm512_mask_compressstoreu_epi8",
			"-ccgo-hide", "_mm512_mask_compressstoreu_pd",
			"-ccgo-hide", "_mm512_mask_compressstoreu_ps",
			"-ccgo-hide", "_mm512_mask_conflict_epi32",
			"-ccgo-hide", "_mm512_mask_conflict_epi64",
			"-ccgo-hide", "_mm512_mask_cvtepi16_epi32",
			"-ccgo-hide", "_mm512_mask_cvtepi16_epi64",
			"-ccgo-hide", "_mm512_mask_cvtepi16_epi8",
			"-ccgo-hide", "_mm512_mask_cvtepi16_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvtepi32_epi16",
			"-ccgo-hide", "_mm512_mask_cvtepi32_epi64",
			"-ccgo-hide", "_mm512_mask_cvtepi32_epi8",
			"-ccgo-hide", "_mm512_mask_cvtepi32_pd",
			"-ccgo-hide", "_mm512_mask_cvtepi32_ps",
			"-ccgo-hide", "_mm512_mask_cvtepi32_storeu_epi16",
			"-ccgo-hide", "_mm512_mask_cvtepi32_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvtepi64_epi16",
			"-ccgo-hide", "_mm512_mask_cvtepi64_epi32",
			"-ccgo-hide", "_mm512_mask_cvtepi64_epi8",
			"-ccgo-hide", "_mm512_mask_cvtepi64_pd",
			"-ccgo-hide", "_mm512_mask_cvtepi64_ps",
			"-ccgo-hide", "_mm512_mask_cvtepi64_storeu_epi16",
			"-ccgo-hide", "_mm512_mask_cvtepi64_storeu_epi32",
			"-ccgo-hide", "_mm512_mask_cvtepi64_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvtepi8_epi16",
			"-ccgo-hide", "_mm512_mask_cvtepi8_epi32",
			"-ccgo-hide", "_mm512_mask_cvtepi8_epi64",
			"-ccgo-hide", "_mm512_mask_cvtepu16_epi32",
			"-ccgo-hide", "_mm512_mask_cvtepu16_epi64",
			"-ccgo-hide", "_mm512_mask_cvtepu32_epi64",
			"-ccgo-hide", "_mm512_mask_cvtepu32_pd",
			"-ccgo-hide", "_mm512_mask_cvtepu32_ps",
			"-ccgo-hide", "_mm512_mask_cvtepu64_pd",
			"-ccgo-hide", "_mm512_mask_cvtepu64_ps",
			"-ccgo-hide", "_mm512_mask_cvtepu8_epi16",
			"-ccgo-hide", "_mm512_mask_cvtepu8_epi32",
			"-ccgo-hide", "_mm512_mask_cvtepu8_epi64",
			"-ccgo-hide", "_mm512_mask_cvtpd_epi32",
			"-ccgo-hide", "_mm512_mask_cvtpd_epi64",
			"-ccgo-hide", "_mm512_mask_cvtpd_epu32",
			"-ccgo-hide", "_mm512_mask_cvtpd_epu64",
			"-ccgo-hide", "_mm512_mask_cvtpd_ps",
			"-ccgo-hide", "_mm512_mask_cvtph_ps",
			"-ccgo-hide", "_mm512_mask_cvtps_epi32",
			"-ccgo-hide", "_mm512_mask_cvtps_epi64",
			"-ccgo-hide", "_mm512_mask_cvtps_epu32",
			"-ccgo-hide", "_mm512_mask_cvtps_epu64",
			"-ccgo-hide", "_mm512_mask_cvtps_pd",
			"-ccgo-hide", "_mm512_mask_cvtsepi16_epi8",
			"-ccgo-hide", "_mm512_mask_cvtsepi16_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvtsepi32_epi16",
			"-ccgo-hide", "_mm512_mask_cvtsepi32_epi8",
			"-ccgo-hide", "_mm512_mask_cvtsepi32_storeu_epi16",
			"-ccgo-hide", "_mm512_mask_cvtsepi32_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvtsepi64_epi16",
			"-ccgo-hide", "_mm512_mask_cvtsepi64_epi32",
			"-ccgo-hide", "_mm512_mask_cvtsepi64_epi8",
			"-ccgo-hide", "_mm512_mask_cvtsepi64_storeu_epi16",
			"-ccgo-hide", "_mm512_mask_cvtsepi64_storeu_epi32",
			"-ccgo-hide", "_mm512_mask_cvtsepi64_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvttpd_epi32",
			"-ccgo-hide", "_mm512_mask_cvttpd_epi64",
			"-ccgo-hide", "_mm512_mask_cvttpd_epu32",
			"-ccgo-hide", "_mm512_mask_cvttpd_epu64",
			"-ccgo-hide", "_mm512_mask_cvttps_epi32",
			"-ccgo-hide", "_mm512_mask_cvttps_epi64",
			"-ccgo-hide", "_mm512_mask_cvttps_epu32",
			"-ccgo-hide", "_mm512_mask_cvttps_epu64",
			"-ccgo-hide", "_mm512_mask_cvtusepi16_epi8",
			"-ccgo-hide", "_mm512_mask_cvtusepi16_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvtusepi32_epi16",
			"-ccgo-hide", "_mm512_mask_cvtusepi32_epi8",
			"-ccgo-hide", "_mm512_mask_cvtusepi32_storeu_epi16",
			"-ccgo-hide", "_mm512_mask_cvtusepi32_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_cvtusepi64_epi16",
			"-ccgo-hide", "_mm512_mask_cvtusepi64_epi32",
			"-ccgo-hide", "_mm512_mask_cvtusepi64_epi8",
			"-ccgo-hide", "_mm512_mask_cvtusepi64_storeu_epi16",
			"-ccgo-hide", "_mm512_mask_cvtusepi64_storeu_epi32",
			"-ccgo-hide", "_mm512_mask_cvtusepi64_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_div_pd",
			"-ccgo-hide", "_mm512_mask_div_ps",
			"-ccgo-hide", "_mm512_mask_dpbusd_epi32",
			"-ccgo-hide", "_mm512_mask_dpbusds_epi32",
			"-ccgo-hide", "_mm512_mask_dpwssd_epi32",
			"-ccgo-hide", "_mm512_mask_dpwssds_epi32",
			"-ccgo-hide", "_mm512_mask_expand_epi16",
			"-ccgo-hide", "_mm512_mask_expand_epi32",
			"-ccgo-hide", "_mm512_mask_expand_epi64",
			"-ccgo-hide", "_mm512_mask_expand_epi8",
			"-ccgo-hide", "_mm512_mask_expand_pd",
			"-ccgo-hide", "_mm512_mask_expand_ps",
			"-ccgo-hide", "_mm512_mask_expandloadu_epi16",
			"-ccgo-hide", "_mm512_mask_expandloadu_epi32",
			"-ccgo-hide", "_mm512_mask_expandloadu_epi64",
			"-ccgo-hide", "_mm512_mask_expandloadu_epi8",
			"-ccgo-hide", "_mm512_mask_expandloadu_pd",
			"-ccgo-hide", "_mm512_mask_expandloadu_ps",
			"-ccgo-hide", "_mm512_mask_floor_pd",
			"-ccgo-hide", "_mm512_mask_floor_ps",
			"-ccgo-hide", "_mm512_mask_fmadd_pd",
			"-ccgo-hide", "_mm512_mask_fmadd_ps",
			"-ccgo-hide", "_mm512_mask_fmaddsub_pd",
			"-ccgo-hide", "_mm512_mask_fmaddsub_ps",
			"-ccgo-hide", "_mm512_mask_fmsub_pd",
			"-ccgo-hide", "_mm512_mask_fmsub_ps",
			"-ccgo-hide", "_mm512_mask_fmsubadd_pd",
			"-ccgo-hide", "_mm512_mask_fmsubadd_ps",
			"-ccgo-hide", "_mm512_mask_fnmadd_pd",
			"-ccgo-hide", "_mm512_mask_fnmadd_ps",
			"-ccgo-hide", "_mm512_mask_fnmsub_pd",
			"-ccgo-hide", "_mm512_mask_fnmsub_ps",
			"-ccgo-hide", "_mm512_mask_gf2p8mul_epi8",
			"-ccgo-hide", "_mm512_mask_load_epi32",
			"-ccgo-hide", "_mm512_mask_load_epi64",
			"-ccgo-hide", "_mm512_mask_load_pd",
			"-ccgo-hide", "_mm512_mask_load_ps",
			"-ccgo-hide", "_mm512_mask_loadu_epi16",
			"-ccgo-hide", "_mm512_mask_loadu_epi32",
			"-ccgo-hide", "_mm512_mask_loadu_epi64",
			"-ccgo-hide", "_mm512_mask_loadu_epi8",
			"-ccgo-hide", "_mm512_mask_loadu_pd",
			"-ccgo-hide", "_mm512_mask_loadu_ps",
			"-ccgo-hide", "_mm512_mask_lzcnt_epi32",
			"-ccgo-hide", "_mm512_mask_lzcnt_epi64",
			"-ccgo-hide", "_mm512_mask_madd52hi_epu64",
			"-ccgo-hide", "_mm512_mask_madd52lo_epu64",
			"-ccgo-hide", "_mm512_mask_madd_epi16",
			"-ccgo-hide", "_mm512_mask_maddubs_epi16",
			"-ccgo-hide", "_mm512_mask_max_epi16",
			"-ccgo-hide", "_mm512_mask_max_epi32",
			"-ccgo-hide", "_mm512_mask_max_epi64",
			"-ccgo-hide", "_mm512_mask_max_epi8",
			"-ccgo-hide", "_mm512_mask_max_epu16",
			"-ccgo-hide", "_mm512_mask_max_epu32",
			"-ccgo-hide", "_mm512_mask_max_epu64",
			"-ccgo-hide", "_mm512_mask_max_epu8",
			"-ccgo-hide", "_mm512_mask_max_pd",
			"-ccgo-hide", "_mm512_mask_max_ps",
			"-ccgo-hide", "_mm512_mask_min_epi16",
			"-ccgo-hide", "_mm512_mask_min_epi32",
			"-ccgo-hide", "_mm512_mask_min_epi64",
			"-ccgo-hide", "_mm512_mask_min_epi8",
			"-ccgo-hide", "_mm512_mask_min_epu16",
			"-ccgo-hide", "_mm512_mask_min_epu32",
			"-ccgo-hide", "_mm512_mask_min_epu64",
			"-ccgo-hide", "_mm512_mask_min_epu8",
			"-ccgo-hide", "_mm512_mask_min_pd",
			"-ccgo-hide", "_mm512_mask_min_ps",
			"-ccgo-hide", "_mm512_mask_mov_epi16",
			"-ccgo-hide", "_mm512_mask_mov_epi32",
			"-ccgo-hide", "_mm512_mask_mov_epi64",
			"-ccgo-hide", "_mm512_mask_mov_epi8",
			"-ccgo-hide", "_mm512_mask_mov_pd",
			"-ccgo-hide", "_mm512_mask_mov_ps",
			"-ccgo-hide", "_mm512_mask_movedup_pd",
			"-ccgo-hide", "_mm512_mask_movehdup_ps",
			"-ccgo-hide", "_mm512_mask_moveldup_ps",
			"-ccgo-hide", "_mm512_mask_mul_epi32",
			"-ccgo-hide", "_mm512_mask_mul_epu32",
			"-ccgo-hide", "_mm512_mask_mul_pd",
			"-ccgo-hide", "_mm512_mask_mul_ps",
			"-ccgo-hide", "_mm512_mask_mulhi_epi16",
			"-ccgo-hide", "_mm512_mask_mulhi_epu16",
			"-ccgo-hide", "_mm512_mask_mulhrs_epi16",
			"-ccgo-hide", "_mm512_mask_mullo_epi16",
			"-ccgo-hide", "_mm512_mask_mullo_epi32",
			"-ccgo-hide", "_mm512_mask_mullo_epi64",
			"-ccgo-hide", "_mm512_mask_multishift_epi64_epi8",
			"-ccgo-hide", "_mm512_mask_or_epi32",
			"-ccgo-hide", "_mm512_mask_or_epi64",
			"-ccgo-hide", "_mm512_mask_or_pd",
			"-ccgo-hide", "_mm512_mask_or_ps",
			"-ccgo-hide", "_mm512_mask_packs_epi16",
			"-ccgo-hide", "_mm512_mask_packs_epi32",
			"-ccgo-hide", "_mm512_mask_packus_epi16",
			"-ccgo-hide", "_mm512_mask_packus_epi32",
			"-ccgo-hide", "_mm512_mask_permutevar_pd",
			"-ccgo-hide", "_mm512_mask_permutevar_ps",
			"-ccgo-hide", "_mm512_mask_permutex2var_epi16",
			"-ccgo-hide", "_mm512_mask_permutex2var_epi32",
			"-ccgo-hide", "_mm512_mask_permutex2var_epi64",
			"-ccgo-hide", "_mm512_mask_permutex2var_epi8",
			"-ccgo-hide", "_mm512_mask_permutex2var_pd",
			"-ccgo-hide", "_mm512_mask_permutex2var_ps",
			"-ccgo-hide", "_mm512_mask_permutexvar_epi16",
			"-ccgo-hide", "_mm512_mask_permutexvar_epi32",
			"-ccgo-hide", "_mm512_mask_permutexvar_epi64",
			"-ccgo-hide", "_mm512_mask_permutexvar_epi8",
			"-ccgo-hide", "_mm512_mask_permutexvar_pd",
			"-ccgo-hide", "_mm512_mask_permutexvar_ps",
			"-ccgo-hide", "_mm512_mask_popcnt_epi16",
			"-ccgo-hide", "_mm512_mask_popcnt_epi32",
			"-ccgo-hide", "_mm512_mask_popcnt_epi64",
			"-ccgo-hide", "_mm512_mask_popcnt_epi8",
			"-ccgo-hide", "_mm512_mask_rcp14_pd",
			"-ccgo-hide", "_mm512_mask_rcp14_ps",
			"-ccgo-hide", "_mm512_mask_reduce_add_epi32",
			"-ccgo-hide", "_mm512_mask_reduce_add_epi64",
			"-ccgo-hide", "_mm512_mask_reduce_add_pd",
			"-ccgo-hide", "_mm512_mask_reduce_add_ps",
			"-ccgo-hide", "_mm512_mask_reduce_and_epi32",
			"-ccgo-hide", "_mm512_mask_reduce_and_epi64",
			"-ccgo-hide", "_mm512_mask_reduce_max_epi32",
			"-ccgo-hide", "_mm512_mask_reduce_max_epi64",
			"-ccgo-hide", "_mm512_mask_reduce_max_epu32",
			"-ccgo-hide", "_mm512_mask_reduce_max_epu64",
			"-ccgo-hide", "_mm512_mask_reduce_max_pd",
			"-ccgo-hide", "_mm512_mask_reduce_max_ps",
			"-ccgo-hide", "_mm512_mask_reduce_min_epi32",
			"-ccgo-hide", "_mm512_mask_reduce_min_epi64",
			"-ccgo-hide", "_mm512_mask_reduce_min_epu32",
			"-ccgo-hide", "_mm512_mask_reduce_min_epu64",
			"-ccgo-hide", "_mm512_mask_reduce_min_pd",
			"-ccgo-hide", "_mm512_mask_reduce_min_ps",
			"-ccgo-hide", "_mm512_mask_reduce_mul_epi32",
			"-ccgo-hide", "_mm512_mask_reduce_mul_epi64",
			"-ccgo-hide", "_mm512_mask_reduce_mul_pd",
			"-ccgo-hide", "_mm512_mask_reduce_mul_ps",
			"-ccgo-hide", "_mm512_mask_reduce_or_epi32",
			"-ccgo-hide", "_mm512_mask_reduce_or_epi64",
			"-ccgo-hide", "_mm512_mask_rolv_epi32",
			"-ccgo-hide", "_mm512_mask_rolv_epi64",
			"-ccgo-hide", "_mm512_mask_rorv_epi32",
			"-ccgo-hide", "_mm512_mask_rorv_epi64",
			"-ccgo-hide", "_mm512_mask_rsqrt14_pd",
			"-ccgo-hide", "_mm512_mask_rsqrt14_ps",
			"-ccgo-hide", "_mm512_mask_scalef_pd",
			"-ccgo-hide", "_mm512_mask_scalef_ps",
			"-ccgo-hide", "_mm512_mask_set1_epi16",
			"-ccgo-hide", "_mm512_mask_set1_epi32",
			"-ccgo-hide", "_mm512_mask_set1_epi64",
			"-ccgo-hide", "_mm512_mask_set1_epi8",
			"-ccgo-hide", "_mm512_mask_shldv_epi16",
			"-ccgo-hide", "_mm512_mask_shldv_epi32",
			"-ccgo-hide", "_mm512_mask_shldv_epi64",
			"-ccgo-hide", "_mm512_mask_shrdv_epi16",
			"-ccgo-hide", "_mm512_mask_shrdv_epi32",
			"-ccgo-hide", "_mm512_mask_shrdv_epi64",
			"-ccgo-hide", "_mm512_mask_shuffle_epi8",
			"-ccgo-hide", "_mm512_mask_sll_epi16",
			"-ccgo-hide", "_mm512_mask_sll_epi32",
			"-ccgo-hide", "_mm512_mask_sll_epi64",
			"-ccgo-hide", "_mm512_mask_sllv_epi16",
			"-ccgo-hide", "_mm512_mask_sllv_epi32",
			"-ccgo-hide", "_mm512_mask_sllv_epi64",
			"-ccgo-hide", "_mm512_mask_sqrt_pd",
			"-ccgo-hide", "_mm512_mask_sqrt_ps",
			"-ccgo-hide", "_mm512_mask_sra_epi16",
			"-ccgo-hide", "_mm512_mask_sra_epi32",
			"-ccgo-hide", "_mm512_mask_sra_epi64",
			"-ccgo-hide", "_mm512_mask_srav_epi16",
			"-ccgo-hide", "_mm512_mask_srav_epi32",
			"-ccgo-hide", "_mm512_mask_srav_epi64",
			"-ccgo-hide", "_mm512_mask_srl_epi16",
			"-ccgo-hide", "_mm512_mask_srl_epi32",
			"-ccgo-hide", "_mm512_mask_srl_epi64",
			"-ccgo-hide", "_mm512_mask_srlv_epi16",
			"-ccgo-hide", "_mm512_mask_srlv_epi32",
			"-ccgo-hide", "_mm512_mask_srlv_epi64",
			"-ccgo-hide", "_mm512_mask_store_epi32",
			"-ccgo-hide", "_mm512_mask_store_epi64",
			"-ccgo-hide", "_mm512_mask_store_pd",
			"-ccgo-hide", "_mm512_mask_store_ps",
			"-ccgo-hide", "_mm512_mask_storeu_epi16",
			"-ccgo-hide", "_mm512_mask_storeu_epi32",
			"-ccgo-hide", "_mm512_mask_storeu_epi64",
			"-ccgo-hide", "_mm512_mask_storeu_epi8",
			"-ccgo-hide", "_mm512_mask_storeu_pd",
			"-ccgo-hide", "_mm512_mask_storeu_ps",
			"-ccgo-hide", "_mm512_mask_sub_epi16",
			"-ccgo-hide", "_mm512_mask_sub_epi32",
			"-ccgo-hide", "_mm512_mask_sub_epi64",
			"-ccgo-hide", "_mm512_mask_sub_epi8",
			"-ccgo-hide", "_mm512_mask_sub_pd",
			"-ccgo-hide", "_mm512_mask_sub_ps",
			"-ccgo-hide", "_mm512_mask_subs_epi16",
			"-ccgo-hide", "_mm512_mask_subs_epi8",
			"-ccgo-hide", "_mm512_mask_subs_epu16",
			"-ccgo-hide", "_mm512_mask_subs_epu8",
			"-ccgo-hide", "_mm512_mask_test_epi16_mask",
			"-ccgo-hide", "_mm512_mask_test_epi32_mask",
			"-ccgo-hide", "_mm512_mask_test_epi64_mask",
			"-ccgo-hide", "_mm512_mask_test_epi8_mask",
			"-ccgo-hide", "_mm512_mask_testn_epi16_mask",
			"-ccgo-hide", "_mm512_mask_testn_epi32_mask",
			"-ccgo-hide", "_mm512_mask_testn_epi64_mask",
			"-ccgo-hide", "_mm512_mask_testn_epi8_mask",
			"-ccgo-hide", "_mm512_mask_unpackhi_epi16",
			"-ccgo-hide", "_mm512_mask_unpackhi_epi32",
			"-ccgo-hide", "_mm512_mask_unpackhi_epi64",
			"-ccgo-hide", "_mm512_mask_unpackhi_epi8",
			"-ccgo-hide", "_mm512_mask_unpackhi_pd",
			"-ccgo-hide", "_mm512_mask_unpackhi_ps",
			"-ccgo-hide", "_mm512_mask_unpacklo_epi16",
			"-ccgo-hide", "_mm512_mask_unpacklo_epi32",
			"-ccgo-hide", "_mm512_mask_unpacklo_epi64",
			"-ccgo-hide", "_mm512_mask_unpacklo_epi8",
			"-ccgo-hide", "_mm512_mask_unpacklo_pd",
			"-ccgo-hide", "_mm512_mask_unpacklo_ps",
			"-ccgo-hide", "_mm512_mask_xor_epi32",
			"-ccgo-hide", "_mm512_mask_xor_epi64",
			"-ccgo-hide", "_mm512_mask_xor_pd",
			"-ccgo-hide", "_mm512_mask_xor_ps",
			"-ccgo-hide", "_mm512_maskz_4dpwssd_epi32",
			"-ccgo-hide", "_mm512_maskz_4dpwssds_epi32",
			"-ccgo-hide", "_mm512_maskz_4fmadd_ps",
			"-ccgo-hide", "_mm512_maskz_4fnmadd_ps",
			"-ccgo-hide", "_mm512_maskz_abs_epi16",
			"-ccgo-hide", "_mm512_maskz_abs_epi32",
			"-ccgo-hide", "_mm512_maskz_abs_epi64",
			"-ccgo-hide", "_mm512_maskz_abs_epi8",
			"-ccgo-hide", "_mm512_maskz_add_epi16",
			"-ccgo-hide", "_mm512_maskz_add_epi32",
			"-ccgo-hide", "_mm512_maskz_add_epi64",
			"-ccgo-hide", "_mm512_maskz_add_epi8",
			"-ccgo-hide", "_mm512_maskz_add_pd",
			"-ccgo-hide", "_mm512_maskz_add_ps",
			"-ccgo-hide", "_mm512_maskz_adds_epi16",
			"-ccgo-hide", "_mm512_maskz_adds_epi8",
			"-ccgo-hide", "_mm512_maskz_adds_epu16",
			"-ccgo-hide", "_mm512_maskz_adds_epu8",
			"-ccgo-hide", "_mm512_maskz_and_epi32",
			"-ccgo-hide", "_mm512_maskz_and_epi64",
			"-ccgo-hide", "_mm512_maskz_and_pd",
			"-ccgo-hide", "_mm512_maskz_and_ps",
			"-ccgo-hide", "_mm512_maskz_andnot_epi32",
			"-ccgo-hide", "_mm512_maskz_andnot_epi64",
			"-ccgo-hide", "_mm512_maskz_andnot_pd",
			"-ccgo-hide", "_mm512_maskz_andnot_ps",
			"-ccgo-hide", "_mm512_maskz_avg_epu16",
			"-ccgo-hide", "_mm512_maskz_avg_epu8",
			"-ccgo-hide", "_mm512_maskz_broadcast_f32x2",
			"-ccgo-hide", "_mm512_maskz_broadcast_f32x4",
			"-ccgo-hide", "_mm512_maskz_broadcast_f32x8",
			"-ccgo-hide", "_mm512_maskz_broadcast_f64x2",
			"-ccgo-hide", "_mm512_maskz_broadcast_f64x4",
			"-ccgo-hide", "_mm512_maskz_broadcast_i32x2",
			"-ccgo-hide", "_mm512_maskz_broadcast_i32x4",
			"-ccgo-hide", "_mm512_maskz_broadcast_i32x8",
			"-ccgo-hide", "_mm512_maskz_broadcast_i64x2",
			"-ccgo-hide", "_mm512_maskz_broadcast_i64x4",
			"-ccgo-hide", "_mm512_maskz_broadcastb_epi8",
			"-ccgo-hide", "_mm512_maskz_broadcastd_epi32",
			"-ccgo-hide", "_mm512_maskz_broadcastq_epi64",
			"-ccgo-hide", "_mm512_maskz_broadcastsd_pd",
			"-ccgo-hide", "_mm512_maskz_broadcastss_ps",
			"-ccgo-hide", "_mm512_maskz_broadcastw_epi16",
			"-ccgo-hide", "_mm512_maskz_compress_epi16",
			"-ccgo-hide", "_mm512_maskz_compress_epi32",
			"-ccgo-hide", "_mm512_maskz_compress_epi64",
			"-ccgo-hide", "_mm512_maskz_compress_epi8",
			"-ccgo-hide", "_mm512_maskz_compress_pd",
			"-ccgo-hide", "_mm512_maskz_compress_ps",
			"-ccgo-hide", "_mm512_maskz_conflict_epi32",
			"-ccgo-hide", "_mm512_maskz_conflict_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtepi16_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtepi16_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtepi16_epi8",
			"-ccgo-hide", "_mm512_maskz_cvtepi32_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtepi32_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtepi32_epi8",
			"-ccgo-hide", "_mm512_maskz_cvtepi32_pd",
			"-ccgo-hide", "_mm512_maskz_cvtepi32_ps",
			"-ccgo-hide", "_mm512_maskz_cvtepi64_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtepi64_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtepi64_epi8",
			"-ccgo-hide", "_mm512_maskz_cvtepi64_pd",
			"-ccgo-hide", "_mm512_maskz_cvtepi64_ps",
			"-ccgo-hide", "_mm512_maskz_cvtepi8_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtepi8_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtepi8_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtepu16_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtepu16_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtepu32_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtepu32_pd",
			"-ccgo-hide", "_mm512_maskz_cvtepu32_ps",
			"-ccgo-hide", "_mm512_maskz_cvtepu64_pd",
			"-ccgo-hide", "_mm512_maskz_cvtepu64_ps",
			"-ccgo-hide", "_mm512_maskz_cvtepu8_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtepu8_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtepu8_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtpd_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtpd_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtpd_epu32",
			"-ccgo-hide", "_mm512_maskz_cvtpd_epu64",
			"-ccgo-hide", "_mm512_maskz_cvtpd_ps",
			"-ccgo-hide", "_mm512_maskz_cvtph_ps",
			"-ccgo-hide", "_mm512_maskz_cvtps_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtps_epi64",
			"-ccgo-hide", "_mm512_maskz_cvtps_epu32",
			"-ccgo-hide", "_mm512_maskz_cvtps_epu64",
			"-ccgo-hide", "_mm512_maskz_cvtps_pd",
			"-ccgo-hide", "_mm512_maskz_cvtsepi16_epi8",
			"-ccgo-hide", "_mm512_maskz_cvtsepi32_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtsepi32_epi8",
			"-ccgo-hide", "_mm512_maskz_cvtsepi64_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtsepi64_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtsepi64_epi8",
			"-ccgo-hide", "_mm512_maskz_cvttpd_epi32",
			"-ccgo-hide", "_mm512_maskz_cvttpd_epi64",
			"-ccgo-hide", "_mm512_maskz_cvttpd_epu32",
			"-ccgo-hide", "_mm512_maskz_cvttpd_epu64",
			"-ccgo-hide", "_mm512_maskz_cvttps_epi32",
			"-ccgo-hide", "_mm512_maskz_cvttps_epi64",
			"-ccgo-hide", "_mm512_maskz_cvttps_epu32",
			"-ccgo-hide", "_mm512_maskz_cvttps_epu64",
			"-ccgo-hide", "_mm512_maskz_cvtusepi16_epi8",
			"-ccgo-hide", "_mm512_maskz_cvtusepi32_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtusepi32_epi8",
			"-ccgo-hide", "_mm512_maskz_cvtusepi64_epi16",
			"-ccgo-hide", "_mm512_maskz_cvtusepi64_epi32",
			"-ccgo-hide", "_mm512_maskz_cvtusepi64_epi8",
			"-ccgo-hide", "_mm512_maskz_div_pd",
			"-ccgo-hide", "_mm512_maskz_div_ps",
			"-ccgo-hide", "_mm512_maskz_dpbusd_epi32",
			"-ccgo-hide", "_mm512_maskz_dpbusds_epi32",
			"-ccgo-hide", "_mm512_maskz_dpwssd_epi32",
			"-ccgo-hide", "_mm512_maskz_dpwssds_epi32",
			"-ccgo-hide", "_mm512_maskz_expand_epi16",
			"-ccgo-hide", "_mm512_maskz_expand_epi32",
			"-ccgo-hide", "_mm512_maskz_expand_epi64",
			"-ccgo-hide", "_mm512_maskz_expand_epi8",
			"-ccgo-hide", "_mm512_maskz_expand_pd",
			"-ccgo-hide", "_mm512_maskz_expand_ps",
			"-ccgo-hide", "_mm512_maskz_expandloadu_epi16",
			"-ccgo-hide", "_mm512_maskz_expandloadu_epi32",
			"-ccgo-hide", "_mm512_maskz_expandloadu_epi64",
			"-ccgo-hide", "_mm512_maskz_expandloadu_epi8",
			"-ccgo-hide", "_mm512_maskz_expandloadu_pd",
			"-ccgo-hide", "_mm512_maskz_expandloadu_ps",
			"-ccgo-hide", "_mm512_maskz_fmadd_pd",
			"-ccgo-hide", "_mm512_maskz_fmadd_ps",
			"-ccgo-hide", "_mm512_maskz_fmaddsub_pd",
			"-ccgo-hide", "_mm512_maskz_fmaddsub_ps",
			"-ccgo-hide", "_mm512_maskz_fmsub_pd",
			"-ccgo-hide", "_mm512_maskz_fmsub_ps",
			"-ccgo-hide", "_mm512_maskz_fmsubadd_pd",
			"-ccgo-hide", "_mm512_maskz_fmsubadd_ps",
			"-ccgo-hide", "_mm512_maskz_fnmadd_pd",
			"-ccgo-hide", "_mm512_maskz_fnmadd_ps",
			"-ccgo-hide", "_mm512_maskz_fnmsub_pd",
			"-ccgo-hide", "_mm512_maskz_fnmsub_ps",
			"-ccgo-hide", "_mm512_maskz_gf2p8mul_epi8",
			"-ccgo-hide", "_mm512_maskz_load_epi32",
			"-ccgo-hide", "_mm512_maskz_load_epi64",
			"-ccgo-hide", "_mm512_maskz_load_pd",
			"-ccgo-hide", "_mm512_maskz_load_ps",
			"-ccgo-hide", "_mm512_maskz_loadu_epi16",
			"-ccgo-hide", "_mm512_maskz_loadu_epi32",
			"-ccgo-hide", "_mm512_maskz_loadu_epi64",
			"-ccgo-hide", "_mm512_maskz_loadu_epi8",
			"-ccgo-hide", "_mm512_maskz_loadu_pd",
			"-ccgo-hide", "_mm512_maskz_loadu_ps",
			"-ccgo-hide", "_mm512_maskz_lzcnt_epi32",
			"-ccgo-hide", "_mm512_maskz_lzcnt_epi64",
			"-ccgo-hide", "_mm512_maskz_madd52hi_epu64",
			"-ccgo-hide", "_mm512_maskz_madd52lo_epu64",
			"-ccgo-hide", "_mm512_maskz_madd_epi16",
			"-ccgo-hide", "_mm512_maskz_maddubs_epi16",
			"-ccgo-hide", "_mm512_maskz_max_epi16",
			"-ccgo-hide", "_mm512_maskz_max_epi32",
			"-ccgo-hide", "_mm512_maskz_max_epi64",
			"-ccgo-hide", "_mm512_maskz_max_epi8",
			"-ccgo-hide", "_mm512_maskz_max_epu16",
			"-ccgo-hide", "_mm512_maskz_max_epu32",
			"-ccgo-hide", "_mm512_maskz_max_epu64",
			"-ccgo-hide", "_mm512_maskz_max_epu8",
			"-ccgo-hide", "_mm512_maskz_max_pd",
			"-ccgo-hide", "_mm512_maskz_max_ps",
			"-ccgo-hide", "_mm512_maskz_min_epi16",
			"-ccgo-hide", "_mm512_maskz_min_epi32",
			"-ccgo-hide", "_mm512_maskz_min_epi64",
			"-ccgo-hide", "_mm512_maskz_min_epi8",
			"-ccgo-hide", "_mm512_maskz_min_epu16",
			"-ccgo-hide", "_mm512_maskz_min_epu32",
			"-ccgo-hide", "_mm512_maskz_min_epu64",
			"-ccgo-hide", "_mm512_maskz_min_epu8",
			"-ccgo-hide", "_mm512_maskz_min_pd",
			"-ccgo-hide", "_mm512_maskz_min_ps",
			"-ccgo-hide", "_mm512_maskz_mov_epi16",
			"-ccgo-hide", "_mm512_maskz_mov_epi32",
			"-ccgo-hide", "_mm512_maskz_mov_epi64",
			"-ccgo-hide", "_mm512_maskz_mov_epi8",
			"-ccgo-hide", "_mm512_maskz_mov_pd",
			"-ccgo-hide", "_mm512_maskz_mov_ps",
			"-ccgo-hide", "_mm512_maskz_movedup_pd",
			"-ccgo-hide", "_mm512_maskz_movehdup_ps",
			"-ccgo-hide", "_mm512_maskz_moveldup_ps",
			"-ccgo-hide", "_mm512_maskz_mul_epi32",
			"-ccgo-hide", "_mm512_maskz_mul_epu32",
			"-ccgo-hide", "_mm512_maskz_mul_pd",
			"-ccgo-hide", "_mm512_maskz_mul_ps",
			"-ccgo-hide", "_mm512_maskz_mulhi_epi16",
			"-ccgo-hide", "_mm512_maskz_mulhi_epu16",
			"-ccgo-hide", "_mm512_maskz_mulhrs_epi16",
			"-ccgo-hide", "_mm512_maskz_mullo_epi16",
			"-ccgo-hide", "_mm512_maskz_mullo_epi32",
			"-ccgo-hide", "_mm512_maskz_mullo_epi64",
			"-ccgo-hide", "_mm512_maskz_multishift_epi64_epi8",
			"-ccgo-hide", "_mm512_maskz_or_epi32",
			"-ccgo-hide", "_mm512_maskz_or_epi64",
			"-ccgo-hide", "_mm512_maskz_or_pd",
			"-ccgo-hide", "_mm512_maskz_or_ps",
			"-ccgo-hide", "_mm512_maskz_packs_epi16",
			"-ccgo-hide", "_mm512_maskz_packs_epi32",
			"-ccgo-hide", "_mm512_maskz_packus_epi16",
			"-ccgo-hide", "_mm512_maskz_packus_epi32",
			"-ccgo-hide", "_mm512_maskz_permutevar_pd",
			"-ccgo-hide", "_mm512_maskz_permutevar_ps",
			"-ccgo-hide", "_mm512_maskz_permutex2var_epi16",
			"-ccgo-hide", "_mm512_maskz_permutex2var_epi32",
			"-ccgo-hide", "_mm512_maskz_permutex2var_epi64",
			"-ccgo-hide", "_mm512_maskz_permutex2var_epi8",
			"-ccgo-hide", "_mm512_maskz_permutex2var_pd",
			"-ccgo-hide", "_mm512_maskz_permutex2var_ps",
			"-ccgo-hide", "_mm512_maskz_permutexvar_epi16",
			"-ccgo-hide", "_mm512_maskz_permutexvar_epi32",
			"-ccgo-hide", "_mm512_maskz_permutexvar_epi64",
			"-ccgo-hide", "_mm512_maskz_permutexvar_epi8",
			"-ccgo-hide", "_mm512_maskz_permutexvar_pd",
			"-ccgo-hide", "_mm512_maskz_permutexvar_ps",
			"-ccgo-hide", "_mm512_maskz_popcnt_epi16",
			"-ccgo-hide", "_mm512_maskz_popcnt_epi32",
			"-ccgo-hide", "_mm512_maskz_popcnt_epi64",
			"-ccgo-hide", "_mm512_maskz_popcnt_epi8",
			"-ccgo-hide", "_mm512_maskz_rcp14_pd",
			"-ccgo-hide", "_mm512_maskz_rcp14_ps",
			"-ccgo-hide", "_mm512_maskz_rolv_epi32",
			"-ccgo-hide", "_mm512_maskz_rolv_epi64",
			"-ccgo-hide", "_mm512_maskz_rorv_epi32",
			"-ccgo-hide", "_mm512_maskz_rorv_epi64",
			"-ccgo-hide", "_mm512_maskz_rsqrt14_pd",
			"-ccgo-hide", "_mm512_maskz_rsqrt14_ps",
			"-ccgo-hide", "_mm512_maskz_scalef_pd",
			"-ccgo-hide", "_mm512_maskz_scalef_ps",
			"-ccgo-hide", "_mm512_maskz_set1_epi16",
			"-ccgo-hide", "_mm512_maskz_set1_epi32",
			"-ccgo-hide", "_mm512_maskz_set1_epi64",
			"-ccgo-hide", "_mm512_maskz_set1_epi8",
			"-ccgo-hide", "_mm512_maskz_shldv_epi16",
			"-ccgo-hide", "_mm512_maskz_shldv_epi32",
			"-ccgo-hide", "_mm512_maskz_shldv_epi64",
			"-ccgo-hide", "_mm512_maskz_shrdv_epi16",
			"-ccgo-hide", "_mm512_maskz_shrdv_epi32",
			"-ccgo-hide", "_mm512_maskz_shrdv_epi64",
			"-ccgo-hide", "_mm512_maskz_shuffle_epi8",
			"-ccgo-hide", "_mm512_maskz_sll_epi16",
			"-ccgo-hide", "_mm512_maskz_sll_epi32",
			"-ccgo-hide", "_mm512_maskz_sll_epi64",
			"-ccgo-hide", "_mm512_maskz_sllv_epi16",
			"-ccgo-hide", "_mm512_maskz_sllv_epi32",
			"-ccgo-hide", "_mm512_maskz_sllv_epi64",
			"-ccgo-hide", "_mm512_maskz_sqrt_pd",
			"-ccgo-hide", "_mm512_maskz_sqrt_ps",
			"-ccgo-hide", "_mm512_maskz_sra_epi16",
			"-ccgo-hide", "_mm512_maskz_sra_epi32",
			"-ccgo-hide", "_mm512_maskz_sra_epi64",
			"-ccgo-hide", "_mm512_maskz_srav_epi16",
			"-ccgo-hide", "_mm512_maskz_srav_epi32",
			"-ccgo-hide", "_mm512_maskz_srav_epi64",
			"-ccgo-hide", "_mm512_maskz_srl_epi16",
			"-ccgo-hide", "_mm512_maskz_srl_epi32",
			"-ccgo-hide", "_mm512_maskz_srl_epi64",
			"-ccgo-hide", "_mm512_maskz_srlv_epi16",
			"-ccgo-hide", "_mm512_maskz_srlv_epi32",
			"-ccgo-hide", "_mm512_maskz_srlv_epi64",
			"-ccgo-hide", "_mm512_maskz_sub_epi16",
			"-ccgo-hide", "_mm512_maskz_sub_epi32",
			"-ccgo-hide", "_mm512_maskz_sub_epi64",
			"-ccgo-hide", "_mm512_maskz_sub_epi8",
			"-ccgo-hide", "_mm512_maskz_sub_pd",
			"-ccgo-hide", "_mm512_maskz_sub_ps",
			"-ccgo-hide", "_mm512_maskz_subs_epi16",
			"-ccgo-hide", "_mm512_maskz_subs_epi8",
			"-ccgo-hide", "_mm512_maskz_subs_epu16",
			"-ccgo-hide", "_mm512_maskz_subs_epu8",
			"-ccgo-hide", "_mm512_maskz_unpackhi_epi16",
			"-ccgo-hide", "_mm512_maskz_unpackhi_epi32",
			"-ccgo-hide", "_mm512_maskz_unpackhi_epi64",
			"-ccgo-hide", "_mm512_maskz_unpackhi_epi8",
			"-ccgo-hide", "_mm512_maskz_unpackhi_pd",
			"-ccgo-hide", "_mm512_maskz_unpackhi_ps",
			"-ccgo-hide", "_mm512_maskz_unpacklo_epi16",
			"-ccgo-hide", "_mm512_maskz_unpacklo_epi32",
			"-ccgo-hide", "_mm512_maskz_unpacklo_epi64",
			"-ccgo-hide", "_mm512_maskz_unpacklo_epi8",
			"-ccgo-hide", "_mm512_maskz_unpacklo_pd",
			"-ccgo-hide", "_mm512_maskz_unpacklo_ps",
			"-ccgo-hide", "_mm512_maskz_xor_epi32",
			"-ccgo-hide", "_mm512_maskz_xor_epi64",
			"-ccgo-hide", "_mm512_maskz_xor_pd",
			"-ccgo-hide", "_mm512_maskz_xor_ps",
			"-ccgo-hide", "_mm512_max_epi16",
			"-ccgo-hide", "_mm512_max_epi32",
			"-ccgo-hide", "_mm512_max_epi64",
			"-ccgo-hide", "_mm512_max_epi8",
			"-ccgo-hide", "_mm512_max_epu16",
			"-ccgo-hide", "_mm512_max_epu32",
			"-ccgo-hide", "_mm512_max_epu64",
			"-ccgo-hide", "_mm512_max_epu8",
			"-ccgo-hide", "_mm512_max_pd",
			"-ccgo-hide", "_mm512_max_ps",
			"-ccgo-hide", "_mm512_min_epi16",
			"-ccgo-hide", "_mm512_min_epi32",
			"-ccgo-hide", "_mm512_min_epi64",
			"-ccgo-hide", "_mm512_min_epi8",
			"-ccgo-hide", "_mm512_min_epu16",
			"-ccgo-hide", "_mm512_min_epu32",
			"-ccgo-hide", "_mm512_min_epu64",
			"-ccgo-hide", "_mm512_min_epu8",
			"-ccgo-hide", "_mm512_min_pd",
			"-ccgo-hide", "_mm512_min_ps",
			"-ccgo-hide", "_mm512_movedup_pd",
			"-ccgo-hide", "_mm512_movehdup_ps",
			"-ccgo-hide", "_mm512_moveldup_ps",
			"-ccgo-hide", "_mm512_movepi16_mask",
			"-ccgo-hide", "_mm512_movepi32_mask",
			"-ccgo-hide", "_mm512_movepi64_mask",
			"-ccgo-hide", "_mm512_movepi8_mask",
			"-ccgo-hide", "_mm512_movm_epi16",
			"-ccgo-hide", "_mm512_movm_epi32",
			"-ccgo-hide", "_mm512_movm_epi64",
			"-ccgo-hide", "_mm512_movm_epi8",
			"-ccgo-hide", "_mm512_mul_epi32",
			"-ccgo-hide", "_mm512_mul_epu32",
			"-ccgo-hide", "_mm512_mul_pd",
			"-ccgo-hide", "_mm512_mul_ps",
			"-ccgo-hide", "_mm512_mulhi_epi16",
			"-ccgo-hide", "_mm512_mulhi_epu16",
			"-ccgo-hide", "_mm512_mulhrs_epi16",
			"-ccgo-hide", "_mm512_mullo_epi16",
			"-ccgo-hide", "_mm512_mullo_epi32",
			"-ccgo-hide", "_mm512_mullo_epi64",
			"-ccgo-hide", "_mm512_multishift_epi64_epi8",
			"-ccgo-hide", "_mm512_or_epi32",
			"-ccgo-hide", "_mm512_or_epi64",
			"-ccgo-hide", "_mm512_or_pd",
			"-ccgo-hide", "_mm512_or_ps",
			"-ccgo-hide", "_mm512_or_si512",
			"-ccgo-hide", "_mm512_packs_epi16",
			"-ccgo-hide", "_mm512_packs_epi32",
			"-ccgo-hide", "_mm512_packus_epi16",
			"-ccgo-hide", "_mm512_packus_epi32",
			"-ccgo-hide", "_mm512_permutevar_pd",
			"-ccgo-hide", "_mm512_permutevar_ps",
			"-ccgo-hide", "_mm512_permutex2var_epi16",
			"-ccgo-hide", "_mm512_permutex2var_epi32",
			"-ccgo-hide", "_mm512_permutex2var_epi64",
			"-ccgo-hide", "_mm512_permutex2var_epi8",
			"-ccgo-hide", "_mm512_permutex2var_pd",
			"-ccgo-hide", "_mm512_permutex2var_ps",
			"-ccgo-hide", "_mm512_permutexvar_epi16",
			"-ccgo-hide", "_mm512_permutexvar_epi32",
			"-ccgo-hide", "_mm512_permutexvar_epi64",
			"-ccgo-hide", "_mm512_permutexvar_epi8",
			"-ccgo-hide", "_mm512_permutexvar_pd",
			"-ccgo-hide", "_mm512_permutexvar_ps",
			"-ccgo-hide", "_mm512_popcnt_epi16",
			"-ccgo-hide", "_mm512_popcnt_epi32",
			"-ccgo-hide", "_mm512_popcnt_epi64",
			"-ccgo-hide", "_mm512_popcnt_epi8",
			"-ccgo-hide", "_mm512_rcp14_pd",
			"-ccgo-hide", "_mm512_rcp14_ps",
			"-ccgo-hide", "_mm512_reduce_add_epi32",
			"-ccgo-hide", "_mm512_reduce_add_epi64",
			"-ccgo-hide", "_mm512_reduce_add_pd",
			"-ccgo-hide", "_mm512_reduce_add_ps",
			"-ccgo-hide", "_mm512_reduce_and_epi32",
			"-ccgo-hide", "_mm512_reduce_and_epi64",
			"-ccgo-hide", "_mm512_reduce_max_epi32",
			"-ccgo-hide", "_mm512_reduce_max_epi64",
			"-ccgo-hide", "_mm512_reduce_max_epu32",
			"-ccgo-hide", "_mm512_reduce_max_epu64",
			"-ccgo-hide", "_mm512_reduce_max_pd",
			"-ccgo-hide", "_mm512_reduce_max_ps",
			"-ccgo-hide", "_mm512_reduce_min_epi32",
			"-ccgo-hide", "_mm512_reduce_min_epi64",
			"-ccgo-hide", "_mm512_reduce_min_epu32",
			"-ccgo-hide", "_mm512_reduce_min_epu64",
			"-ccgo-hide", "_mm512_reduce_min_pd",
			"-ccgo-hide", "_mm512_reduce_min_ps",
			"-ccgo-hide", "_mm512_reduce_mul_epi32",
			"-ccgo-hide", "_mm512_reduce_mul_epi64",
			"-ccgo-hide", "_mm512_reduce_mul_pd",
			"-ccgo-hide", "_mm512_reduce_mul_ps",
			"-ccgo-hide", "_mm512_reduce_or_epi32",
			"-ccgo-hide", "_mm512_reduce_or_epi64",
			"-ccgo-hide", "_mm512_rolv_epi32",
			"-ccgo-hide", "_mm512_rolv_epi64",
			"-ccgo-hide", "_mm512_rorv_epi32",
			"-ccgo-hide", "_mm512_rorv_epi64",
			"-ccgo-hide", "_mm512_rsqrt14_pd",
			"-ccgo-hide", "_mm512_rsqrt14_ps",
			"-ccgo-hide", "_mm512_sad_epu8",
			"-ccgo-hide", "_mm512_scalef_pd",
			"-ccgo-hide", "_mm512_scalef_ps",
			"-ccgo-hide", "_mm512_set1_epi16",
			"-ccgo-hide", "_mm512_set1_epi32",
			"-ccgo-hide", "_mm512_set1_epi64",
			"-ccgo-hide", "_mm512_set1_epi8",
			"-ccgo-hide", "_mm512_set1_pd",
			"-ccgo-hide", "_mm512_set1_ps",
			"-ccgo-hide", "_mm512_set4_epi32",
			"-ccgo-hide", "_mm512_set4_epi64",
			"-ccgo-hide", "_mm512_set4_pd",
			"-ccgo-hide", "_mm512_set4_ps",
			"-ccgo-hide", "_mm512_set_epi32",
			"-ccgo-hide", "_mm512_set_epi64",
			"-ccgo-hide", "_mm512_set_pd",
			"-ccgo-hide", "_mm512_set_ps",
			"-ccgo-hide", "_mm512_setzero_epi32",
			"-ccgo-hide", "_mm512_setzero_pd",
			"-ccgo-hide", "_mm512_setzero_ps",
			"-ccgo-hide", "_mm512_setzero_si512",
			"-ccgo-hide", "_mm512_shldv_epi16",
			"-ccgo-hide", "_mm512_shldv_epi32",
			"-ccgo-hide", "_mm512_shldv_epi64",
			"-ccgo-hide", "_mm512_shrdv_epi16",
			"-ccgo-hide", "_mm512_shrdv_epi32",
			"-ccgo-hide", "_mm512_shrdv_epi64",
			"-ccgo-hide", "_mm512_shuffle_epi8",
			"-ccgo-hide", "_mm512_sll_epi16",
			"-ccgo-hide", "_mm512_sll_epi32",
			"-ccgo-hide", "_mm512_sll_epi64",
			"-ccgo-hide", "_mm512_sllv_epi16",
			"-ccgo-hide", "_mm512_sllv_epi32",
			"-ccgo-hide", "_mm512_sllv_epi64",
			"-ccgo-hide", "_mm512_sqrt_pd",
			"-ccgo-hide", "_mm512_sqrt_ps",
			"-ccgo-hide", "_mm512_sra_epi16",
			"-ccgo-hide", "_mm512_sra_epi32",
			"-ccgo-hide", "_mm512_sra_epi64",
			"-ccgo-hide", "_mm512_srav_epi16",
			"-ccgo-hide", "_mm512_srav_epi32",
			"-ccgo-hide", "_mm512_srav_epi64",
			"-ccgo-hide", "_mm512_srl_epi16",
			"-ccgo-hide", "_mm512_srl_epi32",
			"-ccgo-hide", "_mm512_srl_epi64",
			"-ccgo-hide", "_mm512_srlv_epi16",
			"-ccgo-hide", "_mm512_srlv_epi32",
			"-ccgo-hide", "_mm512_srlv_epi64",
			"-ccgo-hide", "_mm512_store_epi32",
			"-ccgo-hide", "_mm512_store_epi64",
			"-ccgo-hide", "_mm512_store_pd",
			"-ccgo-hide", "_mm512_store_ps",
			"-ccgo-hide", "_mm512_store_si512",
			"-ccgo-hide", "_mm512_storeu_pd",
			"-ccgo-hide", "_mm512_storeu_ps",
			"-ccgo-hide", "_mm512_storeu_si512",
			"-ccgo-hide", "_mm512_stream_load_si512",
			"-ccgo-hide", "_mm512_stream_pd",
			"-ccgo-hide", "_mm512_stream_ps",
			"-ccgo-hide", "_mm512_stream_si512",
			"-ccgo-hide", "_mm512_sub_epi16",
			"-ccgo-hide", "_mm512_sub_epi32",
			"-ccgo-hide", "_mm512_sub_epi64",
			"-ccgo-hide", "_mm512_sub_epi8",
			"-ccgo-hide", "_mm512_sub_pd",
			"-ccgo-hide", "_mm512_sub_ps",
			"-ccgo-hide", "_mm512_subs_epi16",
			"-ccgo-hide", "_mm512_subs_epi8",
			"-ccgo-hide", "_mm512_subs_epu16",
			"-ccgo-hide", "_mm512_subs_epu8",
			"-ccgo-hide", "_mm512_test_epi16_mask",
			"-ccgo-hide", "_mm512_test_epi32_mask",
			"-ccgo-hide", "_mm512_test_epi64_mask",
			"-ccgo-hide", "_mm512_test_epi8_mask",
			"-ccgo-hide", "_mm512_testn_epi16_mask",
			"-ccgo-hide", "_mm512_testn_epi32_mask",
			"-ccgo-hide", "_mm512_testn_epi64_mask",
			"-ccgo-hide", "_mm512_testn_epi8_mask",
			"-ccgo-hide", "_mm512_undefined_epi32",
			"-ccgo-hide", "_mm512_undefined_pd",
			"-ccgo-hide", "_mm512_undefined_ps",
			"-ccgo-hide", "_mm512_unpackhi_epi16",
			"-ccgo-hide", "_mm512_unpackhi_epi32",
			"-ccgo-hide", "_mm512_unpackhi_epi64",
			"-ccgo-hide", "_mm512_unpackhi_epi8",
			"-ccgo-hide", "_mm512_unpackhi_pd",
			"-ccgo-hide", "_mm512_unpackhi_ps",
			"-ccgo-hide", "_mm512_unpacklo_epi16",
			"-ccgo-hide", "_mm512_unpacklo_epi32",
			"-ccgo-hide", "_mm512_unpacklo_epi64",
			"-ccgo-hide", "_mm512_unpacklo_epi8",
			"-ccgo-hide", "_mm512_unpacklo_pd",
			"-ccgo-hide", "_mm512_unpacklo_ps",
			"-ccgo-hide", "_mm512_xor_epi32",
			"-ccgo-hide", "_mm512_xor_epi64",
			"-ccgo-hide", "_mm512_xor_pd",
			"-ccgo-hide", "_mm512_xor_ps",
			"-ccgo-hide", "_mm512_xor_si512",
			"-ccgo-hide", "_mm_4fmadd_ss",
			"-ccgo-hide", "_mm_4fnmadd_ss",
			"-ccgo-hide", "_mm_abs_epi16",
			"-ccgo-hide", "_mm_abs_epi32",
			"-ccgo-hide", "_mm_abs_epi64",
			"-ccgo-hide", "_mm_abs_epi8",
			"-ccgo-hide", "_mm_abs_pi16",
			"-ccgo-hide", "_mm_abs_pi32",
			"-ccgo-hide", "_mm_abs_pi8",
			"-ccgo-hide", "_mm_add_epi16",
			"-ccgo-hide", "_mm_add_epi32",
			"-ccgo-hide", "_mm_add_epi64",
			"-ccgo-hide", "_mm_add_epi8",
			"-ccgo-hide", "_mm_add_pd",
			"-ccgo-hide", "_mm_add_pi16",
			"-ccgo-hide", "_mm_add_pi32",
			"-ccgo-hide", "_mm_add_pi8",
			"-ccgo-hide", "_mm_add_ps",
			"-ccgo-hide", "_mm_add_sd",
			"-ccgo-hide", "_mm_add_si64",
			"-ccgo-hide", "_mm_add_ss",
			"-ccgo-hide", "_mm_adds_epi16",
			"-ccgo-hide", "_mm_adds_epi8",
			"-ccgo-hide", "_mm_adds_epu16",
			"-ccgo-hide", "_mm_adds_epu8",
			"-ccgo-hide", "_mm_adds_pi16",
			"-ccgo-hide", "_mm_adds_pi8",
			"-ccgo-hide", "_mm_adds_pu16",
			"-ccgo-hide", "_mm_adds_pu8",
			"-ccgo-hide", "_mm_addsub_pd",
			"-ccgo-hide", "_mm_addsub_ps",
			"-ccgo-hide", "_mm_aesdec_epi128",
			"-ccgo-hide", "_mm_aesdec_si128",
			"-ccgo-hide", "_mm_aesdeclast_epi128",
			"-ccgo-hide", "_mm_aesdeclast_si128",
			"-ccgo-hide", "_mm_aesenc_epi128",
			"-ccgo-hide", "_mm_aesenc_si128",
			"-ccgo-hide", "_mm_aesenclast_epi128",
			"-ccgo-hide", "_mm_aesenclast_si128",
			"-ccgo-hide", "_mm_aesimc_si128",
			"-ccgo-hide", "_mm_and_pd",
			"-ccgo-hide", "_mm_and_ps",
			"-ccgo-hide", "_mm_and_si128",
			"-ccgo-hide", "_mm_and_si64",
			"-ccgo-hide", "_mm_andnot_pd",
			"-ccgo-hide", "_mm_andnot_ps",
			"-ccgo-hide", "_mm_andnot_si128",
			"-ccgo-hide", "_mm_andnot_si64",
			"-ccgo-hide", "_mm_avg_epu16",
			"-ccgo-hide", "_mm_avg_epu8",
			"-ccgo-hide", "_mm_avg_pu16",
			"-ccgo-hide", "_mm_avg_pu8",
			"-ccgo-hide", "_mm_bitshuffle_epi64_mask",
			"-ccgo-hide", "_mm_blendv_epi8",
			"-ccgo-hide", "_mm_blendv_pd",
			"-ccgo-hide", "_mm_blendv_ps",
			"-ccgo-hide", "_mm_broadcast_i32x2",
			"-ccgo-hide", "_mm_broadcast_ss",
			"-ccgo-hide", "_mm_broadcastb_epi8",
			"-ccgo-hide", "_mm_broadcastd_epi32",
			"-ccgo-hide", "_mm_broadcastmb_epi64",
			"-ccgo-hide", "_mm_broadcastmw_epi32",
			"-ccgo-hide", "_mm_broadcastq_epi64",
			"-ccgo-hide", "_mm_broadcastss_ps",
			"-ccgo-hide", "_mm_broadcastw_epi16",
			"-ccgo-hide", "_mm_castpd_ps",
			"-ccgo-hide", "_mm_castpd_si128",
			"-ccgo-hide", "_mm_castps_pd",
			"-ccgo-hide", "_mm_castps_si128",
			"-ccgo-hide", "_mm_castsi128_pd",
			"-ccgo-hide", "_mm_castsi128_ps",
			"-ccgo-hide", "_mm_clflush",
			"-ccgo-hide", "_mm_clflushopt",
			"-ccgo-hide", "_mm_clwb",
			"-ccgo-hide", "_mm_clzero",
			"-ccgo-hide", "_mm_cmov_si128",
			"-ccgo-hide", "_mm_cmpeq_epi16",
			"-ccgo-hide", "_mm_cmpeq_epi16_mask",
			"-ccgo-hide", "_mm_cmpeq_epi32",
			"-ccgo-hide", "_mm_cmpeq_epi32_mask",
			"-ccgo-hide", "_mm_cmpeq_epi64",
			"-ccgo-hide", "_mm_cmpeq_epi64_mask",
			"-ccgo-hide", "_mm_cmpeq_epi8",
			"-ccgo-hide", "_mm_cmpeq_epi8_mask",
			"-ccgo-hide", "_mm_cmpeq_epu16_mask",
			"-ccgo-hide", "_mm_cmpeq_epu32_mask",
			"-ccgo-hide", "_mm_cmpeq_epu64_mask",
			"-ccgo-hide", "_mm_cmpeq_epu8_mask",
			"-ccgo-hide", "_mm_cmpeq_pd",
			"-ccgo-hide", "_mm_cmpeq_pi16",
			"-ccgo-hide", "_mm_cmpeq_pi32",
			"-ccgo-hide", "_mm_cmpeq_pi8",
			"-ccgo-hide", "_mm_cmpeq_ps",
			"-ccgo-hide", "_mm_cmpeq_sd",
			"-ccgo-hide", "_mm_cmpeq_ss",
			"-ccgo-hide", "_mm_cmpge_epi16_mask",
			"-ccgo-hide", "_mm_cmpge_epi32_mask",
			"-ccgo-hide", "_mm_cmpge_epi64_mask",
			"-ccgo-hide", "_mm_cmpge_epi8_mask",
			"-ccgo-hide", "_mm_cmpge_epu16_mask",
			"-ccgo-hide", "_mm_cmpge_epu32_mask",
			"-ccgo-hide", "_mm_cmpge_epu64_mask",
			"-ccgo-hide", "_mm_cmpge_epu8_mask",
			"-ccgo-hide", "_mm_cmpge_pd",
			"-ccgo-hide", "_mm_cmpge_ps",
			"-ccgo-hide", "_mm_cmpge_sd",
			"-ccgo-hide", "_mm_cmpge_ss",
			"-ccgo-hide", "_mm_cmpgt_epi16",
			"-ccgo-hide", "_mm_cmpgt_epi16_mask",
			"-ccgo-hide", "_mm_cmpgt_epi32",
			"-ccgo-hide", "_mm_cmpgt_epi32_mask",
			"-ccgo-hide", "_mm_cmpgt_epi64",
			"-ccgo-hide", "_mm_cmpgt_epi64_mask",
			"-ccgo-hide", "_mm_cmpgt_epi8",
			"-ccgo-hide", "_mm_cmpgt_epi8_mask",
			"-ccgo-hide", "_mm_cmpgt_epu16_mask",
			"-ccgo-hide", "_mm_cmpgt_epu32_mask",
			"-ccgo-hide", "_mm_cmpgt_epu64_mask",
			"-ccgo-hide", "_mm_cmpgt_epu8_mask",
			"-ccgo-hide", "_mm_cmpgt_pd",
			"-ccgo-hide", "_mm_cmpgt_pi16",
			"-ccgo-hide", "_mm_cmpgt_pi32",
			"-ccgo-hide", "_mm_cmpgt_pi8",
			"-ccgo-hide", "_mm_cmpgt_ps",
			"-ccgo-hide", "_mm_cmpgt_sd",
			"-ccgo-hide", "_mm_cmpgt_ss",
			"-ccgo-hide", "_mm_cmple_epi16_mask",
			"-ccgo-hide", "_mm_cmple_epi32_mask",
			"-ccgo-hide", "_mm_cmple_epi64_mask",
			"-ccgo-hide", "_mm_cmple_epi8_mask",
			"-ccgo-hide", "_mm_cmple_epu16_mask",
			"-ccgo-hide", "_mm_cmple_epu32_mask",
			"-ccgo-hide", "_mm_cmple_epu64_mask",
			"-ccgo-hide", "_mm_cmple_epu8_mask",
			"-ccgo-hide", "_mm_cmple_pd",
			"-ccgo-hide", "_mm_cmple_ps",
			"-ccgo-hide", "_mm_cmple_sd",
			"-ccgo-hide", "_mm_cmple_ss",
			"-ccgo-hide", "_mm_cmplt_epi16",
			"-ccgo-hide", "_mm_cmplt_epi16_mask",
			"-ccgo-hide", "_mm_cmplt_epi32",
			"-ccgo-hide", "_mm_cmplt_epi32_mask",
			"-ccgo-hide", "_mm_cmplt_epi64_mask",
			"-ccgo-hide", "_mm_cmplt_epi8",
			"-ccgo-hide", "_mm_cmplt_epi8_mask",
			"-ccgo-hide", "_mm_cmplt_epu16_mask",
			"-ccgo-hide", "_mm_cmplt_epu32_mask",
			"-ccgo-hide", "_mm_cmplt_epu64_mask",
			"-ccgo-hide", "_mm_cmplt_epu8_mask",
			"-ccgo-hide", "_mm_cmplt_pd",
			"-ccgo-hide", "_mm_cmplt_ps",
			"-ccgo-hide", "_mm_cmplt_sd",
			"-ccgo-hide", "_mm_cmplt_ss",
			"-ccgo-hide", "_mm_cmpneq_epi16_mask",
			"-ccgo-hide", "_mm_cmpneq_epi32_mask",
			"-ccgo-hide", "_mm_cmpneq_epi64_mask",
			"-ccgo-hide", "_mm_cmpneq_epi8_mask",
			"-ccgo-hide", "_mm_cmpneq_epu16_mask",
			"-ccgo-hide", "_mm_cmpneq_epu32_mask",
			"-ccgo-hide", "_mm_cmpneq_epu64_mask",
			"-ccgo-hide", "_mm_cmpneq_epu8_mask",
			"-ccgo-hide", "_mm_cmpneq_pd",
			"-ccgo-hide", "_mm_cmpneq_ps",
			"-ccgo-hide", "_mm_cmpneq_sd",
			"-ccgo-hide", "_mm_cmpneq_ss",
			"-ccgo-hide", "_mm_cmpnge_pd",
			"-ccgo-hide", "_mm_cmpnge_ps",
			"-ccgo-hide", "_mm_cmpnge_sd",
			"-ccgo-hide", "_mm_cmpnge_ss",
			"-ccgo-hide", "_mm_cmpngt_pd",
			"-ccgo-hide", "_mm_cmpngt_ps",
			"-ccgo-hide", "_mm_cmpngt_sd",
			"-ccgo-hide", "_mm_cmpngt_ss",
			"-ccgo-hide", "_mm_cmpnle_pd",
			"-ccgo-hide", "_mm_cmpnle_ps",
			"-ccgo-hide", "_mm_cmpnle_sd",
			"-ccgo-hide", "_mm_cmpnle_ss",
			"-ccgo-hide", "_mm_cmpnlt_pd",
			"-ccgo-hide", "_mm_cmpnlt_ps",
			"-ccgo-hide", "_mm_cmpnlt_sd",
			"-ccgo-hide", "_mm_cmpnlt_ss",
			"-ccgo-hide", "_mm_cmpord_pd",
			"-ccgo-hide", "_mm_cmpord_ps",
			"-ccgo-hide", "_mm_cmpord_sd",
			"-ccgo-hide", "_mm_cmpord_ss",
			"-ccgo-hide", "_mm_cmpunord_pd",
			"-ccgo-hide", "_mm_cmpunord_ps",
			"-ccgo-hide", "_mm_cmpunord_sd",
			"-ccgo-hide", "_mm_cmpunord_ss",
			"-ccgo-hide", "_mm_comeq_epi16",
			"-ccgo-hide", "_mm_comeq_epi32",
			"-ccgo-hide", "_mm_comeq_epi64",
			"-ccgo-hide", "_mm_comeq_epi8",
			"-ccgo-hide", "_mm_comeq_epu16",
			"-ccgo-hide", "_mm_comeq_epu32",
			"-ccgo-hide", "_mm_comeq_epu64",
			"-ccgo-hide", "_mm_comeq_epu8",
			"-ccgo-hide", "_mm_comfalse_epi16",
			"-ccgo-hide", "_mm_comfalse_epi32",
			"-ccgo-hide", "_mm_comfalse_epi64",
			"-ccgo-hide", "_mm_comfalse_epi8",
			"-ccgo-hide", "_mm_comfalse_epu16",
			"-ccgo-hide", "_mm_comfalse_epu32",
			"-ccgo-hide", "_mm_comfalse_epu64",
			"-ccgo-hide", "_mm_comfalse_epu8",
			"-ccgo-hide", "_mm_comge_epi16",
			"-ccgo-hide", "_mm_comge_epi32",
			"-ccgo-hide", "_mm_comge_epi64",
			"-ccgo-hide", "_mm_comge_epi8",
			"-ccgo-hide", "_mm_comge_epu16",
			"-ccgo-hide", "_mm_comge_epu32",
			"-ccgo-hide", "_mm_comge_epu64",
			"-ccgo-hide", "_mm_comge_epu8",
			"-ccgo-hide", "_mm_comgt_epi16",
			"-ccgo-hide", "_mm_comgt_epi32",
			"-ccgo-hide", "_mm_comgt_epi64",
			"-ccgo-hide", "_mm_comgt_epi8",
			"-ccgo-hide", "_mm_comgt_epu16",
			"-ccgo-hide", "_mm_comgt_epu32",
			"-ccgo-hide", "_mm_comgt_epu64",
			"-ccgo-hide", "_mm_comgt_epu8",
			"-ccgo-hide", "_mm_comieq_sd",
			"-ccgo-hide", "_mm_comieq_ss",
			"-ccgo-hide", "_mm_comige_sd",
			"-ccgo-hide", "_mm_comige_ss",
			"-ccgo-hide", "_mm_comigt_sd",
			"-ccgo-hide", "_mm_comigt_ss",
			"-ccgo-hide", "_mm_comile_sd",
			"-ccgo-hide", "_mm_comile_ss",
			"-ccgo-hide", "_mm_comilt_sd",
			"-ccgo-hide", "_mm_comilt_ss",
			"-ccgo-hide", "_mm_comineq_sd",
			"-ccgo-hide", "_mm_comineq_ss",
			"-ccgo-hide", "_mm_comle_epi16",
			"-ccgo-hide", "_mm_comle_epi32",
			"-ccgo-hide", "_mm_comle_epi64",
			"-ccgo-hide", "_mm_comle_epi8",
			"-ccgo-hide", "_mm_comle_epu16",
			"-ccgo-hide", "_mm_comle_epu32",
			"-ccgo-hide", "_mm_comle_epu64",
			"-ccgo-hide", "_mm_comle_epu8",
			"-ccgo-hide", "_mm_comlt_epi16",
			"-ccgo-hide", "_mm_comlt_epi32",
			"-ccgo-hide", "_mm_comlt_epi64",
			"-ccgo-hide", "_mm_comlt_epi8",
			"-ccgo-hide", "_mm_comlt_epu16",
			"-ccgo-hide", "_mm_comlt_epu32",
			"-ccgo-hide", "_mm_comlt_epu64",
			"-ccgo-hide", "_mm_comlt_epu8",
			"-ccgo-hide", "_mm_comneq_epi16",
			"-ccgo-hide", "_mm_comneq_epi32",
			"-ccgo-hide", "_mm_comneq_epi64",
			"-ccgo-hide", "_mm_comneq_epi8",
			"-ccgo-hide", "_mm_comneq_epu16",
			"-ccgo-hide", "_mm_comneq_epu32",
			"-ccgo-hide", "_mm_comneq_epu64",
			"-ccgo-hide", "_mm_comneq_epu8",
			"-ccgo-hide", "_mm_comtrue_epi16",
			"-ccgo-hide", "_mm_comtrue_epi32",
			"-ccgo-hide", "_mm_comtrue_epi64",
			"-ccgo-hide", "_mm_comtrue_epi8",
			"-ccgo-hide", "_mm_comtrue_epu16",
			"-ccgo-hide", "_mm_comtrue_epu32",
			"-ccgo-hide", "_mm_comtrue_epu64",
			"-ccgo-hide", "_mm_comtrue_epu8",
			"-ccgo-hide", "_mm_conflict_epi32",
			"-ccgo-hide", "_mm_conflict_epi64",
			"-ccgo-hide", "_mm_crc32_u16",
			"-ccgo-hide", "_mm_crc32_u32",
			"-ccgo-hide", "_mm_crc32_u64",
			"-ccgo-hide", "_mm_crc32_u8",
			"-ccgo-hide", "_mm_cvt_pi2ps",
			"-ccgo-hide", "_mm_cvt_ps2pi",
			"-ccgo-hide", "_mm_cvt_si2ss",
			"-ccgo-hide", "_mm_cvt_ss2si",
			"-ccgo-hide", "_mm_cvtepi16_epi32",
			"-ccgo-hide", "_mm_cvtepi16_epi64",
			"-ccgo-hide", "_mm_cvtepi16_epi8",
			"-ccgo-hide", "_mm_cvtepi32_epi16",
			"-ccgo-hide", "_mm_cvtepi32_epi64",
			"-ccgo-hide", "_mm_cvtepi32_epi8",
			"-ccgo-hide", "_mm_cvtepi32_pd",
			"-ccgo-hide", "_mm_cvtepi32_ps",
			"-ccgo-hide", "_mm_cvtepi64_epi16",
			"-ccgo-hide", "_mm_cvtepi64_epi32",
			"-ccgo-hide", "_mm_cvtepi64_epi8",
			"-ccgo-hide", "_mm_cvtepi64_pd",
			"-ccgo-hide", "_mm_cvtepi64_ps",
			"-ccgo-hide", "_mm_cvtepi8_epi16",
			"-ccgo-hide", "_mm_cvtepi8_epi32",
			"-ccgo-hide", "_mm_cvtepi8_epi64",
			"-ccgo-hide", "_mm_cvtepu16_epi32",
			"-ccgo-hide", "_mm_cvtepu16_epi64",
			"-ccgo-hide", "_mm_cvtepu32_epi64",
			"-ccgo-hide", "_mm_cvtepu32_pd",
			"-ccgo-hide", "_mm_cvtepu32_ps",
			"-ccgo-hide", "_mm_cvtepu64_pd",
			"-ccgo-hide", "_mm_cvtepu64_ps",
			"-ccgo-hide", "_mm_cvtepu8_epi16",
			"-ccgo-hide", "_mm_cvtepu8_epi32",
			"-ccgo-hide", "_mm_cvtepu8_epi64",
			"-ccgo-hide", "_mm_cvtm64_si64",
			"-ccgo-hide", "_mm_cvtpd_epi32",
			"-ccgo-hide", "_mm_cvtpd_epi64",
			"-ccgo-hide", "_mm_cvtpd_epu32",
			"-ccgo-hide", "_mm_cvtpd_epu64",
			"-ccgo-hide", "_mm_cvtpd_pi32",
			"-ccgo-hide", "_mm_cvtpd_ps",
			"-ccgo-hide", "_mm_cvtph_ps",
			"-ccgo-hide", "_mm_cvtpi16_ps",
			"-ccgo-hide", "_mm_cvtpi32_pd",
			"-ccgo-hide", "_mm_cvtpi32_ps",
			"-ccgo-hide", "_mm_cvtpi32x2_ps",
			"-ccgo-hide", "_mm_cvtpi8_ps",
			"-ccgo-hide", "_mm_cvtps_epi32",
			"-ccgo-hide", "_mm_cvtps_epi64",
			"-ccgo-hide", "_mm_cvtps_epu32",
			"-ccgo-hide", "_mm_cvtps_epu64",
			"-ccgo-hide", "_mm_cvtps_pd",
			"-ccgo-hide", "_mm_cvtps_pi16",
			"-ccgo-hide", "_mm_cvtps_pi32",
			"-ccgo-hide", "_mm_cvtps_pi8",
			"-ccgo-hide", "_mm_cvtpu16_ps",
			"-ccgo-hide", "_mm_cvtpu8_ps",
			"-ccgo-hide", "_mm_cvtsd_f64",
			"-ccgo-hide", "_mm_cvtsd_si32",
			"-ccgo-hide", "_mm_cvtsd_si64",
			"-ccgo-hide", "_mm_cvtsd_si64x",
			"-ccgo-hide", "_mm_cvtsd_ss",
			"-ccgo-hide", "_mm_cvtsd_u32",
			"-ccgo-hide", "_mm_cvtsd_u64",
			"-ccgo-hide", "_mm_cvtsepi16_epi8",
			"-ccgo-hide", "_mm_cvtsepi32_epi16",
			"-ccgo-hide", "_mm_cvtsepi32_epi8",
			"-ccgo-hide", "_mm_cvtsepi64_epi16",
			"-ccgo-hide", "_mm_cvtsepi64_epi32",
			"-ccgo-hide", "_mm_cvtsepi64_epi8",
			"-ccgo-hide", "_mm_cvtsi128_si32",
			"-ccgo-hide", "_mm_cvtsi128_si64",
			"-ccgo-hide", "_mm_cvtsi128_si64x",
			"-ccgo-hide", "_mm_cvtsi32_sd",
			"-ccgo-hide", "_mm_cvtsi32_si128",
			"-ccgo-hide", "_mm_cvtsi32_si64",
			"-ccgo-hide", "_mm_cvtsi32_ss",
			"-ccgo-hide", "_mm_cvtsi64_m64",
			"-ccgo-hide", "_mm_cvtsi64_sd",
			"-ccgo-hide", "_mm_cvtsi64_si128",
			"-ccgo-hide", "_mm_cvtsi64_si32",
			"-ccgo-hide", "_mm_cvtsi64_si64x",
			"-ccgo-hide", "_mm_cvtsi64_ss",
			"-ccgo-hide", "_mm_cvtsi64x_sd",
			"-ccgo-hide", "_mm_cvtsi64x_si128",
			"-ccgo-hide", "_mm_cvtsi64x_si64",
			"-ccgo-hide", "_mm_cvtsi64x_ss",
			"-ccgo-hide", "_mm_cvtss_f32",
			"-ccgo-hide", "_mm_cvtss_sd",
			"-ccgo-hide", "_mm_cvtss_si32",
			"-ccgo-hide", "_mm_cvtss_si64",
			"-ccgo-hide", "_mm_cvtss_si64x",
			"-ccgo-hide", "_mm_cvtss_u32",
			"-ccgo-hide", "_mm_cvtss_u64",
			"-ccgo-hide", "_mm_cvtt_ps2pi",
			"-ccgo-hide", "_mm_cvtt_ss2si",
			"-ccgo-hide", "_mm_cvttpd_epi32",
			"-ccgo-hide", "_mm_cvttpd_epi64",
			"-ccgo-hide", "_mm_cvttpd_epu32",
			"-ccgo-hide", "_mm_cvttpd_epu64",
			"-ccgo-hide", "_mm_cvttpd_pi32",
			"-ccgo-hide", "_mm_cvttps_epi32",
			"-ccgo-hide", "_mm_cvttps_epi64",
			"-ccgo-hide", "_mm_cvttps_epu32",
			"-ccgo-hide", "_mm_cvttps_epu64",
			"-ccgo-hide", "_mm_cvttps_pi32",
			"-ccgo-hide", "_mm_cvttsd_i32",
			"-ccgo-hide", "_mm_cvttsd_i64",
			"-ccgo-hide", "_mm_cvttsd_si32",
			"-ccgo-hide", "_mm_cvttsd_si64",
			"-ccgo-hide", "_mm_cvttsd_si64x",
			"-ccgo-hide", "_mm_cvttsd_u32",
			"-ccgo-hide", "_mm_cvttsd_u64",
			"-ccgo-hide", "_mm_cvttss_i32",
			"-ccgo-hide", "_mm_cvttss_i64",
			"-ccgo-hide", "_mm_cvttss_si32",
			"-ccgo-hide", "_mm_cvttss_si64",
			"-ccgo-hide", "_mm_cvttss_si64x",
			"-ccgo-hide", "_mm_cvttss_u32",
			"-ccgo-hide", "_mm_cvttss_u64",
			"-ccgo-hide", "_mm_cvtu32_sd",
			"-ccgo-hide", "_mm_cvtu32_ss",
			"-ccgo-hide", "_mm_cvtu64_sd",
			"-ccgo-hide", "_mm_cvtu64_ss",
			"-ccgo-hide", "_mm_cvtusepi16_epi8",
			"-ccgo-hide", "_mm_cvtusepi32_epi16",
			"-ccgo-hide", "_mm_cvtusepi32_epi8",
			"-ccgo-hide", "_mm_cvtusepi64_epi16",
			"-ccgo-hide", "_mm_cvtusepi64_epi32",
			"-ccgo-hide", "_mm_cvtusepi64_epi8",
			"-ccgo-hide", "_mm_div_pd",
			"-ccgo-hide", "_mm_div_ps",
			"-ccgo-hide", "_mm_div_sd",
			"-ccgo-hide", "_mm_div_ss",
			"-ccgo-hide", "_mm_dpbusd_epi32",
			"-ccgo-hide", "_mm_dpbusds_epi32",
			"-ccgo-hide", "_mm_dpwssd_epi32",
			"-ccgo-hide", "_mm_dpwssds_epi32",
			"-ccgo-hide", "_mm_empty",
			"-ccgo-hide", "_mm_extract_si64",
			"-ccgo-hide", "_mm_fmadd_pd",
			"-ccgo-hide", "_mm_fmadd_ps",
			"-ccgo-hide", "_mm_fmadd_sd",
			"-ccgo-hide", "_mm_fmadd_ss",
			"-ccgo-hide", "_mm_fmaddsub_pd",
			"-ccgo-hide", "_mm_fmaddsub_ps",
			"-ccgo-hide", "_mm_fmsub_pd",
			"-ccgo-hide", "_mm_fmsub_ps",
			"-ccgo-hide", "_mm_fmsub_sd",
			"-ccgo-hide", "_mm_fmsub_ss",
			"-ccgo-hide", "_mm_fmsubadd_pd",
			"-ccgo-hide", "_mm_fmsubadd_ps",
			"-ccgo-hide", "_mm_fnmadd_pd",
			"-ccgo-hide", "_mm_fnmadd_ps",
			"-ccgo-hide", "_mm_fnmadd_sd",
			"-ccgo-hide", "_mm_fnmadd_ss",
			"-ccgo-hide", "_mm_fnmsub_pd",
			"-ccgo-hide", "_mm_fnmsub_ps",
			"-ccgo-hide", "_mm_fnmsub_sd",
			"-ccgo-hide", "_mm_fnmsub_ss",
			"-ccgo-hide", "_mm_frcz_pd",
			"-ccgo-hide", "_mm_frcz_ps",
			"-ccgo-hide", "_mm_frcz_sd",
			"-ccgo-hide", "_mm_frcz_ss",
			"-ccgo-hide", "_mm_getcsr",
			"-ccgo-hide", "_mm_getexp_pd",
			"-ccgo-hide", "_mm_getexp_ps",
			"-ccgo-hide", "_mm_gf2p8mul_epi8",
			"-ccgo-hide", "_mm_hadd_epi16",
			"-ccgo-hide", "_mm_hadd_epi32",
			"-ccgo-hide", "_mm_hadd_pd",
			"-ccgo-hide", "_mm_hadd_pi16",
			"-ccgo-hide", "_mm_hadd_pi32",
			"-ccgo-hide", "_mm_hadd_ps",
			"-ccgo-hide", "_mm_haddd_epi16",
			"-ccgo-hide", "_mm_haddd_epi8",
			"-ccgo-hide", "_mm_haddd_epu16",
			"-ccgo-hide", "_mm_haddd_epu8",
			"-ccgo-hide", "_mm_haddq_epi16",
			"-ccgo-hide", "_mm_haddq_epi32",
			"-ccgo-hide", "_mm_haddq_epi8",
			"-ccgo-hide", "_mm_haddq_epu16",
			"-ccgo-hide", "_mm_haddq_epu32",
			"-ccgo-hide", "_mm_haddq_epu8",
			"-ccgo-hide", "_mm_hadds_epi16",
			"-ccgo-hide", "_mm_hadds_pi16",
			"-ccgo-hide", "_mm_haddw_epi8",
			"-ccgo-hide", "_mm_haddw_epu8",
			"-ccgo-hide", "_mm_hsub_epi16",
			"-ccgo-hide", "_mm_hsub_epi32",
			"-ccgo-hide", "_mm_hsub_pd",
			"-ccgo-hide", "_mm_hsub_pi16",
			"-ccgo-hide", "_mm_hsub_pi32",
			"-ccgo-hide", "_mm_hsub_ps",
			"-ccgo-hide", "_mm_hsubd_epi16",
			"-ccgo-hide", "_mm_hsubq_epi32",
			"-ccgo-hide", "_mm_hsubs_epi16",
			"-ccgo-hide", "_mm_hsubs_pi16",
			"-ccgo-hide", "_mm_hsubw_epi8",
			"-ccgo-hide", "_mm_insert_si64",
			"-ccgo-hide", "_mm_lddqu_si128",
			"-ccgo-hide", "_mm_lfence",
			"-ccgo-hide", "_mm_load1_pd",
			"-ccgo-hide", "_mm_load1_ps",
			"-ccgo-hide", "_mm_load_pd",
			"-ccgo-hide", "_mm_load_pd1",
			"-ccgo-hide", "_mm_load_ps",
			"-ccgo-hide", "_mm_load_ps1",
			"-ccgo-hide", "_mm_load_sd",
			"-ccgo-hide", "_mm_load_si128",
			"-ccgo-hide", "_mm_load_ss",
			"-ccgo-hide", "_mm_loaddup_pd",
			"-ccgo-hide", "_mm_loadh_pd",
			"-ccgo-hide", "_mm_loadh_pi",
			"-ccgo-hide", "_mm_loadl_epi64",
			"-ccgo-hide", "_mm_loadl_pd",
			"-ccgo-hide", "_mm_loadl_pi",
			"-ccgo-hide", "_mm_loadr_pd",
			"-ccgo-hide", "_mm_loadr_ps",
			"-ccgo-hide", "_mm_loadu_pd",
			"-ccgo-hide", "_mm_loadu_ps",
			"-ccgo-hide", "_mm_loadu_si128",
			"-ccgo-hide", "_mm_lzcnt_epi32",
			"-ccgo-hide", "_mm_lzcnt_epi64",
			"-ccgo-hide", "_mm_macc_epi16",
			"-ccgo-hide", "_mm_macc_epi32",
			"-ccgo-hide", "_mm_macc_pd",
			"-ccgo-hide", "_mm_macc_ps",
			"-ccgo-hide", "_mm_macc_sd",
			"-ccgo-hide", "_mm_macc_ss",
			"-ccgo-hide", "_mm_maccd_epi16",
			"-ccgo-hide", "_mm_macchi_epi32",
			"-ccgo-hide", "_mm_macclo_epi32",
			"-ccgo-hide", "_mm_maccs_epi16",
			"-ccgo-hide", "_mm_maccs_epi32",
			"-ccgo-hide", "_mm_maccsd_epi16",
			"-ccgo-hide", "_mm_maccshi_epi32",
			"-ccgo-hide", "_mm_maccslo_epi32",
			"-ccgo-hide", "_mm_madd52hi_epu64",
			"-ccgo-hide", "_mm_madd52lo_epu64",
			"-ccgo-hide", "_mm_madd_epi16",
			"-ccgo-hide", "_mm_madd_pi16",
			"-ccgo-hide", "_mm_maddd_epi16",
			"-ccgo-hide", "_mm_maddsd_epi16",
			"-ccgo-hide", "_mm_maddsub_pd",
			"-ccgo-hide", "_mm_maddsub_ps",
			"-ccgo-hide", "_mm_maddubs_epi16",
			"-ccgo-hide", "_mm_maddubs_pi16",
			"-ccgo-hide", "_mm_mask2_permutex2var_epi16",
			"-ccgo-hide", "_mm_mask2_permutex2var_epi32",
			"-ccgo-hide", "_mm_mask2_permutex2var_epi64",
			"-ccgo-hide", "_mm_mask2_permutex2var_epi8",
			"-ccgo-hide", "_mm_mask2_permutex2var_pd",
			"-ccgo-hide", "_mm_mask2_permutex2var_ps",
			"-ccgo-hide", "_mm_mask3_fmadd_pd",
			"-ccgo-hide", "_mm_mask3_fmadd_ps",
			"-ccgo-hide", "_mm_mask3_fmaddsub_pd",
			"-ccgo-hide", "_mm_mask3_fmaddsub_ps",
			"-ccgo-hide", "_mm_mask3_fmsub_pd",
			"-ccgo-hide", "_mm_mask3_fmsub_ps",
			"-ccgo-hide", "_mm_mask3_fmsubadd_pd",
			"-ccgo-hide", "_mm_mask3_fmsubadd_ps",
			"-ccgo-hide", "_mm_mask3_fnmadd_pd",
			"-ccgo-hide", "_mm_mask3_fnmadd_ps",
			"-ccgo-hide", "_mm_mask3_fnmsub_pd",
			"-ccgo-hide", "_mm_mask3_fnmsub_ps",
			"-ccgo-hide", "_mm_mask_4fmadd_ss",
			"-ccgo-hide", "_mm_mask_4fnmadd_ss",
			"-ccgo-hide", "_mm_mask_abs_epi16",
			"-ccgo-hide", "_mm_mask_abs_epi32",
			"-ccgo-hide", "_mm_mask_abs_epi64",
			"-ccgo-hide", "_mm_mask_abs_epi8",
			"-ccgo-hide", "_mm_mask_add_epi16",
			"-ccgo-hide", "_mm_mask_add_epi32",
			"-ccgo-hide", "_mm_mask_add_epi64",
			"-ccgo-hide", "_mm_mask_add_epi8",
			"-ccgo-hide", "_mm_mask_add_pd",
			"-ccgo-hide", "_mm_mask_add_ps",
			"-ccgo-hide", "_mm_mask_add_sd",
			"-ccgo-hide", "_mm_mask_add_ss",
			"-ccgo-hide", "_mm_mask_adds_epi16",
			"-ccgo-hide", "_mm_mask_adds_epi8",
			"-ccgo-hide", "_mm_mask_adds_epu16",
			"-ccgo-hide", "_mm_mask_adds_epu8",
			"-ccgo-hide", "_mm_mask_and_epi32",
			"-ccgo-hide", "_mm_mask_and_epi64",
			"-ccgo-hide", "_mm_mask_and_pd",
			"-ccgo-hide", "_mm_mask_and_ps",
			"-ccgo-hide", "_mm_mask_andnot_epi32",
			"-ccgo-hide", "_mm_mask_andnot_epi64",
			"-ccgo-hide", "_mm_mask_andnot_pd",
			"-ccgo-hide", "_mm_mask_andnot_ps",
			"-ccgo-hide", "_mm_mask_avg_epu16",
			"-ccgo-hide", "_mm_mask_avg_epu8",
			"-ccgo-hide", "_mm_mask_bitshuffle_epi64_mask",
			"-ccgo-hide", "_mm_mask_broadcast_i32x2",
			"-ccgo-hide", "_mm_mask_broadcastb_epi8",
			"-ccgo-hide", "_mm_mask_broadcastd_epi32",
			"-ccgo-hide", "_mm_mask_broadcastq_epi64",
			"-ccgo-hide", "_mm_mask_broadcastss_ps",
			"-ccgo-hide", "_mm_mask_broadcastw_epi16",
			"-ccgo-hide", "_mm_mask_cmpeq_epi16_mask",
			"-ccgo-hide", "_mm_mask_cmpeq_epi32_mask",
			"-ccgo-hide", "_mm_mask_cmpeq_epi64_mask",
			"-ccgo-hide", "_mm_mask_cmpeq_epi8_mask",
			"-ccgo-hide", "_mm_mask_cmpeq_epu16_mask",
			"-ccgo-hide", "_mm_mask_cmpeq_epu32_mask",
			"-ccgo-hide", "_mm_mask_cmpeq_epu64_mask",
			"-ccgo-hide", "_mm_mask_cmpeq_epu8_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epi16_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epi32_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epi64_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epi8_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epu16_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epu32_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epu64_mask",
			"-ccgo-hide", "_mm_mask_cmpge_epu8_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epi16_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epi32_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epi64_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epi8_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epu16_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epu32_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epu64_mask",
			"-ccgo-hide", "_mm_mask_cmpgt_epu8_mask",
			"-ccgo-hide", "_mm_mask_cmple_epi16_mask",
			"-ccgo-hide", "_mm_mask_cmple_epi32_mask",
			"-ccgo-hide", "_mm_mask_cmple_epi64_mask",
			"-ccgo-hide", "_mm_mask_cmple_epi8_mask",
			"-ccgo-hide", "_mm_mask_cmple_epu16_mask",
			"-ccgo-hide", "_mm_mask_cmple_epu32_mask",
			"-ccgo-hide", "_mm_mask_cmple_epu64_mask",
			"-ccgo-hide", "_mm_mask_cmple_epu8_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epi16_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epi32_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epi64_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epi8_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epu16_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epu32_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epu64_mask",
			"-ccgo-hide", "_mm_mask_cmplt_epu8_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epi16_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epi32_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epi64_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epi8_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epu16_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epu32_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epu64_mask",
			"-ccgo-hide", "_mm_mask_cmpneq_epu8_mask",
			"-ccgo-hide", "_mm_mask_compress_epi16",
			"-ccgo-hide", "_mm_mask_compress_epi32",
			"-ccgo-hide", "_mm_mask_compress_epi64",
			"-ccgo-hide", "_mm_mask_compress_epi8",
			"-ccgo-hide", "_mm_mask_compress_pd",
			"-ccgo-hide", "_mm_mask_compress_ps",
			"-ccgo-hide", "_mm_mask_compressstoreu_epi16",
			"-ccgo-hide", "_mm_mask_compressstoreu_epi32",
			"-ccgo-hide", "_mm_mask_compressstoreu_epi64",
			"-ccgo-hide", "_mm_mask_compressstoreu_epi8",
			"-ccgo-hide", "_mm_mask_compressstoreu_pd",
			"-ccgo-hide", "_mm_mask_compressstoreu_ps",
			"-ccgo-hide", "_mm_mask_conflict_epi32",
			"-ccgo-hide", "_mm_mask_conflict_epi64",
			"-ccgo-hide", "_mm_mask_cvtepi16_epi32",
			"-ccgo-hide", "_mm_mask_cvtepi16_epi64",
			"-ccgo-hide", "_mm_mask_cvtepi16_epi8",
			"-ccgo-hide", "_mm_mask_cvtepi16_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvtepi32_epi16",
			"-ccgo-hide", "_mm_mask_cvtepi32_epi64",
			"-ccgo-hide", "_mm_mask_cvtepi32_epi8",
			"-ccgo-hide", "_mm_mask_cvtepi32_pd",
			"-ccgo-hide", "_mm_mask_cvtepi32_ps",
			"-ccgo-hide", "_mm_mask_cvtepi32_storeu_epi16",
			"-ccgo-hide", "_mm_mask_cvtepi32_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvtepi64_epi16",
			"-ccgo-hide", "_mm_mask_cvtepi64_epi32",
			"-ccgo-hide", "_mm_mask_cvtepi64_epi8",
			"-ccgo-hide", "_mm_mask_cvtepi64_pd",
			"-ccgo-hide", "_mm_mask_cvtepi64_ps",
			"-ccgo-hide", "_mm_mask_cvtepi64_storeu_epi16",
			"-ccgo-hide", "_mm_mask_cvtepi64_storeu_epi32",
			"-ccgo-hide", "_mm_mask_cvtepi64_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvtepi8_epi16",
			"-ccgo-hide", "_mm_mask_cvtepi8_epi32",
			"-ccgo-hide", "_mm_mask_cvtepi8_epi64",
			"-ccgo-hide", "_mm_mask_cvtepu16_epi32",
			"-ccgo-hide", "_mm_mask_cvtepu16_epi64",
			"-ccgo-hide", "_mm_mask_cvtepu32_epi64",
			"-ccgo-hide", "_mm_mask_cvtepu32_pd",
			"-ccgo-hide", "_mm_mask_cvtepu32_ps",
			"-ccgo-hide", "_mm_mask_cvtepu64_pd",
			"-ccgo-hide", "_mm_mask_cvtepu64_ps",
			"-ccgo-hide", "_mm_mask_cvtepu8_epi16",
			"-ccgo-hide", "_mm_mask_cvtepu8_epi32",
			"-ccgo-hide", "_mm_mask_cvtepu8_epi64",
			"-ccgo-hide", "_mm_mask_cvtpd_epi32",
			"-ccgo-hide", "_mm_mask_cvtpd_epi64",
			"-ccgo-hide", "_mm_mask_cvtpd_epu32",
			"-ccgo-hide", "_mm_mask_cvtpd_epu64",
			"-ccgo-hide", "_mm_mask_cvtpd_ps",
			"-ccgo-hide", "_mm_mask_cvtph_ps",
			"-ccgo-hide", "_mm_mask_cvtps_epi32",
			"-ccgo-hide", "_mm_mask_cvtps_epi64",
			"-ccgo-hide", "_mm_mask_cvtps_epu32",
			"-ccgo-hide", "_mm_mask_cvtps_epu64",
			"-ccgo-hide", "_mm_mask_cvtps_pd",
			"-ccgo-hide", "_mm_mask_cvtsepi16_epi8",
			"-ccgo-hide", "_mm_mask_cvtsepi16_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvtsepi32_epi16",
			"-ccgo-hide", "_mm_mask_cvtsepi32_epi8",
			"-ccgo-hide", "_mm_mask_cvtsepi32_storeu_epi16",
			"-ccgo-hide", "_mm_mask_cvtsepi32_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvtsepi64_epi16",
			"-ccgo-hide", "_mm_mask_cvtsepi64_epi32",
			"-ccgo-hide", "_mm_mask_cvtsepi64_epi8",
			"-ccgo-hide", "_mm_mask_cvtsepi64_storeu_epi16",
			"-ccgo-hide", "_mm_mask_cvtsepi64_storeu_epi32",
			"-ccgo-hide", "_mm_mask_cvtsepi64_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvttpd_epi32",
			"-ccgo-hide", "_mm_mask_cvttpd_epi64",
			"-ccgo-hide", "_mm_mask_cvttpd_epu32",
			"-ccgo-hide", "_mm_mask_cvttpd_epu64",
			"-ccgo-hide", "_mm_mask_cvttps_epi32",
			"-ccgo-hide", "_mm_mask_cvttps_epi64",
			"-ccgo-hide", "_mm_mask_cvttps_epu32",
			"-ccgo-hide", "_mm_mask_cvttps_epu64",
			"-ccgo-hide", "_mm_mask_cvtusepi16_epi8",
			"-ccgo-hide", "_mm_mask_cvtusepi16_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvtusepi32_epi16",
			"-ccgo-hide", "_mm_mask_cvtusepi32_epi8",
			"-ccgo-hide", "_mm_mask_cvtusepi32_storeu_epi16",
			"-ccgo-hide", "_mm_mask_cvtusepi32_storeu_epi8",
			"-ccgo-hide", "_mm_mask_cvtusepi64_epi16",
			"-ccgo-hide", "_mm_mask_cvtusepi64_epi32",
			"-ccgo-hide", "_mm_mask_cvtusepi64_epi8",
			"-ccgo-hide", "_mm_mask_cvtusepi64_storeu_epi16",
			"-ccgo-hide", "_mm_mask_cvtusepi64_storeu_epi32",
			"-ccgo-hide", "_mm_mask_cvtusepi64_storeu_epi8",
			"-ccgo-hide", "_mm_mask_div_pd",
			"-ccgo-hide", "_mm_mask_div_ps",
			"-ccgo-hide", "_mm_mask_div_sd",
			"-ccgo-hide", "_mm_mask_div_ss",
			"-ccgo-hide", "_mm_mask_dpbusd_epi32",
			"-ccgo-hide", "_mm_mask_dpbusds_epi32",
			"-ccgo-hide", "_mm_mask_dpwssd_epi32",
			"-ccgo-hide", "_mm_mask_dpwssds_epi32",
			"-ccgo-hide", "_mm_mask_expand_epi16",
			"-ccgo-hide", "_mm_mask_expand_epi32",
			"-ccgo-hide", "_mm_mask_expand_epi64",
			"-ccgo-hide", "_mm_mask_expand_epi8",
			"-ccgo-hide", "_mm_mask_expand_pd",
			"-ccgo-hide", "_mm_mask_expand_ps",
			"-ccgo-hide", "_mm_mask_expandloadu_epi16",
			"-ccgo-hide", "_mm_mask_expandloadu_epi32",
			"-ccgo-hide", "_mm_mask_expandloadu_epi64",
			"-ccgo-hide", "_mm_mask_expandloadu_epi8",
			"-ccgo-hide", "_mm_mask_expandloadu_pd",
			"-ccgo-hide", "_mm_mask_expandloadu_ps",
			"-ccgo-hide", "_mm_mask_fmadd_pd",
			"-ccgo-hide", "_mm_mask_fmadd_ps",
			"-ccgo-hide", "_mm_mask_fmaddsub_pd",
			"-ccgo-hide", "_mm_mask_fmaddsub_ps",
			"-ccgo-hide", "_mm_mask_fmsub_pd",
			"-ccgo-hide", "_mm_mask_fmsub_ps",
			"-ccgo-hide", "_mm_mask_fmsubadd_pd",
			"-ccgo-hide", "_mm_mask_fmsubadd_ps",
			"-ccgo-hide", "_mm_mask_fnmadd_pd",
			"-ccgo-hide", "_mm_mask_fnmadd_ps",
			"-ccgo-hide", "_mm_mask_fnmsub_pd",
			"-ccgo-hide", "_mm_mask_fnmsub_ps",
			"-ccgo-hide", "_mm_mask_getexp_pd",
			"-ccgo-hide", "_mm_mask_getexp_ps",
			"-ccgo-hide", "_mm_mask_gf2p8mul_epi8",
			"-ccgo-hide", "_mm_mask_load_epi32",
			"-ccgo-hide", "_mm_mask_load_epi64",
			"-ccgo-hide", "_mm_mask_load_pd",
			"-ccgo-hide", "_mm_mask_load_ps",
			"-ccgo-hide", "_mm_mask_loadu_epi16",
			"-ccgo-hide", "_mm_mask_loadu_epi32",
			"-ccgo-hide", "_mm_mask_loadu_epi64",
			"-ccgo-hide", "_mm_mask_loadu_epi8",
			"-ccgo-hide", "_mm_mask_loadu_pd",
			"-ccgo-hide", "_mm_mask_loadu_ps",
			"-ccgo-hide", "_mm_mask_lzcnt_epi32",
			"-ccgo-hide", "_mm_mask_lzcnt_epi64",
			"-ccgo-hide", "_mm_mask_madd52hi_epu64",
			"-ccgo-hide", "_mm_mask_madd52lo_epu64",
			"-ccgo-hide", "_mm_mask_madd_epi16",
			"-ccgo-hide", "_mm_mask_maddubs_epi16",
			"-ccgo-hide", "_mm_mask_max_epi16",
			"-ccgo-hide", "_mm_mask_max_epi32",
			"-ccgo-hide", "_mm_mask_max_epi64",
			"-ccgo-hide", "_mm_mask_max_epi8",
			"-ccgo-hide", "_mm_mask_max_epu16",
			"-ccgo-hide", "_mm_mask_max_epu32",
			"-ccgo-hide", "_mm_mask_max_epu64",
			"-ccgo-hide", "_mm_mask_max_epu8",
			"-ccgo-hide", "_mm_mask_max_pd",
			"-ccgo-hide", "_mm_mask_max_ps",
			"-ccgo-hide", "_mm_mask_max_sd",
			"-ccgo-hide", "_mm_mask_max_ss",
			"-ccgo-hide", "_mm_mask_min_epi16",
			"-ccgo-hide", "_mm_mask_min_epi32",
			"-ccgo-hide", "_mm_mask_min_epi64",
			"-ccgo-hide", "_mm_mask_min_epi8",
			"-ccgo-hide", "_mm_mask_min_epu16",
			"-ccgo-hide", "_mm_mask_min_epu32",
			"-ccgo-hide", "_mm_mask_min_epu64",
			"-ccgo-hide", "_mm_mask_min_epu8",
			"-ccgo-hide", "_mm_mask_min_pd",
			"-ccgo-hide", "_mm_mask_min_ps",
			"-ccgo-hide", "_mm_mask_min_sd",
			"-ccgo-hide", "_mm_mask_min_ss",
			"-ccgo-hide", "_mm_mask_mov_epi16",
			"-ccgo-hide", "_mm_mask_mov_epi32",
			"-ccgo-hide", "_mm_mask_mov_epi64",
			"-ccgo-hide", "_mm_mask_mov_epi8",
			"-ccgo-hide", "_mm_mask_mov_pd",
			"-ccgo-hide", "_mm_mask_mov_ps",
			"-ccgo-hide", "_mm_mask_movedup_pd",
			"-ccgo-hide", "_mm_mask_movehdup_ps",
			"-ccgo-hide", "_mm_mask_moveldup_ps",
			"-ccgo-hide", "_mm_mask_mul_epi32",
			"-ccgo-hide", "_mm_mask_mul_epu32",
			"-ccgo-hide", "_mm_mask_mul_pd",
			"-ccgo-hide", "_mm_mask_mul_ps",
			"-ccgo-hide", "_mm_mask_mul_sd",
			"-ccgo-hide", "_mm_mask_mul_ss",
			"-ccgo-hide", "_mm_mask_mulhi_epi16",
			"-ccgo-hide", "_mm_mask_mulhi_epu16",
			"-ccgo-hide", "_mm_mask_mulhrs_epi16",
			"-ccgo-hide", "_mm_mask_mullo_epi16",
			"-ccgo-hide", "_mm_mask_mullo_epi32",
			"-ccgo-hide", "_mm_mask_mullo_epi64",
			"-ccgo-hide", "_mm_mask_multishift_epi64_epi8",
			"-ccgo-hide", "_mm_mask_or_epi32",
			"-ccgo-hide", "_mm_mask_or_epi64",
			"-ccgo-hide", "_mm_mask_or_pd",
			"-ccgo-hide", "_mm_mask_or_ps",
			"-ccgo-hide", "_mm_mask_packs_epi16",
			"-ccgo-hide", "_mm_mask_packs_epi32",
			"-ccgo-hide", "_mm_mask_packus_epi16",
			"-ccgo-hide", "_mm_mask_packus_epi32",
			"-ccgo-hide", "_mm_mask_permutevar_pd",
			"-ccgo-hide", "_mm_mask_permutevar_ps",
			"-ccgo-hide", "_mm_mask_permutex2var_epi16",
			"-ccgo-hide", "_mm_mask_permutex2var_epi32",
			"-ccgo-hide", "_mm_mask_permutex2var_epi64",
			"-ccgo-hide", "_mm_mask_permutex2var_epi8",
			"-ccgo-hide", "_mm_mask_permutex2var_pd",
			"-ccgo-hide", "_mm_mask_permutex2var_ps",
			"-ccgo-hide", "_mm_mask_permutexvar_epi16",
			"-ccgo-hide", "_mm_mask_permutexvar_epi8",
			"-ccgo-hide", "_mm_mask_popcnt_epi16",
			"-ccgo-hide", "_mm_mask_popcnt_epi32",
			"-ccgo-hide", "_mm_mask_popcnt_epi64",
			"-ccgo-hide", "_mm_mask_popcnt_epi8",
			"-ccgo-hide", "_mm_mask_rcp14_pd",
			"-ccgo-hide", "_mm_mask_rcp14_ps",
			"-ccgo-hide", "_mm_mask_rcp14_sd",
			"-ccgo-hide", "_mm_mask_rcp14_ss",
			"-ccgo-hide", "_mm_mask_rolv_epi32",
			"-ccgo-hide", "_mm_mask_rolv_epi64",
			"-ccgo-hide", "_mm_mask_rorv_epi32",
			"-ccgo-hide", "_mm_mask_rorv_epi64",
			"-ccgo-hide", "_mm_mask_rsqrt14_pd",
			"-ccgo-hide", "_mm_mask_rsqrt14_ps",
			"-ccgo-hide", "_mm_mask_rsqrt14_sd",
			"-ccgo-hide", "_mm_mask_rsqrt14_ss",
			"-ccgo-hide", "_mm_mask_scalef_pd",
			"-ccgo-hide", "_mm_mask_scalef_ps",
			"-ccgo-hide", "_mm_mask_set1_epi16",
			"-ccgo-hide", "_mm_mask_set1_epi32",
			"-ccgo-hide", "_mm_mask_set1_epi64",
			"-ccgo-hide", "_mm_mask_set1_epi8",
			"-ccgo-hide", "_mm_mask_shldv_epi16",
			"-ccgo-hide", "_mm_mask_shldv_epi32",
			"-ccgo-hide", "_mm_mask_shldv_epi64",
			"-ccgo-hide", "_mm_mask_shrdv_epi16",
			"-ccgo-hide", "_mm_mask_shrdv_epi32",
			"-ccgo-hide", "_mm_mask_shrdv_epi64",
			"-ccgo-hide", "_mm_mask_shuffle_epi8",
			"-ccgo-hide", "_mm_mask_sll_epi16",
			"-ccgo-hide", "_mm_mask_sll_epi32",
			"-ccgo-hide", "_mm_mask_sll_epi64",
			"-ccgo-hide", "_mm_mask_sllv_epi16",
			"-ccgo-hide", "_mm_mask_sllv_epi32",
			"-ccgo-hide", "_mm_mask_sllv_epi64",
			"-ccgo-hide", "_mm_mask_sqrt_pd",
			"-ccgo-hide", "_mm_mask_sqrt_ps",
			"-ccgo-hide", "_mm_mask_sra_epi16",
			"-ccgo-hide", "_mm_mask_sra_epi32",
			"-ccgo-hide", "_mm_mask_sra_epi64",
			"-ccgo-hide", "_mm_mask_srav_epi16",
			"-ccgo-hide", "_mm_mask_srav_epi32",
			"-ccgo-hide", "_mm_mask_srav_epi64",
			"-ccgo-hide", "_mm_mask_srl_epi16",
			"-ccgo-hide", "_mm_mask_srl_epi32",
			"-ccgo-hide", "_mm_mask_srl_epi64",
			"-ccgo-hide", "_mm_mask_srlv_epi16",
			"-ccgo-hide", "_mm_mask_srlv_epi32",
			"-ccgo-hide", "_mm_mask_srlv_epi64",
			"-ccgo-hide", "_mm_mask_store_epi32",
			"-ccgo-hide", "_mm_mask_store_epi64",
			"-ccgo-hide", "_mm_mask_store_pd",
			"-ccgo-hide", "_mm_mask_store_ps",
			"-ccgo-hide", "_mm_mask_storeu_epi16",
			"-ccgo-hide", "_mm_mask_storeu_epi32",
			"-ccgo-hide", "_mm_mask_storeu_epi64",
			"-ccgo-hide", "_mm_mask_storeu_epi8",
			"-ccgo-hide", "_mm_mask_storeu_pd",
			"-ccgo-hide", "_mm_mask_storeu_ps",
			"-ccgo-hide", "_mm_mask_sub_epi16",
			"-ccgo-hide", "_mm_mask_sub_epi32",
			"-ccgo-hide", "_mm_mask_sub_epi64",
			"-ccgo-hide", "_mm_mask_sub_epi8",
			"-ccgo-hide", "_mm_mask_sub_pd",
			"-ccgo-hide", "_mm_mask_sub_ps",
			"-ccgo-hide", "_mm_mask_sub_sd",
			"-ccgo-hide", "_mm_mask_sub_ss",
			"-ccgo-hide", "_mm_mask_subs_epi16",
			"-ccgo-hide", "_mm_mask_subs_epi8",
			"-ccgo-hide", "_mm_mask_subs_epu16",
			"-ccgo-hide", "_mm_mask_subs_epu8",
			"-ccgo-hide", "_mm_mask_test_epi16_mask",
			"-ccgo-hide", "_mm_mask_test_epi32_mask",
			"-ccgo-hide", "_mm_mask_test_epi64_mask",
			"-ccgo-hide", "_mm_mask_test_epi8_mask",
			"-ccgo-hide", "_mm_mask_testn_epi16_mask",
			"-ccgo-hide", "_mm_mask_testn_epi32_mask",
			"-ccgo-hide", "_mm_mask_testn_epi64_mask",
			"-ccgo-hide", "_mm_mask_testn_epi8_mask",
			"-ccgo-hide", "_mm_mask_unpackhi_epi16",
			"-ccgo-hide", "_mm_mask_unpackhi_epi32",
			"-ccgo-hide", "_mm_mask_unpackhi_epi64",
			"-ccgo-hide", "_mm_mask_unpackhi_epi8",
			"-ccgo-hide", "_mm_mask_unpackhi_pd",
			"-ccgo-hide", "_mm_mask_unpackhi_ps",
			"-ccgo-hide", "_mm_mask_unpacklo_epi16",
			"-ccgo-hide", "_mm_mask_unpacklo_epi32",
			"-ccgo-hide", "_mm_mask_unpacklo_epi64",
			"-ccgo-hide", "_mm_mask_unpacklo_epi8",
			"-ccgo-hide", "_mm_mask_unpacklo_pd",
			"-ccgo-hide", "_mm_mask_unpacklo_ps",
			"-ccgo-hide", "_mm_mask_xor_epi32",
			"-ccgo-hide", "_mm_mask_xor_epi64",
			"-ccgo-hide", "_mm_mask_xor_pd",
			"-ccgo-hide", "_mm_mask_xor_ps",
			"-ccgo-hide", "_mm_maskload_epi32",
			"-ccgo-hide", "_mm_maskload_epi64",
			"-ccgo-hide", "_mm_maskload_pd",
			"-ccgo-hide", "_mm_maskload_ps",
			"-ccgo-hide", "_mm_maskmove_si64",
			"-ccgo-hide", "_mm_maskmoveu_si128",
			"-ccgo-hide", "_mm_maskstore_epi32",
			"-ccgo-hide", "_mm_maskstore_epi64",
			"-ccgo-hide", "_mm_maskstore_pd",
			"-ccgo-hide", "_mm_maskstore_ps",
			"-ccgo-hide", "_mm_maskz_4fmadd_ss",
			"-ccgo-hide", "_mm_maskz_4fnmadd_ss",
			"-ccgo-hide", "_mm_maskz_abs_epi16",
			"-ccgo-hide", "_mm_maskz_abs_epi32",
			"-ccgo-hide", "_mm_maskz_abs_epi64",
			"-ccgo-hide", "_mm_maskz_abs_epi8",
			"-ccgo-hide", "_mm_maskz_add_epi16",
			"-ccgo-hide", "_mm_maskz_add_epi32",
			"-ccgo-hide", "_mm_maskz_add_epi64",
			"-ccgo-hide", "_mm_maskz_add_epi8",
			"-ccgo-hide", "_mm_maskz_add_pd",
			"-ccgo-hide", "_mm_maskz_add_ps",
			"-ccgo-hide", "_mm_maskz_add_sd",
			"-ccgo-hide", "_mm_maskz_add_ss",
			"-ccgo-hide", "_mm_maskz_adds_epi16",
			"-ccgo-hide", "_mm_maskz_adds_epi8",
			"-ccgo-hide", "_mm_maskz_adds_epu16",
			"-ccgo-hide", "_mm_maskz_adds_epu8",
			"-ccgo-hide", "_mm_maskz_and_epi32",
			"-ccgo-hide", "_mm_maskz_and_epi64",
			"-ccgo-hide", "_mm_maskz_and_pd",
			"-ccgo-hide", "_mm_maskz_and_ps",
			"-ccgo-hide", "_mm_maskz_andnot_epi32",
			"-ccgo-hide", "_mm_maskz_andnot_epi64",
			"-ccgo-hide", "_mm_maskz_andnot_pd",
			"-ccgo-hide", "_mm_maskz_andnot_ps",
			"-ccgo-hide", "_mm_maskz_avg_epu16",
			"-ccgo-hide", "_mm_maskz_avg_epu8",
			"-ccgo-hide", "_mm_maskz_broadcast_i32x2",
			"-ccgo-hide", "_mm_maskz_broadcastb_epi8",
			"-ccgo-hide", "_mm_maskz_broadcastd_epi32",
			"-ccgo-hide", "_mm_maskz_broadcastq_epi64",
			"-ccgo-hide", "_mm_maskz_broadcastss_ps",
			"-ccgo-hide", "_mm_maskz_broadcastw_epi16",
			"-ccgo-hide", "_mm_maskz_compress_epi16",
			"-ccgo-hide", "_mm_maskz_compress_epi32",
			"-ccgo-hide", "_mm_maskz_compress_epi64",
			"-ccgo-hide", "_mm_maskz_compress_epi8",
			"-ccgo-hide", "_mm_maskz_compress_pd",
			"-ccgo-hide", "_mm_maskz_compress_ps",
			"-ccgo-hide", "_mm_maskz_conflict_epi32",
			"-ccgo-hide", "_mm_maskz_conflict_epi64",
			"-ccgo-hide", "_mm_maskz_cvtepi16_epi32",
			"-ccgo-hide", "_mm_maskz_cvtepi16_epi64",
			"-ccgo-hide", "_mm_maskz_cvtepi16_epi8",
			"-ccgo-hide", "_mm_maskz_cvtepi32_epi16",
			"-ccgo-hide", "_mm_maskz_cvtepi32_epi64",
			"-ccgo-hide", "_mm_maskz_cvtepi32_epi8",
			"-ccgo-hide", "_mm_maskz_cvtepi32_pd",
			"-ccgo-hide", "_mm_maskz_cvtepi32_ps",
			"-ccgo-hide", "_mm_maskz_cvtepi64_epi16",
			"-ccgo-hide", "_mm_maskz_cvtepi64_epi32",
			"-ccgo-hide", "_mm_maskz_cvtepi64_epi8",
			"-ccgo-hide", "_mm_maskz_cvtepi64_pd",
			"-ccgo-hide", "_mm_maskz_cvtepi64_ps",
			"-ccgo-hide", "_mm_maskz_cvtepi8_epi16",
			"-ccgo-hide", "_mm_maskz_cvtepi8_epi32",
			"-ccgo-hide", "_mm_maskz_cvtepi8_epi64",
			"-ccgo-hide", "_mm_maskz_cvtepu16_epi32",
			"-ccgo-hide", "_mm_maskz_cvtepu16_epi64",
			"-ccgo-hide", "_mm_maskz_cvtepu32_epi64",
			"-ccgo-hide", "_mm_maskz_cvtepu32_pd",
			"-ccgo-hide", "_mm_maskz_cvtepu32_ps",
			"-ccgo-hide", "_mm_maskz_cvtepu64_pd",
			"-ccgo-hide", "_mm_maskz_cvtepu64_ps",
			"-ccgo-hide", "_mm_maskz_cvtepu8_epi16",
			"-ccgo-hide", "_mm_maskz_cvtepu8_epi32",
			"-ccgo-hide", "_mm_maskz_cvtepu8_epi64",
			"-ccgo-hide", "_mm_maskz_cvtpd_epi32",
			"-ccgo-hide", "_mm_maskz_cvtpd_epi64",
			"-ccgo-hide", "_mm_maskz_cvtpd_epu32",
			"-ccgo-hide", "_mm_maskz_cvtpd_epu64",
			"-ccgo-hide", "_mm_maskz_cvtpd_ps",
			"-ccgo-hide", "_mm_maskz_cvtph_ps",
			"-ccgo-hide", "_mm_maskz_cvtps_epi32",
			"-ccgo-hide", "_mm_maskz_cvtps_epi64",
			"-ccgo-hide", "_mm_maskz_cvtps_epu32",
			"-ccgo-hide", "_mm_maskz_cvtps_epu64",
			"-ccgo-hide", "_mm_maskz_cvtps_pd",
			"-ccgo-hide", "_mm_maskz_cvtsepi16_epi8",
			"-ccgo-hide", "_mm_maskz_cvtsepi32_epi16",
			"-ccgo-hide", "_mm_maskz_cvtsepi32_epi8",
			"-ccgo-hide", "_mm_maskz_cvtsepi64_epi16",
			"-ccgo-hide", "_mm_maskz_cvtsepi64_epi32",
			"-ccgo-hide", "_mm_maskz_cvtsepi64_epi8",
			"-ccgo-hide", "_mm_maskz_cvttpd_epi32",
			"-ccgo-hide", "_mm_maskz_cvttpd_epi64",
			"-ccgo-hide", "_mm_maskz_cvttpd_epu32",
			"-ccgo-hide", "_mm_maskz_cvttpd_epu64",
			"-ccgo-hide", "_mm_maskz_cvttps_epi32",
			"-ccgo-hide", "_mm_maskz_cvttps_epi64",
			"-ccgo-hide", "_mm_maskz_cvttps_epu32",
			"-ccgo-hide", "_mm_maskz_cvttps_epu64",
			"-ccgo-hide", "_mm_maskz_cvtusepi16_epi8",
			"-ccgo-hide", "_mm_maskz_cvtusepi32_epi16",
			"-ccgo-hide", "_mm_maskz_cvtusepi32_epi8",
			"-ccgo-hide", "_mm_maskz_cvtusepi64_epi16",
			"-ccgo-hide", "_mm_maskz_cvtusepi64_epi32",
			"-ccgo-hide", "_mm_maskz_cvtusepi64_epi8",
			"-ccgo-hide", "_mm_maskz_div_pd",
			"-ccgo-hide", "_mm_maskz_div_ps",
			"-ccgo-hide", "_mm_maskz_div_sd",
			"-ccgo-hide", "_mm_maskz_div_ss",
			"-ccgo-hide", "_mm_maskz_dpbusd_epi32",
			"-ccgo-hide", "_mm_maskz_dpbusds_epi32",
			"-ccgo-hide", "_mm_maskz_dpwssd_epi32",
			"-ccgo-hide", "_mm_maskz_dpwssds_epi32",
			"-ccgo-hide", "_mm_maskz_expand_epi16",
			"-ccgo-hide", "_mm_maskz_expand_epi32",
			"-ccgo-hide", "_mm_maskz_expand_epi64",
			"-ccgo-hide", "_mm_maskz_expand_epi8",
			"-ccgo-hide", "_mm_maskz_expand_pd",
			"-ccgo-hide", "_mm_maskz_expand_ps",
			"-ccgo-hide", "_mm_maskz_expandloadu_epi16",
			"-ccgo-hide", "_mm_maskz_expandloadu_epi32",
			"-ccgo-hide", "_mm_maskz_expandloadu_epi64",
			"-ccgo-hide", "_mm_maskz_expandloadu_epi8",
			"-ccgo-hide", "_mm_maskz_expandloadu_pd",
			"-ccgo-hide", "_mm_maskz_expandloadu_ps",
			"-ccgo-hide", "_mm_maskz_fmadd_pd",
			"-ccgo-hide", "_mm_maskz_fmadd_ps",
			"-ccgo-hide", "_mm_maskz_fmaddsub_pd",
			"-ccgo-hide", "_mm_maskz_fmaddsub_ps",
			"-ccgo-hide", "_mm_maskz_fmsub_pd",
			"-ccgo-hide", "_mm_maskz_fmsub_ps",
			"-ccgo-hide", "_mm_maskz_fmsubadd_pd",
			"-ccgo-hide", "_mm_maskz_fmsubadd_ps",
			"-ccgo-hide", "_mm_maskz_fnmadd_pd",
			"-ccgo-hide", "_mm_maskz_fnmadd_ps",
			"-ccgo-hide", "_mm_maskz_fnmsub_pd",
			"-ccgo-hide", "_mm_maskz_fnmsub_ps",
			"-ccgo-hide", "_mm_maskz_getexp_pd",
			"-ccgo-hide", "_mm_maskz_getexp_ps",
			"-ccgo-hide", "_mm_maskz_gf2p8mul_epi8",
			"-ccgo-hide", "_mm_maskz_load_epi32",
			"-ccgo-hide", "_mm_maskz_load_epi64",
			"-ccgo-hide", "_mm_maskz_load_pd",
			"-ccgo-hide", "_mm_maskz_load_ps",
			"-ccgo-hide", "_mm_maskz_loadu_epi16",
			"-ccgo-hide", "_mm_maskz_loadu_epi32",
			"-ccgo-hide", "_mm_maskz_loadu_epi64",
			"-ccgo-hide", "_mm_maskz_loadu_epi8",
			"-ccgo-hide", "_mm_maskz_loadu_pd",
			"-ccgo-hide", "_mm_maskz_loadu_ps",
			"-ccgo-hide", "_mm_maskz_lzcnt_epi32",
			"-ccgo-hide", "_mm_maskz_lzcnt_epi64",
			"-ccgo-hide", "_mm_maskz_madd52hi_epu64",
			"-ccgo-hide", "_mm_maskz_madd52lo_epu64",
			"-ccgo-hide", "_mm_maskz_madd_epi16",
			"-ccgo-hide", "_mm_maskz_maddubs_epi16",
			"-ccgo-hide", "_mm_maskz_max_epi16",
			"-ccgo-hide", "_mm_maskz_max_epi32",
			"-ccgo-hide", "_mm_maskz_max_epi64",
			"-ccgo-hide", "_mm_maskz_max_epi8",
			"-ccgo-hide", "_mm_maskz_max_epu16",
			"-ccgo-hide", "_mm_maskz_max_epu32",
			"-ccgo-hide", "_mm_maskz_max_epu64",
			"-ccgo-hide", "_mm_maskz_max_epu8",
			"-ccgo-hide", "_mm_maskz_max_pd",
			"-ccgo-hide", "_mm_maskz_max_ps",
			"-ccgo-hide", "_mm_maskz_max_sd",
			"-ccgo-hide", "_mm_maskz_max_ss",
			"-ccgo-hide", "_mm_maskz_min_epi16",
			"-ccgo-hide", "_mm_maskz_min_epi32",
			"-ccgo-hide", "_mm_maskz_min_epi64",
			"-ccgo-hide", "_mm_maskz_min_epi8",
			"-ccgo-hide", "_mm_maskz_min_epu16",
			"-ccgo-hide", "_mm_maskz_min_epu32",
			"-ccgo-hide", "_mm_maskz_min_epu64",
			"-ccgo-hide", "_mm_maskz_min_epu8",
			"-ccgo-hide", "_mm_maskz_min_pd",
			"-ccgo-hide", "_mm_maskz_min_ps",
			"-ccgo-hide", "_mm_maskz_min_sd",
			"-ccgo-hide", "_mm_maskz_min_ss",
			"-ccgo-hide", "_mm_maskz_mov_epi16",
			"-ccgo-hide", "_mm_maskz_mov_epi32",
			"-ccgo-hide", "_mm_maskz_mov_epi64",
			"-ccgo-hide", "_mm_maskz_mov_epi8",
			"-ccgo-hide", "_mm_maskz_mov_pd",
			"-ccgo-hide", "_mm_maskz_mov_ps",
			"-ccgo-hide", "_mm_maskz_movedup_pd",
			"-ccgo-hide", "_mm_maskz_movehdup_ps",
			"-ccgo-hide", "_mm_maskz_moveldup_ps",
			"-ccgo-hide", "_mm_maskz_mul_epi32",
			"-ccgo-hide", "_mm_maskz_mul_epu32",
			"-ccgo-hide", "_mm_maskz_mul_pd",
			"-ccgo-hide", "_mm_maskz_mul_ps",
			"-ccgo-hide", "_mm_maskz_mul_sd",
			"-ccgo-hide", "_mm_maskz_mul_ss",
			"-ccgo-hide", "_mm_maskz_mulhi_epi16",
			"-ccgo-hide", "_mm_maskz_mulhi_epu16",
			"-ccgo-hide", "_mm_maskz_mulhrs_epi16",
			"-ccgo-hide", "_mm_maskz_mullo_epi16",
			"-ccgo-hide", "_mm_maskz_mullo_epi32",
			"-ccgo-hide", "_mm_maskz_mullo_epi64",
			"-ccgo-hide", "_mm_maskz_multishift_epi64_epi8",
			"-ccgo-hide", "_mm_maskz_or_epi32",
			"-ccgo-hide", "_mm_maskz_or_epi64",
			"-ccgo-hide", "_mm_maskz_or_pd",
			"-ccgo-hide", "_mm_maskz_or_ps",
			"-ccgo-hide", "_mm_maskz_packs_epi16",
			"-ccgo-hide", "_mm_maskz_packs_epi32",
			"-ccgo-hide", "_mm_maskz_packus_epi16",
			"-ccgo-hide", "_mm_maskz_packus_epi32",
			"-ccgo-hide", "_mm_maskz_permutevar_pd",
			"-ccgo-hide", "_mm_maskz_permutevar_ps",
			"-ccgo-hide", "_mm_maskz_permutex2var_epi16",
			"-ccgo-hide", "_mm_maskz_permutex2var_epi32",
			"-ccgo-hide", "_mm_maskz_permutex2var_epi64",
			"-ccgo-hide", "_mm_maskz_permutex2var_epi8",
			"-ccgo-hide", "_mm_maskz_permutex2var_pd",
			"-ccgo-hide", "_mm_maskz_permutex2var_ps",
			"-ccgo-hide", "_mm_maskz_permutexvar_epi16",
			"-ccgo-hide", "_mm_maskz_permutexvar_epi8",
			"-ccgo-hide", "_mm_maskz_popcnt_epi16",
			"-ccgo-hide", "_mm_maskz_popcnt_epi32",
			"-ccgo-hide", "_mm_maskz_popcnt_epi64",
			"-ccgo-hide", "_mm_maskz_popcnt_epi8",
			"-ccgo-hide", "_mm_maskz_rcp14_pd",
			"-ccgo-hide", "_mm_maskz_rcp14_ps",
			"-ccgo-hide", "_mm_maskz_rcp14_sd",
			"-ccgo-hide", "_mm_maskz_rcp14_ss",
			"-ccgo-hide", "_mm_maskz_rolv_epi32",
			"-ccgo-hide", "_mm_maskz_rolv_epi64",
			"-ccgo-hide", "_mm_maskz_rorv_epi32",
			"-ccgo-hide", "_mm_maskz_rorv_epi64",
			"-ccgo-hide", "_mm_maskz_rsqrt14_pd",
			"-ccgo-hide", "_mm_maskz_rsqrt14_ps",
			"-ccgo-hide", "_mm_maskz_rsqrt14_sd",
			"-ccgo-hide", "_mm_maskz_rsqrt14_ss",
			"-ccgo-hide", "_mm_maskz_scalef_pd",
			"-ccgo-hide", "_mm_maskz_scalef_ps",
			"-ccgo-hide", "_mm_maskz_set1_epi16",
			"-ccgo-hide", "_mm_maskz_set1_epi32",
			"-ccgo-hide", "_mm_maskz_set1_epi64",
			"-ccgo-hide", "_mm_maskz_set1_epi8",
			"-ccgo-hide", "_mm_maskz_shldv_epi16",
			"-ccgo-hide", "_mm_maskz_shldv_epi32",
			"-ccgo-hide", "_mm_maskz_shldv_epi64",
			"-ccgo-hide", "_mm_maskz_shrdv_epi16",
			"-ccgo-hide", "_mm_maskz_shrdv_epi32",
			"-ccgo-hide", "_mm_maskz_shrdv_epi64",
			"-ccgo-hide", "_mm_maskz_shuffle_epi8",
			"-ccgo-hide", "_mm_maskz_sll_epi16",
			"-ccgo-hide", "_mm_maskz_sll_epi32",
			"-ccgo-hide", "_mm_maskz_sll_epi64",
			"-ccgo-hide", "_mm_maskz_sllv_epi16",
			"-ccgo-hide", "_mm_maskz_sllv_epi32",
			"-ccgo-hide", "_mm_maskz_sllv_epi64",
			"-ccgo-hide", "_mm_maskz_sqrt_pd",
			"-ccgo-hide", "_mm_maskz_sqrt_ps",
			"-ccgo-hide", "_mm_maskz_sra_epi16",
			"-ccgo-hide", "_mm_maskz_sra_epi32",
			"-ccgo-hide", "_mm_maskz_sra_epi64",
			"-ccgo-hide", "_mm_maskz_srav_epi16",
			"-ccgo-hide", "_mm_maskz_srav_epi32",
			"-ccgo-hide", "_mm_maskz_srav_epi64",
			"-ccgo-hide", "_mm_maskz_srl_epi16",
			"-ccgo-hide", "_mm_maskz_srl_epi32",
			"-ccgo-hide", "_mm_maskz_srl_epi64",
			"-ccgo-hide", "_mm_maskz_srlv_epi16",
			"-ccgo-hide", "_mm_maskz_srlv_epi32",
			"-ccgo-hide", "_mm_maskz_srlv_epi64",
			"-ccgo-hide", "_mm_maskz_sub_epi16",
			"-ccgo-hide", "_mm_maskz_sub_epi32",
			"-ccgo-hide", "_mm_maskz_sub_epi64",
			"-ccgo-hide", "_mm_maskz_sub_epi8",
			"-ccgo-hide", "_mm_maskz_sub_pd",
			"-ccgo-hide", "_mm_maskz_sub_ps",
			"-ccgo-hide", "_mm_maskz_sub_sd",
			"-ccgo-hide", "_mm_maskz_sub_ss",
			"-ccgo-hide", "_mm_maskz_subs_epi16",
			"-ccgo-hide", "_mm_maskz_subs_epi8",
			"-ccgo-hide", "_mm_maskz_subs_epu16",
			"-ccgo-hide", "_mm_maskz_subs_epu8",
			"-ccgo-hide", "_mm_maskz_unpackhi_epi16",
			"-ccgo-hide", "_mm_maskz_unpackhi_epi32",
			"-ccgo-hide", "_mm_maskz_unpackhi_epi64",
			"-ccgo-hide", "_mm_maskz_unpackhi_epi8",
			"-ccgo-hide", "_mm_maskz_unpackhi_pd",
			"-ccgo-hide", "_mm_maskz_unpackhi_ps",
			"-ccgo-hide", "_mm_maskz_unpacklo_epi16",
			"-ccgo-hide", "_mm_maskz_unpacklo_epi32",
			"-ccgo-hide", "_mm_maskz_unpacklo_epi64",
			"-ccgo-hide", "_mm_maskz_unpacklo_epi8",
			"-ccgo-hide", "_mm_maskz_unpacklo_pd",
			"-ccgo-hide", "_mm_maskz_unpacklo_ps",
			"-ccgo-hide", "_mm_maskz_xor_epi32",
			"-ccgo-hide", "_mm_maskz_xor_epi64",
			"-ccgo-hide", "_mm_maskz_xor_pd",
			"-ccgo-hide", "_mm_maskz_xor_ps",
			"-ccgo-hide", "_mm_max_epi16",
			"-ccgo-hide", "_mm_max_epi32",
			"-ccgo-hide", "_mm_max_epi64",
			"-ccgo-hide", "_mm_max_epi8",
			"-ccgo-hide", "_mm_max_epu16",
			"-ccgo-hide", "_mm_max_epu32",
			"-ccgo-hide", "_mm_max_epu64",
			"-ccgo-hide", "_mm_max_epu8",
			"-ccgo-hide", "_mm_max_pd",
			"-ccgo-hide", "_mm_max_pi16",
			"-ccgo-hide", "_mm_max_ps",
			"-ccgo-hide", "_mm_max_pu8",
			"-ccgo-hide", "_mm_max_sd",
			"-ccgo-hide", "_mm_max_ss",
			"-ccgo-hide", "_mm_mfence",
			"-ccgo-hide", "_mm_min_epi16",
			"-ccgo-hide", "_mm_min_epi32",
			"-ccgo-hide", "_mm_min_epi64",
			"-ccgo-hide", "_mm_min_epi8",
			"-ccgo-hide", "_mm_min_epu16",
			"-ccgo-hide", "_mm_min_epu32",
			"-ccgo-hide", "_mm_min_epu64",
			"-ccgo-hide", "_mm_min_epu8",
			"-ccgo-hide", "_mm_min_pd",
			"-ccgo-hide", "_mm_min_pi16",
			"-ccgo-hide", "_mm_min_ps",
			"-ccgo-hide", "_mm_min_pu8",
			"-ccgo-hide", "_mm_min_sd",
			"-ccgo-hide", "_mm_min_ss",
			"-ccgo-hide", "_mm_minpos_epu16",
			"-ccgo-hide", "_mm_monitor",
			"-ccgo-hide", "_mm_monitorx",
			"-ccgo-hide", "_mm_move_epi64",
			"-ccgo-hide", "_mm_move_sd",
			"-ccgo-hide", "_mm_move_ss",
			"-ccgo-hide", "_mm_movedup_pd",
			"-ccgo-hide", "_mm_movehdup_ps",
			"-ccgo-hide", "_mm_movehl_ps",
			"-ccgo-hide", "_mm_moveldup_ps",
			"-ccgo-hide", "_mm_movelh_ps",
			"-ccgo-hide", "_mm_movemask_epi8",
			"-ccgo-hide", "_mm_movemask_pd",
			"-ccgo-hide", "_mm_movemask_pi8",
			"-ccgo-hide", "_mm_movemask_ps",
			"-ccgo-hide", "_mm_movepi16_mask",
			"-ccgo-hide", "_mm_movepi32_mask",
			"-ccgo-hide", "_mm_movepi64_mask",
			"-ccgo-hide", "_mm_movepi64_pi64",
			"-ccgo-hide", "_mm_movepi8_mask",
			"-ccgo-hide", "_mm_movm_epi16",
			"-ccgo-hide", "_mm_movm_epi32",
			"-ccgo-hide", "_mm_movm_epi64",
			"-ccgo-hide", "_mm_movm_epi8",
			"-ccgo-hide", "_mm_movpi64_epi64",
			"-ccgo-hide", "_mm_msub_pd",
			"-ccgo-hide", "_mm_msub_ps",
			"-ccgo-hide", "_mm_msub_sd",
			"-ccgo-hide", "_mm_msub_ss",
			"-ccgo-hide", "_mm_msubadd_pd",
			"-ccgo-hide", "_mm_msubadd_ps",
			"-ccgo-hide", "_mm_mul_epi32",
			"-ccgo-hide", "_mm_mul_epu32",
			"-ccgo-hide", "_mm_mul_pd",
			"-ccgo-hide", "_mm_mul_ps",
			"-ccgo-hide", "_mm_mul_sd",
			"-ccgo-hide", "_mm_mul_ss",
			"-ccgo-hide", "_mm_mul_su32",
			"-ccgo-hide", "_mm_mulhi_epi16",
			"-ccgo-hide", "_mm_mulhi_epu16",
			"-ccgo-hide", "_mm_mulhi_pi16",
			"-ccgo-hide", "_mm_mulhi_pu16",
			"-ccgo-hide", "_mm_mulhrs_epi16",
			"-ccgo-hide", "_mm_mulhrs_pi16",
			"-ccgo-hide", "_mm_mullo_epi16",
			"-ccgo-hide", "_mm_mullo_epi32",
			"-ccgo-hide", "_mm_mullo_epi64",
			"-ccgo-hide", "_mm_mullo_pi16",
			"-ccgo-hide", "_mm_multishift_epi64_epi8",
			"-ccgo-hide", "_mm_mwait",
			"-ccgo-hide", "_mm_mwaitx",
			"-ccgo-hide", "_mm_nmacc_pd",
			"-ccgo-hide", "_mm_nmacc_ps",
			"-ccgo-hide", "_mm_nmacc_sd",
			"-ccgo-hide", "_mm_nmacc_ss",
			"-ccgo-hide", "_mm_nmsub_pd",
			"-ccgo-hide", "_mm_nmsub_ps",
			"-ccgo-hide", "_mm_nmsub_sd",
			"-ccgo-hide", "_mm_nmsub_ss",
			"-ccgo-hide", "_mm_or_pd",
			"-ccgo-hide", "_mm_or_ps",
			"-ccgo-hide", "_mm_or_si128",
			"-ccgo-hide", "_mm_or_si64",
			"-ccgo-hide", "_mm_packs_epi16",
			"-ccgo-hide", "_mm_packs_epi32",
			"-ccgo-hide", "_mm_packs_pi16",
			"-ccgo-hide", "_mm_packs_pi32",
			"-ccgo-hide", "_mm_packs_pu16",
			"-ccgo-hide", "_mm_packus_epi16",
			"-ccgo-hide", "_mm_packus_epi32",
			"-ccgo-hide", "_mm_pause",
			"-ccgo-hide", "_mm_perm_epi8",
			"-ccgo-hide", "_mm_permutevar_pd",
			"-ccgo-hide", "_mm_permutevar_ps",
			"-ccgo-hide", "_mm_permutex2var_epi16",
			"-ccgo-hide", "_mm_permutex2var_epi32",
			"-ccgo-hide", "_mm_permutex2var_epi64",
			"-ccgo-hide", "_mm_permutex2var_epi8",
			"-ccgo-hide", "_mm_permutex2var_pd",
			"-ccgo-hide", "_mm_permutex2var_ps",
			"-ccgo-hide", "_mm_permutexvar_epi16",
			"-ccgo-hide", "_mm_permutexvar_epi8",
			"-ccgo-hide", "_mm_popcnt_epi16",
			"-ccgo-hide", "_mm_popcnt_epi32",
			"-ccgo-hide", "_mm_popcnt_epi64",
			"-ccgo-hide", "_mm_popcnt_epi8",
			"-ccgo-hide", "_mm_popcnt_u32",
			"-ccgo-hide", "_mm_popcnt_u64",
			"-ccgo-hide", "_mm_rcp14_pd",
			"-ccgo-hide", "_mm_rcp14_ps",
			"-ccgo-hide", "_mm_rcp14_sd",
			"-ccgo-hide", "_mm_rcp14_ss",
			"-ccgo-hide", "_mm_rcp_ps",
			"-ccgo-hide", "_mm_rcp_ss",
			"-ccgo-hide", "_mm_rolv_epi32",
			"-ccgo-hide", "_mm_rolv_epi64",
			"-ccgo-hide", "_mm_rorv_epi32",
			"-ccgo-hide", "_mm_rorv_epi64",
			"-ccgo-hide", "_mm_rot_epi16",
			"-ccgo-hide", "_mm_rot_epi32",
			"-ccgo-hide", "_mm_rot_epi64",
			"-ccgo-hide", "_mm_rot_epi8",
			"-ccgo-hide", "_mm_rsqrt14_pd",
			"-ccgo-hide", "_mm_rsqrt14_ps",
			"-ccgo-hide", "_mm_rsqrt14_sd",
			"-ccgo-hide", "_mm_rsqrt14_ss",
			"-ccgo-hide", "_mm_rsqrt_ps",
			"-ccgo-hide", "_mm_rsqrt_ss",
			"-ccgo-hide", "_mm_sad_epu8",
			"-ccgo-hide", "_mm_sad_pu8",
			"-ccgo-hide", "_mm_scalef_pd",
			"-ccgo-hide", "_mm_scalef_ps",
			"-ccgo-hide", "_mm_scalef_sd",
			"-ccgo-hide", "_mm_scalef_ss",
			"-ccgo-hide", "_mm_set1_epi16",
			"-ccgo-hide", "_mm_set1_epi32",
			"-ccgo-hide", "_mm_set1_epi64",
			"-ccgo-hide", "_mm_set1_epi64x",
			"-ccgo-hide", "_mm_set1_epi8",
			"-ccgo-hide", "_mm_set1_pd",
			"-ccgo-hide", "_mm_set1_pi16",
			"-ccgo-hide", "_mm_set1_pi32",
			"-ccgo-hide", "_mm_set1_pi8",
			"-ccgo-hide", "_mm_set1_ps",
			"-ccgo-hide", "_mm_set_epi16",
			"-ccgo-hide", "_mm_set_epi32",
			"-ccgo-hide", "_mm_set_epi64",
			"-ccgo-hide", "_mm_set_epi64x",
			"-ccgo-hide", "_mm_set_epi8",
			"-ccgo-hide", "_mm_set_pd",
			"-ccgo-hide", "_mm_set_pd1",
			"-ccgo-hide", "_mm_set_pi16",
			"-ccgo-hide", "_mm_set_pi32",
			"-ccgo-hide", "_mm_set_pi64x",
			"-ccgo-hide", "_mm_set_pi8",
			"-ccgo-hide", "_mm_set_ps",
			"-ccgo-hide", "_mm_set_ps1",
			"-ccgo-hide", "_mm_set_sd",
			"-ccgo-hide", "_mm_set_ss",
			"-ccgo-hide", "_mm_setcsr",
			"-ccgo-hide", "_mm_setr_epi16",
			"-ccgo-hide", "_mm_setr_epi32",
			"-ccgo-hide", "_mm_setr_epi64",
			"-ccgo-hide", "_mm_setr_epi8",
			"-ccgo-hide", "_mm_setr_pd",
			"-ccgo-hide", "_mm_setr_pi16",
			"-ccgo-hide", "_mm_setr_pi32",
			"-ccgo-hide", "_mm_setr_pi8",
			"-ccgo-hide", "_mm_setr_ps",
			"-ccgo-hide", "_mm_setzero_pd",
			"-ccgo-hide", "_mm_setzero_ps",
			"-ccgo-hide", "_mm_setzero_si128",
			"-ccgo-hide", "_mm_setzero_si64",
			"-ccgo-hide", "_mm_sfence",
			"-ccgo-hide", "_mm_sha1msg1_epu32",
			"-ccgo-hide", "_mm_sha1msg2_epu32",
			"-ccgo-hide", "_mm_sha1nexte_epu32",
			"-ccgo-hide", "_mm_sha256msg1_epu32",
			"-ccgo-hide", "_mm_sha256msg2_epu32",
			"-ccgo-hide", "_mm_sha256rnds2_epu32",
			"-ccgo-hide", "_mm_sha_epi16",
			"-ccgo-hide", "_mm_sha_epi32",
			"-ccgo-hide", "_mm_sha_epi64",
			"-ccgo-hide", "_mm_sha_epi8",
			"-ccgo-hide", "_mm_shl_epi16",
			"-ccgo-hide", "_mm_shl_epi32",
			"-ccgo-hide", "_mm_shl_epi64",
			"-ccgo-hide", "_mm_shl_epi8",
			"-ccgo-hide", "_mm_shldv_epi16",
			"-ccgo-hide", "_mm_shldv_epi32",
			"-ccgo-hide", "_mm_shldv_epi64",
			"-ccgo-hide", "_mm_shrdv_epi16",
			"-ccgo-hide", "_mm_shrdv_epi32",
			"-ccgo-hide", "_mm_shrdv_epi64",
			"-ccgo-hide", "_mm_shuffle_epi8",
			"-ccgo-hide", "_mm_shuffle_pi8",
			"-ccgo-hide", "_mm_sign_epi16",
			"-ccgo-hide", "_mm_sign_epi32",
			"-ccgo-hide", "_mm_sign_epi8",
			"-ccgo-hide", "_mm_sign_pi16",
			"-ccgo-hide", "_mm_sign_pi32",
			"-ccgo-hide", "_mm_sign_pi8",
			"-ccgo-hide", "_mm_sll_epi16",
			"-ccgo-hide", "_mm_sll_epi32",
			"-ccgo-hide", "_mm_sll_epi64",
			"-ccgo-hide", "_mm_sll_pi16",
			"-ccgo-hide", "_mm_sll_pi32",
			"-ccgo-hide", "_mm_sll_si64",
			"-ccgo-hide", "_mm_slli_epi16",
			"-ccgo-hide", "_mm_slli_epi32",
			"-ccgo-hide", "_mm_slli_epi64",
			"-ccgo-hide", "_mm_slli_pi16",
			"-ccgo-hide", "_mm_slli_pi32",
			"-ccgo-hide", "_mm_slli_si64",
			"-ccgo-hide", "_mm_sllv_epi16",
			"-ccgo-hide", "_mm_sllv_epi32",
			"-ccgo-hide", "_mm_sllv_epi64",
			"-ccgo-hide", "_mm_sqrt_pd",
			"-ccgo-hide", "_mm_sqrt_ps",
			"-ccgo-hide", "_mm_sqrt_sd",
			"-ccgo-hide", "_mm_sqrt_ss",
			"-ccgo-hide", "_mm_sra_epi16",
			"-ccgo-hide", "_mm_sra_epi32",
			"-ccgo-hide", "_mm_sra_epi64",
			"-ccgo-hide", "_mm_sra_pi16",
			"-ccgo-hide", "_mm_sra_pi32",
			"-ccgo-hide", "_mm_srai_epi16",
			"-ccgo-hide", "_mm_srai_epi32",
			"-ccgo-hide", "_mm_srai_pi16",
			"-ccgo-hide", "_mm_srai_pi32",
			"-ccgo-hide", "_mm_srav_epi16",
			"-ccgo-hide", "_mm_srav_epi32",
			"-ccgo-hide", "_mm_srav_epi64",
			"-ccgo-hide", "_mm_srl_epi16",
			"-ccgo-hide", "_mm_srl_epi32",
			"-ccgo-hide", "_mm_srl_epi64",
			"-ccgo-hide", "_mm_srl_pi16",
			"-ccgo-hide", "_mm_srl_pi32",
			"-ccgo-hide", "_mm_srl_si64",
			"-ccgo-hide", "_mm_srli_epi16",
			"-ccgo-hide", "_mm_srli_epi32",
			"-ccgo-hide", "_mm_srli_epi64",
			"-ccgo-hide", "_mm_srli_pi16",
			"-ccgo-hide", "_mm_srli_pi32",
			"-ccgo-hide", "_mm_srli_si64",
			"-ccgo-hide", "_mm_srlv_epi16",
			"-ccgo-hide", "_mm_srlv_epi32",
			"-ccgo-hide", "_mm_srlv_epi64",
			"-ccgo-hide", "_mm_store1_pd",
			"-ccgo-hide", "_mm_store1_ps",
			"-ccgo-hide", "_mm_store_epi64",
			"-ccgo-hide", "_mm_store_pd",
			"-ccgo-hide", "_mm_store_pd1",
			"-ccgo-hide", "_mm_store_ps",
			"-ccgo-hide", "_mm_store_ps1",
			"-ccgo-hide", "_mm_store_sd",
			"-ccgo-hide", "_mm_store_si128",
			"-ccgo-hide", "_mm_store_ss",
			"-ccgo-hide", "_mm_storeh_pd",
			"-ccgo-hide", "_mm_storeh_pi",
			"-ccgo-hide", "_mm_storel_epi64",
			"-ccgo-hide", "_mm_storel_pd",
			"-ccgo-hide", "_mm_storel_pi",
			"-ccgo-hide", "_mm_storer_pd",
			"-ccgo-hide", "_mm_storer_ps",
			"-ccgo-hide", "_mm_storeu_pd",
			"-ccgo-hide", "_mm_storeu_ps",
			"-ccgo-hide", "_mm_storeu_si128",
			"-ccgo-hide", "_mm_stream_load_si128",
			"-ccgo-hide", "_mm_stream_pd",
			"-ccgo-hide", "_mm_stream_pi",
			"-ccgo-hide", "_mm_stream_ps",
			"-ccgo-hide", "_mm_stream_sd",
			"-ccgo-hide", "_mm_stream_si128",
			"-ccgo-hide", "_mm_stream_si32",
			"-ccgo-hide", "_mm_stream_si64",
			"-ccgo-hide", "_mm_stream_ss",
			"-ccgo-hide", "_mm_sub_epi16",
			"-ccgo-hide", "_mm_sub_epi32",
			"-ccgo-hide", "_mm_sub_epi64",
			"-ccgo-hide", "_mm_sub_epi8",
			"-ccgo-hide", "_mm_sub_pd",
			"-ccgo-hide", "_mm_sub_pi16",
			"-ccgo-hide", "_mm_sub_pi32",
			"-ccgo-hide", "_mm_sub_pi8",
			"-ccgo-hide", "_mm_sub_ps",
			"-ccgo-hide", "_mm_sub_sd",
			"-ccgo-hide", "_mm_sub_si64",
			"-ccgo-hide", "_mm_sub_ss",
			"-ccgo-hide", "_mm_subs_epi16",
			"-ccgo-hide", "_mm_subs_epi8",
			"-ccgo-hide", "_mm_subs_epu16",
			"-ccgo-hide", "_mm_subs_epu8",
			"-ccgo-hide", "_mm_subs_pi16",
			"-ccgo-hide", "_mm_subs_pi8",
			"-ccgo-hide", "_mm_subs_pu16",
			"-ccgo-hide", "_mm_subs_pu8",
			"-ccgo-hide", "_mm_test_epi16_mask",
			"-ccgo-hide", "_mm_test_epi32_mask",
			"-ccgo-hide", "_mm_test_epi64_mask",
			"-ccgo-hide", "_mm_test_epi8_mask",
			"-ccgo-hide", "_mm_testc_pd",
			"-ccgo-hide", "_mm_testc_ps",
			"-ccgo-hide", "_mm_testc_si128",
			"-ccgo-hide", "_mm_testn_epi16_mask",
			"-ccgo-hide", "_mm_testn_epi32_mask",
			"-ccgo-hide", "_mm_testn_epi64_mask",
			"-ccgo-hide", "_mm_testn_epi8_mask",
			"-ccgo-hide", "_mm_testnzc_pd",
			"-ccgo-hide", "_mm_testnzc_ps",
			"-ccgo-hide", "_mm_testnzc_si128",
			"-ccgo-hide", "_mm_testz_pd",
			"-ccgo-hide", "_mm_testz_ps",
			"-ccgo-hide", "_mm_testz_si128",
			"-ccgo-hide", "_mm_ucomieq_sd",
			"-ccgo-hide", "_mm_ucomieq_ss",
			"-ccgo-hide", "_mm_ucomige_sd",
			"-ccgo-hide", "_mm_ucomige_ss",
			"-ccgo-hide", "_mm_ucomigt_sd",
			"-ccgo-hide", "_mm_ucomigt_ss",
			"-ccgo-hide", "_mm_ucomile_sd",
			"-ccgo-hide", "_mm_ucomile_ss",
			"-ccgo-hide", "_mm_ucomilt_sd",
			"-ccgo-hide", "_mm_ucomilt_ss",
			"-ccgo-hide", "_mm_ucomineq_sd",
			"-ccgo-hide", "_mm_ucomineq_ss",
			"-ccgo-hide", "_mm_undefined_pd",
			"-ccgo-hide", "_mm_undefined_ps",
			"-ccgo-hide", "_mm_undefined_si128",
			"-ccgo-hide", "_mm_unpackhi_epi16",
			"-ccgo-hide", "_mm_unpackhi_epi32",
			"-ccgo-hide", "_mm_unpackhi_epi64",
			"-ccgo-hide", "_mm_unpackhi_epi8",
			"-ccgo-hide", "_mm_unpackhi_pd",
			"-ccgo-hide", "_mm_unpackhi_pi16",
			"-ccgo-hide", "_mm_unpackhi_pi32",
			"-ccgo-hide", "_mm_unpackhi_pi8",
			"-ccgo-hide", "_mm_unpackhi_ps",
			"-ccgo-hide", "_mm_unpacklo_epi16",
			"-ccgo-hide", "_mm_unpacklo_epi32",
			"-ccgo-hide", "_mm_unpacklo_epi64",
			"-ccgo-hide", "_mm_unpacklo_epi8",
			"-ccgo-hide", "_mm_unpacklo_pd",
			"-ccgo-hide", "_mm_unpacklo_pi16",
			"-ccgo-hide", "_mm_unpacklo_pi32",
			"-ccgo-hide", "_mm_unpacklo_pi8",
			"-ccgo-hide", "_mm_unpacklo_ps",
			"-ccgo-hide", "_mm_xor_pd",
			"-ccgo-hide", "_mm_xor_ps",
			"-ccgo-hide", "_mm_xor_si128",
			"-ccgo-hide", "_mm_xor_si64",
			"-ccgo-hide", "_movdir64b",
			"-ccgo-hide", "_mulx_u64",
			"-ccgo-hide", "_pconfig_u32",
			"-ccgo-hide", "_pdep_u32",
			"-ccgo-hide", "_pdep_u64",
			"-ccgo-hide", "_pext_u32",
			"-ccgo-hide", "_pext_u64",
			"-ccgo-hide", "_rdpid_u32",
			"-ccgo-hide", "_rdpkru_u32",
			"-ccgo-hide", "_rdrand16_step",
			"-ccgo-hide", "_rdrand32_step",
			"-ccgo-hide", "_rdrand64_step",
			"-ccgo-hide", "_rdseed16_step",
			"-ccgo-hide", "_rdseed32_step",
			"-ccgo-hide", "_rdseed64_step",
			"-ccgo-hide", "_readfsbase_u32",
			"-ccgo-hide", "_readfsbase_u64",
			"-ccgo-hide", "_readgsbase_u32",
			"-ccgo-hide", "_readgsbase_u64",
			"-ccgo-hide", "_rstorssp",
			"-ccgo-hide", "_saveprevssp",
			"-ccgo-hide", "_setssbsy",
			"-ccgo-hide", "_store_mask16",
			"-ccgo-hide", "_store_mask32",
			"-ccgo-hide", "_store_mask64",
			"-ccgo-hide", "_store_mask8",
			"-ccgo-hide", "_subborrow_u32",
			"-ccgo-hide", "_subborrow_u64",
			"-ccgo-hide", "_tzcnt_u32",
			"-ccgo-hide", "_tzcnt_u64",
			"-ccgo-hide", "_wbinvd",
			"-ccgo-hide", "_wbnoinvd",
			"-ccgo-hide", "_writefsbase_u32",
			"-ccgo-hide", "_writefsbase_u64",
			"-ccgo-hide", "_writegsbase_u32",
			"-ccgo-hide", "_writegsbase_u64",
			"-ccgo-hide", "_wrpkru",
			"-ccgo-hide", "_wrssd",
			"-ccgo-hide", "_wrssq",
			"-ccgo-hide", "_wrussd",
			"-ccgo-hide", "_wrussq",
			"-ccgo-hide", "_xbegin",
			"-ccgo-hide", "_xend",
			"-ccgo-hide", "_xgetbv",
			"-ccgo-hide", "_xrstor",
			"-ccgo-hide", "_xrstor64",
			"-ccgo-hide", "_xrstors",
			"-ccgo-hide", "_xrstors64",
			"-ccgo-hide", "_xsave",
			"-ccgo-hide", "_xsave64",
			"-ccgo-hide", "_xsavec",
			"-ccgo-hide", "_xsavec64",
			"-ccgo-hide", "_xsaveopt",
			"-ccgo-hide", "_xsaveopt64",
			"-ccgo-hide", "_xsaves",
			"-ccgo-hide", "_xsaves64",
			"-ccgo-hide", "_xsetbv",
			"-ccgo-hide", "_xtest",
			"-ccgo-hide", "g_rgSCardRawPci",
			"-ccgo-hide", "g_rgSCardT0Pci",
			"-ccgo-hide", "g_rgSCardT1Pci",
		)
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
	if out, err := exec.Command("go", "build", "-o", "shell", main).CombinedOutput(); err != nil {
		s := strings.TrimSpace(string(out))
		if s != "" {
			s += "\n"
		}
		t.Errorf("%s%v", s, err)
		return
	}

	out, err := exec.Command("./shell", "tmp", "create table t(i); insert into t values(42); select 11*i from t;").CombinedOutput()
	if err != nil {
		if *oTrace {
			fmt.Printf("%s\n%s\n", out, err)
		}
		t.Errorf("%s\n%v", out, err)
		return
	}

	if g, e := strings.TrimSpace(string(out)), "462"; g != e {
		t.Errorf("%q %q", g, e)
	}
	if *oTraceO {
		fmt.Printf("%s\n", out)
	}

	if out, err = exec.Command("./shell", "tmp", "select 13*i from t;").CombinedOutput(); err != nil {
		if *oTrace {
			fmt.Printf("%s\n%s\n", out, err)
		}
		t.Errorf("%v", err)
		return
	}

	if g, e := strings.TrimSpace(string(out)), "546"; g != e {
		t.Errorf("%q %q", g, e)
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
		if runtime.GOOS == "windows" {
			switch base {
			case "bisect.c", "fftw.c", "perlin.c": // mingw on windows prints one zero too many for %.4e
				r = append(r, &compCertResult{nm, base, d, 0, true, true, true})
				continue
			}

			out = bytes.ReplaceAll(out, []byte{'\r', '\n'}, []byte{'\n'})
		}
		r = append(r, &compCertResult{nm, base, d, 0, true, true, checkResult(t, out, base, rdir)})
	}
	return r
}

func checkResult(t *testing.T, out []byte, base, rdir string) bool {
	base = base[:len(base)-len(filepath.Ext(base))]
	b, err := ioutil.ReadFile(filepath.Join(rdir, base))
	if err != nil {
		t.Errorf("%v: %v", base, err)
		return false
	}

	if !bytes.Equal(out, b) {
		t.Logf("got\n%s", hex.Dump(out))
		t.Logf("exp\n%s", hex.Dump(b))
		t.Errorf("%v: result differs", base)
		return false
	}

	return true
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
		//TODO "--bitfields --max-nested-struct-level 10 --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 890611563",
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
