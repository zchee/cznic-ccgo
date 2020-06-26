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
	"modernc.org/crt/v3"
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
	oCSmith     = flag.Duration("csmith", time.Minute, "")
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
	sqliteDir = filepath.FromSlash("testdata/sqlite-amalgamation-3300100")
	tccDir    = filepath.FromSlash("testdata/tcc-0.9.27")
	mjsonDir  = filepath.FromSlash("testdata/microjson-1.5")

	testWD string

	csmithDefaultArgs = strings.Join([]string{
		"--bitfields",                     // --bitfields | --no-bitfields: enable | disable full-bitfields structs (disabled by default).
		"--max-nested-struct-level", "10", // --max-nested-struct-level <num>: limit maximum nested level of structs to <num>(default 0). Only works in the exhaustive mode.
		"--no-const-pointers", // --const-pointers | --no-const-pointers: enable | disable const pointers (enabled by default).
		"--no-consts",         // --consts | --no-consts: enable | disable const qualifier (enabled by default).
		"--no-packed-struct",  // --packed-struct | --no-packed-struct: enable | disable packed structs by adding #pragma pack(1) before struct definition (disabled by default).
		// "--no-safe-math",         // --safe-math | --no-safe-math: Emit safe math wrapper functions (enabled by default).
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
		{sqliteDir, "https://www.sqlite.org/2019/sqlite-amalgamation-3300100.zip", 2400, false},
		{tccDir, "http://download.savannah.gnu.org/releases/tinycc/tcc-0.9.27.tar.bz2", 620, false},
	}
)

func TestMain(m *testing.M) {
	defer func() {
		os.Exit(m.Run())
	}()

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
	if err != nil {
		t.Fatal(err)
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

		ccgoArgs := []string{"ccgo", "-o", main}
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
	var _ crt.Intptr
	task := newTask(nil, nil, nil)
	pkgName, capi, err := task.capi("modernc.org/crt/v3")
	if err != nil {
		t.Fatal(err)
	}

	if _, ok := capi["printf"]; !ok {
		t.Fatal("default crt does not export printf")
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
		"991014-1.c":   {}, // Struct type too big
		"align-3.c":    {}, // __attribute__((aligned(256)))
		"eeprof-1.c":   {}, // requires instrumentation
		"fp-cmp-1.c":   {}, // sigfpe
		"fp-cmp-2.c":   {}, // sigfpe
		"fp-cmp-3.c":   {}, // sigfpe

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
		"anon-1.c":        {}, //TODO nested field access
		"pr41317.c":       {}, //TODO nested field access
		"pr42570":         {}, //TODO uint8_t foo[1][0];
		"pushpop_macro.c": {}, //TODO #pragma push_macro("_")
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
		"-DLONGDOUBLE_TYPE=double",
		"-DSQLITE_DEBUG",
		"-DSQLITE_DEFAULT_MEMSTATUS=0",
		"-DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1",
		"-DSQLITE_DQS=0",
		"-DSQLITE_LIKE_DOESNT_MATCH_BLOBS",
		"-DSQLITE_MAX_EXPR_DEPTH=0",
		"-DSQLITE_MEMDEBUG",
		"-DSQLITE_OMIT_DECLTYPE",
		"-DSQLITE_OMIT_DEPRECATED",
		"-DSQLITE_OMIT_PROGRESS_CALLBACK",
		"-DSQLITE_OMIT_SHARED_CACHE",
		"-DSQLITE_THREADSAFE=0",
		"-ccgo-long-double-is-double", // stddef.h
		"-o", main,
		filepath.Join(dir, "shell.c"),
		filepath.Join(dir, "sqlite3.c"),
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

	ccgoArgs := []string{"ccgo", "-o", main, "-ccgo-long-double-is-double", filepath.Join(dir, "mjson.c"), filepath.Join(dir, "test_microjson.c")}
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
			return newTask([]string{"ccgo", "-o", src, fn, "-ccgo-long-double-is-double"}, nil, nil).main()
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

		ccgoArgs := []string{"ccgo", "-o", main}
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
	return //TODO-
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
		j := newTask([]string{"ccgo", "-o", binaryName, csp, "-ccgo-long-double-is-double", "main.c"}, &stdout, &stderr)

		func() {

			defer func() {
				if err := recover(); err != nil {
					t.Fatalf("%s\n%s\nccgo: %s\n%s\n%v\n%s", extra, csOut, stdout.Bytes(), stderr.Bytes(), err, debug.Stack())
				}
			}()

			if err := j.main(); err != nil || stdout.Len() != 0 {
				t.Fatalf("%s\n%s\nccgo: %s\n%s\n%v", extra, csOut, stdout.Bytes(), stderr.Bytes(), err)
			}
		}()

		binOutB, err := func() ([]byte, error) {
			ctx, cancel := context.WithTimeout(context.Background(), 100*time.Second)
			defer cancel()

			return exec.CommandContext(ctx, binaryName).CombinedOutput()
		}()
		if err != nil {
			t.Errorf("%s\n%s\nccgo: %v", extra, csOut, err)
			break
		}

		if g, e := binOutB, binOutA; !bytes.Equal(g, e) {
			t.Errorf("%s\n%s\nccgo: %v\ngot: %s\nexp: %s", extra, csOut, err, g, e)
			break
		}

		ok++
		if *oTrace {
			fmt.Fprintln(os.Stderr, time.Since(t0), files, ok, " no opt")
		}

		if err := os.Remove(binaryName); err != nil {
			t.Fatal(err)
		}
	}
	d := time.Since(t0)
	t.Logf("files %v, bytes %v, ok %v in %v", h(files), h(size), h(ok), d)
}
