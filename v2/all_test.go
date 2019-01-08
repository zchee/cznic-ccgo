// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//	go version go1.11.2 linux/386
//
//	Sa Nov 24 21:05:15 CET 2018
//	TCC	cc 51 ccgo 51 build 51 run 51 ok 51 n 51
//	Other	cc 35 ccgo 35 build 35 run 35 ok 35 n 35
//	GCC Compat	cc 7 ccgo 7 build 7 run 7 ok 7 n 7
//	GCC Compile	cc 993 ccgo 993 build 993 ok 993 n 1708
//	GCC Execute	cc 1097 ccgo 1097 build 1097 run 1097 ok 1097 n 1413
//	Shell	cc 1 ccgo 1 build 1 run 1 ok 1 n 1
//	cc 1 ccgo 1 build 1 run 1 ok 1 (100.00%) csmith 1 (1.772384862s) -s 1322542678
//	cc 2 ccgo 2 build 2 run 2 ok 2 (100.00%) csmith 2 (9.220829194s) -s 1322542678
//	cc 3 ccgo 3 build 3 run 3 ok 3 (100.00%) csmith 3 (9.872009241s) -s 1901462611
//	cc 4 ccgo 4 build 4 run 4 ok 4 (100.00%) csmith 4 (11.977540362s) -s 2829621573
//	cc 5 ccgo 5 build 5 run 5 ok 5 (100.00%) csmith 5 (12.59954287s) -s 576413934
//	cc 6 ccgo 6 build 6 run 6 ok 6 (100.00%) csmith 6 (14.668189393s) -s 151038493
//	cc 7 ccgo 7 build 7 run 7 ok 7 (100.00%) csmith 7 (16.691730411s)
//	cc 8 ccgo 8 build 8 run 8 ok 8 (100.00%) csmith 8 (24.674610885s)
//	cc 9 ccgo 9 build 9 run 9 ok 9 (100.00%) csmith 9 (32.781642971s)
//	cc 10 ccgo 10 build 10 run 10 ok 10 (100.00%) csmith 10 (33.384791624s)
//	cc 11 ccgo 11 build 11 run 11 ok 11 (100.00%) csmith 11 (34.611252984s)
//	cc 12 ccgo 12 build 12 run 12 ok 12 (100.00%) csmith 12 (35.559574894s)
//	cc 13 ccgo 13 build 13 run 13 ok 13 (100.00%) csmith 13 (36.175262857s)
//	cc 14 ccgo 14 build 14 run 14 ok 14 (100.00%) csmith 14 (36.794166036s)
//	cc 15 ccgo 15 build 15 run 15 ok 15 (100.00%) csmith 15 (37.428945143s)
//	cc 16 ccgo 16 build 16 run 16 ok 16 (100.00%) csmith 16 (38.434621417s)
//	cc 17 ccgo 17 build 17 run 17 ok 17 (100.00%) csmith 17 (39.66653564s)
//	cc 18 ccgo 18 build 18 run 18 ok 18 (100.00%) csmith 18 (41.348559407s)
//	cc 19 ccgo 19 build 19 run 19 ok 19 (100.00%) csmith 19 (41.985155382s)
//	cc 20 ccgo 20 build 20 run 20 ok 20 (100.00%) csmith 20 (44.049111677s)
//	cc 21 ccgo 21 build 21 run 21 ok 21 (100.00%) csmith 21 (45.385363353s)
//	cc 22 ccgo 22 build 22 run 22 ok 22 (100.00%) csmith 22 (46.391109708s)
//	cc 23 ccgo 23 build 23 run 23 ok 23 (100.00%) csmith 23 (47.520247769s)
//	cc 24 ccgo 24 build 24 run 24 ok 24 (100.00%) csmith 24 (48.818993469s)
//	cc 25 ccgo 25 build 25 run 25 ok 25 (100.00%) csmith 25 (49.810744952s)
//	cc 26 ccgo 26 build 26 run 26 ok 26 (100.00%) csmith 26 (51.041599503s)
//	cc 27 ccgo 27 build 27 run 27 ok 27 (100.00%) csmith 27 (51.949928976s)
//	cc 28 ccgo 28 build 28 run 28 ok 28 (100.00%) csmith 28 (53.557046737s)
//	cc 29 ccgo 29 build 29 run 29 ok 29 (100.00%) csmith 29 (54.66904889s)
//	CSmith0	cc 29 ccgo 29 build 29 run 29 ok 29 (100.00%) csmith 29 (1m1.787559698s)
//	PASS
//	ok  	modernc.org/ccgo/v2	824.188s

//	go version go1.11.4 linux/amd64
//
//	Tue Jan  8 16:11:42 CET 2019
//	TCC	cc 51 ccgo 51 build 51 run 51 ok 51 n 51
//	Other	cc 35 ccgo 35 build 35 run 35 ok 35 n 35
//	GCC Compat	cc 7 ccgo 7 build 7 run 7 ok 7 n 7
//	GCC Compile	cc 1005 ccgo 1005 build 1005 ok 1005 n 1708
//	GCC Execute	cc 1101 ccgo 1101 build 1101 run 1101 ok 1101 n 1411
//	Shell	cc 1 ccgo 1 build 1 run 1 ok 1 n 1
//	cc 1 ccgo 1 build 1 run 1 ok 1 (100.00%) csmith 1 (1.265786915s) -s 1322542678
//	cc 2 ccgo 2 build 2 run 2 ok 2 (100.00%) csmith 2 (8.424790959s) -s 1322542678
//	cc 3 ccgo 3 build 3 run 3 ok 3 (100.00%) csmith 3 (8.878088363s) -s 1901462611
//	cc 4 ccgo 4 build 4 run 4 ok 4 (100.00%) csmith 4 (10.504015699s) -s 2829621573
//	cc 5 ccgo 5 build 5 run 5 ok 5 (100.00%) csmith 5 (10.950829266s) -s 576413934
//	cc 6 ccgo 6 build 6 run 6 ok 6 (100.00%) csmith 6 (12.371047213s) -s 151038493
//	cc 7 ccgo 7 build 7 run 7 ok 7 (100.00%) csmith 7 (14.594555736s)
//	cc 8 ccgo 8 build 8 run 8 ok 8 (100.00%) csmith 8 (15.631812515s)
//	cc 9 ccgo 9 build 9 run 9 ok 9 (100.00%) csmith 9 (16.897486985s)
//	cc 10 ccgo 10 build 10 run 10 ok 10 (100.00%) csmith 10 (18.144851164s)
//	cc 11 ccgo 11 build 11 run 11 ok 11 (100.00%) csmith 11 (19.901257906s)
//	cc 12 ccgo 12 build 12 run 12 ok 12 (100.00%) csmith 12 (21.034974418s)
//	cc 13 ccgo 13 build 13 run 13 ok 13 (100.00%) csmith 13 (21.478276269s)
//	cc 14 ccgo 14 build 14 run 14 ok 14 (100.00%) csmith 14 (22.451478736s)
//	cc 15 ccgo 15 build 15 run 15 ok 15 (100.00%) csmith 15 (22.893469656s)
//	cc 16 ccgo 16 build 16 run 16 ok 16 (100.00%) csmith 16 (23.688706002s)
//	cc 17 ccgo 17 build 17 run 17 ok 17 (100.00%) csmith 17 (24.123497626s)
//	cc 18 ccgo 18 build 18 run 18 ok 18 (100.00%) csmith 18 (24.967377057s)
//	cc 19 ccgo 19 build 19 run 19 ok 19 (100.00%) csmith 19 (25.404479678s)
//	cc 20 ccgo 20 build 20 run 20 ok 20 (100.00%) csmith 20 (26.590458339s)
//	cc 21 ccgo 21 build 21 run 21 ok 21 (100.00%) csmith 21 (35.898416992s)
//	cc 22 ccgo 22 build 22 run 22 ok 22 (100.00%) csmith 22 (36.90634691s)
//	cc 23 ccgo 23 build 23 run 23 ok 23 (100.00%) csmith 23 (37.377914725s)
//	cc 24 ccgo 24 build 24 run 24 ok 24 (100.00%) csmith 24 (38.783921397s)
//	cc 25 ccgo 25 build 25 run 25 ok 25 (100.00%) csmith 25 (41.883502521s)
//	cc 26 ccgo 26 build 26 run 26 ok 26 (100.00%) csmith 26 (42.937642235s)
//	cc 27 ccgo 27 build 27 run 27 ok 27 (100.00%) csmith 27 (44.4482684s)
//	cc 28 ccgo 28 build 28 run 28 ok 28 (100.00%) csmith 28 (45.862920722s)
//	cc 29 ccgo 29 build 29 run 29 ok 29 (100.00%) csmith 29 (55.090609047s)
//	cc 30 ccgo 30 build 30 run 30 ok 30 (100.00%) csmith 30 (56.192073861s)
//	cc 31 ccgo 31 build 31 run 31 ok 31 (100.00%) csmith 31 (57.51028454s)
//	cc 32 ccgo 32 build 32 run 32 ok 32 (100.00%) csmith 32 (59.164144415s)
//	cc 33 ccgo 33 build 33 run 33 ok 33 (100.00%) csmith 33 (1m0.067423691s)
//	CSmith0	cc 33 ccgo 33 build 33 run 33 ok 33 (100.00%) csmith 33 (1m0.067445063s)
//	PASS
//	ok  	modernc.org/ccgo/v2	542.251s

package ccgo // import "modernc.org/ccgo/v2"

import (
	"bytes"
	"context"
	"encoding/hex"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
	"testing"
	"time"

	"modernc.org/cc/v2"
	"modernc.org/strutil"
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

func use(...interface{}) {}

func init() {
	use(caller, dbg, TODO) //TODOOK
	flag.BoolVar(&traceLConsts, "lconst", false, "")
	flag.BoolVar(&traceOpt, "to", false, "")
	flag.BoolVar(&traceTODO, "todo", false, "")
	flag.BoolVar(&traceWrites, "tw", false, "")
	isTesting = true
}

// ============================================================================

const (
	testTimeout = 60 * time.Second
)

var (
	oBuild   = flag.Bool("build", false, "full build errors")
	oCC      = flag.Bool("cc", false, "full cc errors")
	oCCGO    = flag.Bool("ccgo", false, "full ccgo errors")
	oCSmith  = flag.Duration("csmith", time.Minute, "") // Use something like -timeout 25h -csmith 24h for real testing.
	oEdit    = flag.Bool("edit", false, "")
	oI       = flag.String("I", "", "")
	oNoCmp   = flag.Bool("nocmp", false, "")
	oRE      = flag.String("re", "", "")
	oTCLRace = flag.Bool("tclrace", false, "")
	oTmp     = flag.String("tmp", "", "")
	oTrace   = flag.Bool("trc", false, "")

	re          *regexp.Regexp
	searchPaths []string
	defCCGO     = cc.NewStringSource("<defines>", "#define __ccgo__ 1\n#define __FUNCTION__ __func__\n")
)

func init() {
	var err error
	if searchPaths, err = cc.Paths(true); err != nil {
		panic(err)
	}
}

func TestOpt(t *testing.T) {
	for _, v := range []struct{ in, out string }{
		{"var _ = (a(b))", "var _ = a(b)"},
		{"var _ = ((a)(b))", "var _ = a(b)"},
		{"var _ = *((*a)(b))", "var _ = *(*a)(b)"},
	} {
		in := bytes.NewBufferString(v.in)
		var out bytes.Buffer
		if err := newOpt().do(&out, in, "TestOp"); err != nil {
			t.Fatal(err)
		}
		if g, e := bytes.TrimSpace(out.Bytes()), []byte(v.out); !bytes.Equal(g, e) {
			t.Fatalf("got\n%s\nexp\n%s", g, e)
		}
	}
}

func trim(b []byte) []byte {
	a := bytes.Split(b, []byte{'\n'})
	for i, v := range a {
		a[i] = bytes.TrimRight(v, " ")
	}
	return bytes.Join(a, []byte{'\n'})
}

func translate(tweaks *cc.Tweaks, includePaths, sysIncludePaths []string, def string, sources ...cc.Source) (*cc.TranslationUnit, error) {
	in := []cc.Source{defCCGO, cc.MustBuiltin()}
	if def != "" {
		in = append(in, cc.NewStringSource("<defines>", def))
	}
	in = append(in, sources...)
	if *oTrace {
		fmt.Fprintln(os.Stderr, in)
	}
	return cc.Translate(tweaks, includePaths, sysIncludePaths, in...)
}

func TestTCC(t *testing.T) {
	blacklist := map[string]struct{}{
		"13_integer_literals.c": {}, // 9:12: ExprInt strconv.ParseUint: parsing "0b010101010101": invalid syntax
		"31_args.c":             {},
		"34_array_assignment.c": {}, // gcc: main.c:16:6: error: incompatible types when assigning to type ‘int[4]’ from type ‘int *’
		"46_grep.c":             {}, // incompatible forward declaration type
	}

	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-tcc-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	compiler, err := compileCCGO(dir)
	if err != nil {
		t.Fatal(err)
	}

	root, err := filepath.Abs(filepath.FromSlash("testdata/tcc-0.9.26/tests/tests2/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	m, err := filepath.Glob(root)
	if err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	testGo := filepath.Join(dir, "main.go")
	testBin := filepath.Join(dir, "main")
	var c, ccgo, build, run, ok, n int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, pth)
		}
		n++
		out, err := exec.Command(compiler, "--ccgo-struct-checks", "-o", testGo, pth).CombinedOutput()
		if err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		c++
		ccgo++
		if out, err := exec.Command("go", "build", "-o", testBin, testGo).CombinedOutput(); err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		build++
		if out, err = exec.Command(testBin).CombinedOutput(); err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		run++
		expect, err := ioutil.ReadFile(pth[:len(pth)-len(filepath.Ext(pth))] + ".expect")
		if err != nil {
			if os.IsNotExist(err) {
				ok++
				continue
			}
		}

		out = trim(out)
		expect = trim(expect)
		if !bytes.Equal(out, expect) {
			t.Logf("%s\ngot\n%s\nexp\n%s----\ngot\n%s\nexp\n%s", pth, hex.Dump(out), hex.Dump(expect), out, expect)
			continue
		}

		ok++
	}
	if c != n || ccgo != n || build != n || run != n || ok != n {
		t.Fatalf("TCC cc %v ccgo %v build %v run %v ok %v n %v", c, ccgo, build, run, ok, n)
	}

	if *oEdit {
		fmt.Printf("TCC\tcc %v ccgo %v build %v run %v ok %v n %v\n", c, ccgo, build, run, ok, n)
	}
}

func compileCCGO(dir string) (string, error) {
	ip, err := strutil.ImportPath()
	if err != nil {
		return "", err
	}

	ip = filepath.Join(ip, "ccgo")
	compiler := filepath.Join(dir, "ccgo")
	out, err := exec.Command("go", "build", "-o", compiler, ip).CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("%s\n%v", out, err)
	}

	return compiler, nil
}

func TestOther(t *testing.T) {
	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-other-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	compiler, err := compileCCGO(dir)
	if err != nil {
		t.Fatal(err)
	}

	root, err := filepath.Abs(filepath.FromSlash("testdata/bug/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	m, err := filepath.Glob(root)
	if err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	testGo := filepath.Join(dir, "main.go")
	testBin := filepath.Join(dir, "main")
	var c, ccgo, build, run, ok, n int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, pth)
		}
		n++
		out, err := exec.Command(compiler, "--ccgo-struct-checks", "-o", testGo, pth).CombinedOutput()
		if err != nil {
			t.Logf("%s:\n%s\n%v", pth, out, err)
			continue
		}

		c++
		ccgo++
		if out, err := exec.Command("go", "build", "-o", testBin, testGo).CombinedOutput(); err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		build++
		if out, err = exec.Command(testBin).CombinedOutput(); err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		run++
		expect, err := ioutil.ReadFile(pth[:len(pth)-len(filepath.Ext(pth))] + ".expect")
		if err != nil {
			if os.IsNotExist(err) {
				ok++
				continue
			}
		}

		out = trim(out)
		expect = trim(expect)
		if !bytes.Equal(out, expect) {
			t.Logf("%s\ngot\n%s\nexp\n%s----\ngot\n%s\nexp\n%s", pth, hex.Dump(out), hex.Dump(expect), out, expect)
			continue
		}

		ok++
	}
	if c != n || ccgo != n || build != n || run != n || ok != n {
		t.Fatalf("Other\tcc %v ccgo %v build %v run %v ok %v n %v", c, ccgo, build, run, ok, n)
	}

	if *oEdit {
		fmt.Printf("Other\tcc %v ccgo %v build %v run %v ok %v n %v\n", c, ccgo, build, run, ok, n)
	}
}

func TestGCCCompat(t *testing.T) {
	blacklist := map[string]struct{}{}
	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-gcc-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	compiler, err := compileCCGO(dir)
	if err != nil {
		t.Fatal(err)
	}

	root, err := filepath.Abs(filepath.FromSlash("testdata/github.com/gcc-mirror/gcc/gcc/testsuite/gcc.c-torture/compat/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	m, err := filepath.Glob(root)
	if err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	testGo := filepath.Join(dir, "main.go")
	testBin := filepath.Join(dir, "main")
	var c, ccgo, build, run, ok, n int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, pth)
		}
		n++
		out, err := exec.Command(compiler, "--ccgo-struct-checks", "-o", testGo, pth).CombinedOutput()
		if err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		c++
		ccgo++
		if out, err := exec.Command("go", "build", "-o", testBin, testGo).CombinedOutput(); err != nil {
			t.Errorf("%s: %s\n%v", pth, out, err)
			continue
		}

		build++
		if out, err = exec.Command(testBin).CombinedOutput(); err != nil {
			t.Errorf("%s: %s\n%v", pth, out, err)
			continue
		}

		run++
		expect, err := ioutil.ReadFile(pth[:len(pth)-len(filepath.Ext(pth))] + ".expect")
		if err != nil {
			if os.IsNotExist(err) {
				ok++
				continue
			}
		}

		out = trim(out)
		expect = trim(expect)
		if !bytes.Equal(out, expect) {
			t.Errorf("%s\ngot\n%s\nexp\n%s----\ngot\n%s\nexp\n%s", pth, hex.Dump(out), hex.Dump(expect), out, expect)
			continue
		}

		ok++
	}

	if run == 0 || run != build || ok != build {
		t.Fatalf("cc %v ccgo %v build %v run %v ok %v n %v", c, ccgo, build, run, ok, n)
	}

	if *oEdit {
		fmt.Printf("GCC Compat\tcc %v ccgo %v build %v run %v ok %v n %v\n", c, ccgo, build, run, ok, n)
	}
}

func TestGCCCompile(t *testing.T) {
	blacklist := map[string]struct{}{}
	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-gcc-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	compiler, err := compileCCGO(dir)
	if err != nil {
		t.Fatal(err)
	}

	root, err := filepath.Abs(filepath.FromSlash("testdata/github.com/gcc-mirror/gcc/gcc/testsuite/gcc.c-torture/compile/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	m, err := filepath.Glob(root)
	if err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	testGo := filepath.Join(dir, "test.go")
	var c, ccgo, build, ok, n int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, pth)
		}
		n++
		out, err := exec.Command(compiler, "--ccgo-struct-checks", "-o", testGo, pth).CombinedOutput()
		if err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		c++
		ccgo++
		build++
		ok++
	}

	if ok != build {
		t.Fatalf("cc %v ccgo %v build %v ok %v n %v", c, ccgo, build, ok, n)
	}

	if *oEdit {
		fmt.Printf("GCC Compile\tcc %v ccgo %v build %v ok %v n %v\n", c, ccgo, build, ok, n)
	}
}

func TestGCCExecute(t *testing.T) {
	blacklist := map[string]struct{}{
		"20021127-1.c": {}, // non standard GCC behavior
		"20040520-1.c": {}, // Nested func
		"eeprof-1.c":   {}, // requires profiler instrumentation

		"20010904-1.c":                 {}, //TODO __attribute__((aligned(32)))
		"20010904-2.c":                 {}, //TODO __attribute__((aligned(32)))
		"20040811-1.c":                 {}, //TODO VLA
		"20101011-1.c":                 {}, //TODO Needs sigfpe on int division by zero
		"920929-1.c":                   {}, //TODO VLA
		"970217-1.c":                   {}, //TODO VLA
		"alias-3.c":                    {}, //TODO attr alias for data
		"bitfld-1.c":                   {}, //TODO bits, arithmetic precision
		"bitfld-3.c":                   {}, //TODO bits arithcmetic precision
		"builtin-types-compatible-p.c": {}, //TODO must track type qualifiers
		"medce-1.c":                    {}, //TODO Handle dead code
		"pr23467.c":                    {}, //TODO __attribute__ ((aligned (8)))
		"pr32244-1.c":                  {}, //TODO bits, arithmetic precision
		"pr34971.c":                    {}, //TODO bits, arithmetic precision
		"pr43220.c":                    {}, //TODO VLA
		"pr67037.c":                    {}, //TODO void f(); f(); f(42)
		"pr77767.c":                    {}, //TODO VLA
		"vla-dealloc-1.c":              {}, //TODO VLA
	}

	var re *regexp.Regexp
	if s := *oRE; s != "" {
		re = regexp.MustCompile(s)
	}

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-gcc-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	compiler, err := compileCCGO(dir)
	if err != nil {
		t.Fatal(err)
	}

	root, err := filepath.Abs(filepath.FromSlash("testdata/github.com/gcc-mirror/gcc/gcc/testsuite/gcc.c-torture/execute/*.c"))
	if err != nil {
		t.Fatal(err)
	}

	m, err := filepath.Glob(root)
	if err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	testGo := filepath.Join(dir, "main.go")
	testBin := filepath.Join(dir, "main")
	var c, ccgo, build, run, ok, n int
	for _, pth := range m {
		if re != nil && !re.MatchString(filepath.Base(pth)) {
			continue
		}

		if _, ok := blacklist[filepath.Base(pth)]; ok {
			continue
		}

		if *oTrace {
			fmt.Fprintln(os.Stderr, pth)
		}
		n++
		out, err := exec.Command(compiler, "--ccgo-struct-checks", "-o", testGo, pth).CombinedOutput()
		if err != nil {
			t.Logf("%s: %s\n%v", pth, out, err)
			continue
		}

		//dbg("%s", out) //TODO- DBG
		c++
		ccgo++
		if out, err := exec.Command("go", "build", "-o", testBin, testGo).CombinedOutput(); err != nil {
			t.Errorf("%s: %s\n%v", pth, out, err)
			continue
		}

		build++
		if out, err = exec.Command(testBin).CombinedOutput(); err != nil {
			t.Errorf("%s: %s\n%v", pth, out, err)
			continue
		}

		run++
		expect, err := ioutil.ReadFile(pth[:len(pth)-len(filepath.Ext(pth))] + ".expect")
		if err != nil {
			if os.IsNotExist(err) {
				ok++
				continue
			}
		}

		out = trim(out)
		expect = trim(expect)
		if !bytes.Equal(out, expect) {
			t.Errorf("%s\ngot\n%s\nexp\n%s----\ngot\n%s\nexp\n%s", pth, hex.Dump(out), hex.Dump(expect), out, expect)
			continue
		}

		ok++
	}

	if run == 0 || run != build || ok != build {
		t.Fatalf("cc %v ccgo %v build %v run %v ok %v n %v", c, ccgo, build, run, ok, n)
	}

	if *oEdit {
		fmt.Printf("GCC Execute\tcc %v ccgo %v build %v run %v ok %v n %v\n", c, ccgo, build, run, ok, n)
	}
}

func TestSQLiteShell(t *testing.T) {
	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-sqlite-shell-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	compiler, err := compileCCGO(dir)
	if err != nil {
		t.Fatal(err)
	}

	root, err := filepath.Abs(filepath.FromSlash("testdata/_sqlite/sqlite-amalgamation-3210000"))
	if err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	var c, ccgo, build, run, ok, n int
	for {
		n++
		out, err := exec.Command(compiler, "--ccgo-struct-checks", filepath.Join(root, "shell.c"), filepath.Join(root, "sqlite3.c")).CombinedOutput()
		if err != nil {
			t.Logf("%s: %s\n%v", dir, out, err)
			break
		}

		os.Remove(compiler)
		c++
		ccgo++
		build++
		m, err := filepath.Glob(filepath.Join(dir, "*"))
		if err != nil {
			t.Log(err)
			break

		}

		if len(m) != 1 {
			t.Fatalf("%v\n%v", len(m), m)
		}

		if out, err = exec.Command(m[0], "foo", "create table t(i)").CombinedOutput(); err != nil {
			t.Logf("\n%s\n%v", out, err)
			break
		}

		run++
		ok++
		break
	}
	if c != n || ccgo != n || build != n || run != n || ok != n {
		t.Fatalf("Shell cc %v ccgo %v build %v run %v ok %v n %v", c, ccgo, build, run, ok, n)
	}

	if *oEdit {
		fmt.Printf("Shell\tcc %v ccgo %v build %v run %v ok %v n %v\n", c, ccgo, build, run, ok, n)
	}
}

func TestCSmith(t *testing.T) {
	regressionTests := []string{
		//gcc bug "--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2117898882",

		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 1322542678",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 1352671588",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 1322542678",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 1901462611",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 2829621573",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 576413934",
		"--bitfields --no-const-pointers --no-consts --no-packed-struct --no-volatile-pointers --no-volatiles --paranoid -s 151038493",
	}

	defaultArgs := strings.Join([]string{
		"--bitfields",                     // --bitfields | --no-bitfields: enable | disable full-bitfields structs (disabled by default).
		"--no-const-pointers",             // --const-pointers | --no-const-pointers: enable | disable const pointers (enabled by default).
		"--no-consts",                     // --consts | --no-consts: enable | disable const qualifier (enabled by default).
		"--no-packed-struct",              // --packed-struct | --no-packed-struct: enable | disable packed structs by adding #pragma pack(1) before struct definition (disabled by default).
		"--no-volatile-pointers",          // --volatile-pointers | --no-volatile-pointers: enable | disable volatile pointers (enabled by default).
		"--no-volatiles",                  // --volatiles | --no-volatiles: enable | disable volatiles (enabled by default).
		"--paranoid",                      // --paranoid | --no-paranoid: enable | disable pointer-related assertions (disabled by default).
		"--max-nested-struct-level", "10", // --max-nested-struct-level <num>: limit maximum nested level of structs to <num>(default 0). Only works in the exhaustive mode.
	}, " ")

	cc.FlushCache()
	csmith, err := exec.LookPath("csmith")
	if err != nil {
		t.Logf("%v: skipping test", err)
		return
	}

	gcc, err := exec.LookPath("gcc")
	if err != nil {
		t.Logf("%v: skipping test", err)
		return
	}

	var inc string
	switch runtime.GOOS {
	case "linux":
		inc = "/usr/include"
	default:
		t.Logf("unsupported OS")
		return
	}
again:
	if _, err := os.Stat(filepath.Join(filepath.FromSlash(inc), "csmith.h")); err != nil {
		if os.IsNotExist(err) {
			if inc == "/usr/include" {
				inc += "/csmith"
				goto again
			}
			t.Logf("%s not found: skipping test", inc)
			return
		}

		t.Fatal(err)
	}

	dir := *oTmp
	if dir == "" {
		var err error
		if dir, err = ioutil.TempDir("", "test-ccgo-smith-"); err != nil {
			t.Fatal(err)
		}

		defer func() {
			if err := os.RemoveAll(dir); err != nil {
				t.Fatal(err)
			}
		}()
	}

	compiler, err := compileCCGO(dir)
	if err != nil {
		t.Fatal(err)
	}

	cwd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := os.Chdir(dir); err != nil {
		t.Fatal(err)
	}

	defer func() {
		if err := os.Chdir(cwd); err != nil {
			t.Fatal(err)
		}
	}()

	const (
		gccBin = "gcc"
		mainC  = "main.c"
	)

	for _, v := range []string{
		"csmith.h",
		"random_inc.h",
		"safe_math.h",
		"platform_generic.h",
	} {
		if err := cpFile(v, filepath.Join(inc, v), nil); err != nil {
			t.Fatal(v)
		}
	}

	ch := time.After(*oCSmith)
	var cs, cc, ccgo, build, run, ok int
	t0 := time.Now()
	argsHead := "-o " + mainC + " "
out:
	for i := 0; ; i++ {
		extra := ""
		args := argsHead
		switch {
		case i < len(regressionTests):
			args += regressionTests[i]
			a := strings.Split(regressionTests[i], " ")
			extra = strings.Join(a[len(a)-2:], " ")
		default:
			select {
			case <-ch:
				break out
			default:
			}

			args += defaultArgs
		}

		os.Remove(mainC)
		out, err := exec.Command(csmith, strings.Split(args, " ")...).Output()
		if err != nil {
			t.Fatalf("%v\n%s", err, out)
		}

		if out, err := exec.Command(gcc, "-w", "-o", gccBin, mainC).CombinedOutput(); err != nil {
			t.Fatalf("%v\n%s", err, out)
		}

		var gccOut []byte
		var gccT0 time.Time
		var gccT time.Duration
		func() {
			ctx, cancel := context.WithTimeout(context.Background(), testTimeout/10)

			defer cancel()

			gccT0 = time.Now()
			gccOut, err = exec.CommandContext(ctx, filepath.Join(dir, gccBin)).CombinedOutput()
			gccT = time.Since(gccT0)
		}()
		if err != nil {
			continue
		}

		cs++
		os.Remove("main.go")
		if out, err := exec.Command(compiler, "--ccgo-struct-checks", "-o", "main.go", mainC).CombinedOutput(); err != nil {
			t.Log(err)
			csmithFatal(t, mainC, gccOut, out, cc, ccgo, build, run, ok, cs, gccT)
			continue
		}

		cc++
		ccgo++
		if out, err := exec.Command("go", "build", "-o", "test.bin", "main.go").CombinedOutput(); err != nil {
			t.Log(err)
			csmithFatal(t, mainC, gccOut, out, cc, ccgo, build, run, ok, cs, gccT)
			continue
		}

		build++
		ccgoOut, err := exec.Command("./test.bin").CombinedOutput()
		if err != nil {
			t.Log(err)
			csmithFatal(t, mainC, gccOut, ccgoOut, cc, ccgo, build, run, ok, cs, gccT)
		}

		run++
		if bytes.Equal(gccOut, ccgoOut) {
			ok++
			if *oEdit {
				fmt.Printf("cc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v) %s\n", cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, time.Since(t0), extra)
			}
			continue
		}

		if *oNoCmp {
			continue
		}

		csmithFatal(t, mainC, gccOut, ccgoOut, cc, ccgo, build, run, ok, cs, gccT)
	}
	d := time.Since(t0)
	t.Logf("cc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v)", cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, d)
	if *oEdit {
		fmt.Printf("CSmith0\tcc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v)\n", cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, d)
	}
}

func csmithFatal(t *testing.T, mainC string, gccOut, ccgoOut []byte, cc, ccgo, build, run, ok, cs int, gccT time.Duration) {
	b, err := ioutil.ReadFile(mainC)
	if err != nil {
		t.Fatal(err)
	}

	b2, err := ioutil.ReadFile("main.go")
	if err != nil {
		b2 = nil
	}

	t.Fatalf(`
==== CSmith code ==============================================================
%s
==== Go code (if any ) ========================================================
%s
===============================================================================
 GCC   time: %v
 GCC output: %s
CCGO output: %s
cc %v ccgo %v build %v run %v ok %v (%.2f%%) csmith %v (%v)
`,
		b, b2, gccT, bytes.TrimSpace(gccOut), bytes.TrimSpace(ccgoOut),
		cc, ccgo, build, run, ok, 100*float64(ok)/float64(cs), cs, *oCSmith)
}
