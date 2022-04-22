// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

//TODO CSmith

import (
	"bytes"
	"context"
	"encoding/hex"
	"flag"
	"fmt"
	"io/ioutil"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/dustin/go-humanize"
	"github.com/pmezard/go-difflib/difflib"
	"modernc.org/cc/v4"
	"modernc.org/ccorpus2"
)

var (
	oTrace = flag.Bool("trc", false, "Print tested paths.")

	cfs    = ccorpus2.FS
	goarch = runtime.GOARCH
	goos   = runtime.GOOS
	re     *regexp.Regexp
	hostCC string
)

func stack() []byte { return debug.Stack() }

func TestMain(m *testing.M) {
	extendedErrors = true
	oRE := flag.String("re", "", "")
	flag.Parse()
	if *oRE != "" {
		re = regexp.MustCompile(*oRE)
	}
	cfg, err := cc.NewConfig(runtime.GOOS, runtime.GOARCH)
	if err != nil {
		panic(err)
	}

	hostCC = cfg.CC
	os.Exit(m.Run())
}

func (p *parallel) close(t *testing.T) {
	p.wg.Wait()
	p.Lock()
	for _, v := range p.errors {
		t.Error(v)
	}
	p.Unlock()
	t.Logf("TOTAL: files %v, skip %v, ok %v, fails %v", h(p.files), h(p.skips), h(p.oks), h(p.fails))
}

func h(v interface{}) string {
	switch x := v.(type) {
	case int32:
		return humanize.Comma(int64(x))
	case int64:
		return humanize.Comma(x)
	case uint64:
		if x <= math.MaxInt64 {
			return humanize.Comma(int64(x))
		}
	}
	return fmt.Sprint(v)
}

func cfsWalk(dir string, f func(pth string, fi os.FileInfo) error) error {
	fis, err := cfs.ReadDir(dir)
	if err != nil {
		return err
	}

	for _, v := range fis {
		switch {
		case v.IsDir():
			if err = cfsWalk(dir+"/"+v.Name(), f); err != nil {
				return err
			}
		default:
			fi, err := v.Info()
			if err != nil {
				return err
			}

			if err = f(dir+"/"+v.Name(), fi); err != nil {
				return err
			}
		}
	}
	return nil
}

func TestCompile(t *testing.T) {
	g := newGolden(t, fmt.Sprintf("testdata/test_compile_%s_%s.golden", runtime.GOOS, runtime.GOARCH))

	defer g.close()

	tmp := t.TempDir()
	blacklistCompCert := map[string]struct{}{}
	// blacklistGCC := map[string]struct{}{
	// 	// Assertions are deprecated, not supported.
	// 	"950919-1.c": {},
	// }
	blacklistTCC := map[string]struct{}{
		// panics
		"75_array_in_struct_init.c": {}, //TODO

		"45_empty_for.c":              {}, //TODO
		"46_grep.c":                   {}, //TODO
		"52_unnamed_enum.c":           {}, //TODO
		"54_goto.c":                   {}, //TODO
		"55_lshift_type.c":            {}, //TODO
		"73_arm64.c":                  {}, //TODO
		"76_dollars_in_identifiers.c": {}, //TODO
		"78_vla_label.c":              {}, //TODO
		"79_vla_continue.c":           {}, //TODO
		"80_flexarray.c":              {}, //TODO
		"81_types.c":                  {}, //TODO
		"86_memory-model.c":           {}, //TODO
		"87_dead_code.c":              {}, //TODO
		"88_codeopt.c":                {}, //TODO
		"89_nocode_wanted.c":          {}, //TODO
		"90_struct-init.c":            {}, //TODO
		"91_ptr_longlong_arith32.c":   {}, //TODO
		"92_enum_bitfield.c":          {}, //TODO
		"93_integer_promotion.c":      {}, //TODO
		"94_generic.c":                {}, //TODO
		"95_bitfields.c":              {}, //TODO
		"95_bitfields_ms.c":           {}, //TODO
		"97_utf8_string_literal.c":    {}, //TODO
	}
	switch fmt.Sprintf("%s/%s", runtime.GOOS, runtime.GOARCH) {
	case "darwin/amd64":
		blacklistTCC["22_floating_point.c"] = struct{}{} //TODO
		blacklistTCC["24_math_library.c"] = struct{}{}   //TODO
	case "linux/386":
		blacklistTCC["22_floating_point.c"] = struct{}{} //TODO
		blacklistTCC["24_math_library.c"] = struct{}{}   //TODO
		blacklistTCC["99_fastcall.c"] = struct{}{}       //TODO
	case "linux/s390x":
		blacklistCompCert["aes.c"] = struct{}{} // Unsupported endianness.
	case "netbsd/amd64":
		blacklistTCC["98_al_ax_extend.c"] = struct{}{} //TODO
	case "windows/386":
		blacklistTCC["29_array_address.c"] = struct{}{} //TODO
	case "windows/amd64":
		blacklistTCC["29_array_address.c"] = struct{}{} //TODO
	case "windows/arm64":
		blacklistTCC["29_array_address.c"] = struct{}{} //TODO
	}
	for _, v := range []struct {
		dir       string
		blacklist map[string]struct{}
	}{
		//TODO {"CompCert-3.6/test/c", blacklistCompCert},
		//TODO {"ccgo", nil},
		//TODO {"gcc-9.1.0/gcc/testsuite/gcc.c-torture", blacklistGCC},
		//TODO {"github.com/AbsInt/CompCert/test/c", blacklistCompCert},
		//TODO {"github.com/cxgo", nil},
		//TODO {"github.com/gcc-mirror/gcc/gcc/testsuite", blacklistGCC},
		//TODO {"github.com/vnmakarov", nil},
		//TODO {"sqlite-amalgamation-3380100", nil},
		{"tcc-0.9.27/tests/tests2", blacklistTCC},
		//TODO {"benchmarksgame-team.pages.debian.net", nil},
	} {
		t.Run(v.dir, func(t *testing.T) {
			testCompile(t, tmp, "assets/"+v.dir, v.blacklist, g)
		})
	}
}

func testCompile(t *testing.T, tmp, dir string, blacklist map[string]struct{}, g *golden) {
	p := newParallel()

	defer func() { p.close(t) }()

	p.err(cfsWalk(dir, func(pth string, fi os.FileInfo) error {
		if fi.IsDir() {
			return nil
		}

		if filepath.Ext(pth) != ".c" {
			return nil
		}

		p.file()
		switch {
		case re != nil:
			if !re.MatchString(pth) {
				p.skip()
				return nil
			}
		default:
			if _, ok := blacklist[filepath.Base(pth)]; ok {
				p.skip()
				return nil
			}
		}

		apth := pth
		afi := fi
		p.exec(func() error {
			if *oTrace {
				fmt.Fprintln(os.Stderr, apth)
			}

			func() {
				defer func() {
					if err := recover(); err != nil {
						err = fmt.Errorf("%v: PANIC: %v", filepath.Base(apth), err)
						trc("%v: PANIC: %v\n%s", apth, err, debug.Stack())
						os.Exit(1)
					}
				}()

				ofn := filepath.Join(tmp, fmt.Sprintf("%d.go", p.id()))

				defer os.Remove(ofn)

				var out bytes.Buffer
				task := NewTask(goos, goarch, []string{"ccgo", "-o", ofn, "-c", apth}, &out, &out, cfs)
				ccgoErr := task.Main()
				if ccgoErr == nil {
					p.ok()
					g.w("%s\n", apth)
					return
				}

				checkFailOk(t, p, errorf("%v: %v", filepath.Base(apth), ccgoErr), tmp, apth, ofn, afi, task)
			}()
			return nil
		})
		return nil
	}))
}

func checkFailOk(t *testing.T, p *parallel, ccgoErr error, tmp, src, ofn string, fi os.FileInfo, task *Task) {
	f, err := cfs.Open(src)
	if err != nil {
		p.err(err)
		return
	}

	defer f.Close()

	b := make([]byte, fi.Size())
	if n, _ := f.Read(b); int64(n) != fi.Size() {
		p.err(errorf("%v: short read", src))
		return
	}

	fn := filepath.Join(tmp, filepath.Base(src))
	if err := os.WriteFile(fn, b, 0660); err != nil {
		p.err(errorf("%v: %v", src, err))
		return
	}

	defer os.Remove(fn)

	cfg := task.cfg
	cmd := exec.Command(cfg.CC, "-c", "-o", ofn, fn)
	var buf bytes.Buffer
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		t.Logf("%v: skip: %v: %s %v", src, cfg.CC, buf.Bytes(), err)
		p.skip()
		return
	}

	p.fail()
	p.err(ccgoErr)
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

	if err = os.Chdir(filepath.FromSlash(dir)); err != nil {
		return err
	}

	return f()
}

func absCwd() (string, error) {
	wd, err := os.Getwd()
	if err != nil {
		return "", err
	}

	if wd, err = filepath.Abs(wd); err != nil {
		return "", err
	}

	return wd, nil
}

type echoWriter struct {
	w      bytes.Buffer
	silent bool
}

func (w *echoWriter) Write(b []byte) (int, error) {
	if !w.silent {
		os.Stderr.Write(b)
	}
	return w.w.Write(b)
}

func shell(echo bool, cmd string, args ...string) ([]byte, error) {
	cmd, err := exec.LookPath(cmd)
	if err != nil {
		return nil, err
	}

	wd, err := absCwd()
	if err != nil {
		return nil, err
	}

	if echo {
		fmt.Printf("execute %s %q in %s\n", cmd, args, wd)
	}
	var b echoWriter
	b.silent = !echo
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()
	c := exec.CommandContext(ctx, cmd, args...)
	c.Stdout = &b
	c.Stderr = &b
	err = c.Run()
	return b.w.Bytes(), err
}

func TestExec(t *testing.T) {
	g := newGolden(t, fmt.Sprintf("testdata/test_exec_%s_%s.golden", runtime.GOOS, runtime.GOARCH))

	defer g.close()

	tmp := t.TempDir()
	if err := inDir(tmp, func() error {
		if out, err := shell(true, "go", "mod", "init", "test"); err != nil {
			return fmt.Errorf("%s\vFAIL: %v", out, err)
		}

		if out, err := shell(true, "go", "get", "modernc.org/libc"); err != nil {
			return fmt.Errorf("%s\vFAIL: %v", out, err)
		}

		blacklistCompCert := map[string]struct{}{}
		// blacklistGCC := map[string]struct{}{
		// 	// Assertions are deprecated, not supported.
		// 	"950919-1.c": {},
		// }
		blacklistTCC := map[string]struct{}{
			// panics
			"75_array_in_struct_init.c": {}, //TODO
			"92_enum_bitfield.c":        {}, //TODO

			"45_empty_for.c":              {}, //TODO
			"50_logical_second_arg.c":     {}, //TODO
			"52_unnamed_enum.c":           {}, //TODO
			"54_goto.c":                   {}, //TODO
			"55_lshift_type.c":            {}, //TODO
			"64_macro_nesting.c":          {}, //TODO
			"67_macro_concat.c":           {}, //TODO
			"71_macro_empty_arg.c":        {}, //TODO
			"73_arm64.c":                  {}, //TODO
			"76_dollars_in_identifiers.c": {}, //TODO
			"77_push_pop_macro.c":         {}, //TODO
			"78_vla_label.c":              {}, //TODO
			"79_vla_continue.c":           {}, //TODO
			"80_flexarray.c":              {}, //TODO
			"81_types.c":                  {}, //TODO
			"84_hex-float.c":              {}, //TODO
			"85_asm-outside-function.c":   {}, //TODO
			"86_memory-model.c":           {}, //TODO
			"87_dead_code.c":              {}, //TODO
			"88_codeopt.c":                {}, //TODO
			"89_nocode_wanted.c":          {}, //TODO
			"90_struct-init.c":            {}, //TODO
			"91_ptr_longlong_arith32.c":   {}, //TODO
			"93_integer_promotion.c":      {}, //TODO
			"94_generic.c":                {}, //TODO
			"97_utf8_string_literal.c":    {}, //TODO
			"98_al_ax_extend.c":           {}, //TODO
		}
		switch fmt.Sprintf("%s/%s", runtime.GOOS, runtime.GOARCH) {
		case "darwin/amd64":
			blacklistTCC["22_floating_point.c"] = struct{}{} //TODO
			blacklistTCC["24_math_library.c"] = struct{}{}   //TODO
			blacklistTCC["28_strings.c"] = struct{}{}        //TODO
			blacklistTCC["37_sprintf.c"] = struct{}{}        //TODO
		case "linux/386":
			blacklistTCC["22_floating_point.c"] = struct{}{} //TODO
			blacklistTCC["24_math_library.c"] = struct{}{}   //TODO
		case "linux/s390x":
			blacklistCompCert["aes.c"] = struct{}{} // Unsupported endianness.
		case "windows/386":
			blacklistTCC["29_array_address.c"] = struct{}{} //TODO
		case "windows/amd64":
			blacklistTCC["29_array_address.c"] = struct{}{} //TODO
		case "windows/arm64":
			blacklistTCC["29_array_address.c"] = struct{}{} //TODO
		}
		for _, v := range []struct {
			dir       string
			blacklist map[string]struct{}
		}{
			//TODO {"CompCert-3.6/test/c", blacklistCompCert},
			//TODO {"ccgo", nil},
			//TODO {"gcc-9.1.0/gcc/testsuite/gcc.c-torture", blacklistGCC},
			//TODO {"github.com/AbsInt/CompCert/test/c", blacklistCompCert},
			//TODO {"github.com/cxgo", nil},
			//TODO {"github.com/gcc-mirror/gcc/gcc/testsuite", blacklistGCC},
			//TODO {"github.com/vnmakarov", nil},
			//TODO {"sqlite-amalgamation-3380100", nil},
			{"tcc-0.9.27/tests/tests2", blacklistTCC},
			//TODO {"benchmarksgame-team.pages.debian.net", nil},
		} {
			t.Run(v.dir, func(t *testing.T) {
				testExec(t, "assets/"+v.dir, v.blacklist, g)
			})
		}

		return nil
	}); err != nil {
		t.Fatal(err)
	}
}

func testExec(t *testing.T, dir string, blacklist map[string]struct{}, g *golden) {
	p := newParallel()

	defer func() { p.close(t) }()

	p.err(cfsWalk(dir, func(pth string, fi os.FileInfo) error {
		if fi.IsDir() {
			return nil
		}

		if filepath.Ext(pth) != ".c" {
			return nil
		}

		p.file()
		switch {
		case re != nil:
			if !re.MatchString(pth) {
				p.skip()
				return nil
			}
		default:
			if _, ok := blacklist[filepath.Base(pth)]; ok {
				p.skip()
				return nil
			}
		}

		apth := pth
		p.exec(func() error {
			if *oTrace {
				fmt.Fprintln(os.Stderr, apth)
			}

			func() {
				defer func() {
					if err := recover(); err != nil {
						err = fmt.Errorf("%v: PANIC: %v", filepath.Base(apth), err)
						trc("%v: PANIC: %v\n%s", apth, err, debug.Stack())
						os.Exit(1)
					}
				}()

				id := p.id()
				b, err := getCorpusFile(apth)
				if err != nil {
					p.err(errorf("", err))
					p.fail()
					return
				}

				cfn := fmt.Sprintf("%d.c", id)
				if err := os.WriteFile(cfn, b, 0660); err != nil {
					p.err(errorf("", err))
					p.fail()
					return
				}

				ofn := fmt.Sprintf("%d", id)
				if _, err := shell(false, hostCC, "-o", binary(ofn), cfn); err != nil {
					p.skip()
					return
				}

				defer os.Remove(ofn)

				cOut, err := shell(false, "./"+binary(ofn))
				if err != nil {
					p.skip()
					return
				}

				ofn += ".go"

				defer os.Remove(ofn)

				var out bytes.Buffer
				if err := NewTask(goos, goarch, []string{"ccgo", "-o", binary(ofn), apth}, &out, &out, cfs).Main(); err != nil {
					p.err(errorf("%s: %s: FAIL: %v", filepath.Base(apth), out.Bytes(), err))
					p.fail()
					return
				}

				goOut, err := exec.Command("go", "run", ofn).CombinedOutput()
				if err != nil {
					p.err(errorf("%s: %s: FAIL: %v", filepath.Base(apth), goOut, err))
					p.fail()
					return
				}

				if bytes.Equal(cOut, goOut) {
					p.ok()
					g.w("%s\n", apth)
					return
				}

				cOut = bytes.TrimSpace(cOut)
				goOut = bytes.TrimSpace(goOut)
				if bytes.Equal(cOut, goOut) {
					p.ok()
					return
				}

				if bytes.Contains(cOut, []byte("\r\n")) {
					cOut = bytes.ReplaceAll(cOut, []byte("\r\n"), []byte{'\n'})
				}
				if bytes.Contains(goOut, []byte("\r\n")) {
					goOut = bytes.ReplaceAll(goOut, []byte("\r\n"), []byte{'\n'})
				}
				if bytes.Equal(cOut, goOut) {
					p.ok()
					return
				}

				diff := difflib.UnifiedDiff{
					A:        difflib.SplitLines(string(cOut)),
					B:        difflib.SplitLines(string(goOut)),
					FromFile: "expected",
					ToFile:   "got",
					Context:  0,
				}
				s, _ := difflib.GetUnifiedDiffString(diff)
				t.Errorf("%v:\n%v\n--- expexted\n%s\n\n--- got\n%s\n\n--- expected\n%s\n--- got\n%s", filepath.Base(apth), s, cOut, goOut, hex.Dump(cOut), hex.Dump(goOut))
				p.fail()
			}()
			return nil
		})
		return nil
	}))
}

type golden struct {
	a  []string
	f  *os.File
	mu sync.Mutex
	t  *testing.T

	discard bool
}

func newGolden(t *testing.T, fn string) *golden {
	if re != nil {
		return &golden{discard: true}
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

	return &golden{t: t, f: f}
}

func (g *golden) w(s string, args ...interface{}) {
	if g.discard {
		return
	}

	g.mu.Lock()

	defer g.mu.Unlock()

	if s = strings.TrimRight(s, " \t\n\r"); !strings.HasSuffix(s, "\n") {
		s += "\n"
	}
	g.a = append(g.a, fmt.Sprintf(s, args...))
}

func (g *golden) close() {
	if g.discard || g.f == nil {
		return
	}

	defer func() { g.f = nil }()

	sort.Strings(g.a)
	if _, err := g.f.WriteString(strings.Join(g.a, "")); err != nil {
		g.t.Fatal(err)
	}

	if err := g.f.Sync(); err != nil {
		g.t.Fatal(err)
	}

	if err := g.f.Close(); err != nil {
		g.t.Fatal(err)
	}
}

func getCorpusFile(path string) ([]byte, error) {
	f, err := cfs.Open(path)
	if err != nil {
		return nil, err
	}

	return ioutil.ReadAll(f)
}
