// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"bytes"
	"flag"
	"fmt"
	"math"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"testing"

	"github.com/dustin/go-humanize"
	"modernc.org/ccorpus2"
)

var (
	cfs    = ccorpus2.FS
	goarch = runtime.GOARCH
	goos   = runtime.GOOS
	oTrace = flag.Bool("trc", false, "Print tested paths.")
	re     *regexp.Regexp
)

func TestMain(m *testing.M) {
	extendedErrors = true
	oRE := flag.String("re", "", "")
	flag.Parse()
	if *oRE != "" {
		re = regexp.MustCompile(*oRE)
	}
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

func walk(dir string, f func(pth string, fi os.FileInfo) error) error {
	fis, err := cfs.ReadDir(dir)
	if err != nil {
		return err
	}

	for _, v := range fis {
		switch {
		case v.IsDir():
			if err = walk(dir+"/"+v.Name(), f); err != nil {
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
	tmp := t.TempDir()
	blacklistCompCert := map[string]struct{}{}
	// blacklistGCC := map[string]struct{}{
	// 	// Assertions are deprecated, not supported.
	// 	"950919-1.c": {},
	// }
	blacklistTCC := map[string]struct{}{
		"76_dollars_in_identifiers.c": {},
	}
	switch fmt.Sprintf("%s/%s", runtime.GOOS, runtime.GOARCH) {
	case "linux/s390x":
		blacklistCompCert["aes.c"] = struct{}{} // Unsupported endianness.
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
			testCompile(t, tmp, "assets/"+v.dir, v.blacklist)
		})
	}
}

func testCompile(t *testing.T, tmp, dir string, blacklist map[string]struct{}) {
	return //TODO-
	p := newParallel()

	defer func() { p.close(t) }()

	p.err(walk(dir, func(pth string, fi os.FileInfo) error {
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
						err = fmt.Errorf("%v: PANIC: %v", apth, err)
						trc("%v: PANIC: %v\n%s", apth, err, debug.Stack())
						os.Exit(1)
					}
				}()

				ofn := fmt.Sprintf("__ccgo_%d.go", p.id())

				defer os.Remove(ofn)

				var out bytes.Buffer
				task := NewTask(goos, goarch, []string{"ccgo", "-o", ofn, "-c", apth}, &out, &out, cfs)
				ccgoErr := task.Main()
				if ccgoErr == nil {
					p.ok()
					return
				}

				f, err := cfs.Open(apth)
				if err != nil {
					p.err(err)
					return
				}

				defer f.Close()

				b := make([]byte, afi.Size())
				if n, _ := f.Read(b); int64(n) != afi.Size() {
					p.err(errorf("%v: short read", apth))
					return
				}

				fn := filepath.Join(tmp, filepath.Base(apth))
				if err := os.WriteFile(fn, b, 0660); err != nil {
					p.err(errorf("%v: %v", apth, err))
					return
				}

				defer os.Remove(fn)

				cfg := task.cfg
				cmd := exec.Command(cfg.CC, "-c", "-o", filepath.Join(tmp, ofn), fn)
				var buf bytes.Buffer
				cmd.Stderr = &buf
				if err := cmd.Run(); err != nil {
					t.Logf("%v: skip: %v: %s %v", apth, cfg.CC, buf.Bytes(), err)
					p.skip()
					return
				}

				p.fail()
				p.err(ccgoErr)
			}()
			return nil
		})
		return nil
	}))
}
