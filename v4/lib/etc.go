// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"go/token"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
)

var (
	extendedErrors bool // true: Errors will include origin info.
)

// origin returns caller's short position, skipping skip frames.
func origin(skip int) string {
	pc, fn, fl, _ := runtime.Caller(skip)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
		if strings.HasPrefix(fns, "func") {
			num := true
			for _, c := range fns[len("func"):] {
				if c < '0' || c > '9' {
					num = false
					break
				}
			}
			if num {
				return origin(skip + 2)
			}
		}
	}
	return fmt.Sprintf("%s:%d:%s", filepath.Base(fn), fl, fns)
}

// todo prints and return caller's position and an optional message tagged with TODO. Output goes to stderr.
func todo(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	r := fmt.Sprintf("%s\n\tTODO %s", origin(2), s)
	// fmt.Fprintf(os.Stderr, "%s\n", r)
	// os.Stdout.Sync()
	return r
}

// trc prints and return caller's position and an optional message tagged with TRC. Output goes to stderr.
func trc(s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	r := fmt.Sprintf("%s: TRC %s", origin(2), s)
	fmt.Fprintf(os.Stderr, "%s\n", r)
	os.Stderr.Sync()
	return r
}

type errors []string

// Error implements error.
func (e errors) Error() string { return strings.Join(e, "\n") }

func (e *errors) add(err error) { *e = append(*e, err.Error()) }

func (e errors) err() error {
	w := 0
	for i, v := range e {
		if i != 0 {
			if prev, ok := extractPos(e[i-1]); ok {
				if cur, ok := extractPos(v); ok && prev.Filename == cur.Filename && prev.Line == cur.Line {
					continue
				}
			}
		}
		e[w] = v
		w++
	}
	e = e[:w]
	if len(e) == 0 {
		return nil
	}

	return e
}

// errorf constructs and error value. If extendedErrors is true, the error will
// continue its origin.
func errorf(s string, args ...interface{}) error {
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	switch {
	case extendedErrors:
		return fmt.Errorf("%s (%v:)", s, origin(2))
	default:
		return fmt.Errorf("%s", s)
	}
}

type parallel struct {
	errors errors
	limit  chan struct{}
	sync.Mutex
	wg sync.WaitGroup

	fails int32
	files int32
	ids   int32
	oks   int32
	skips int32
}

func newParallel() *parallel {
	return &parallel{
		limit: make(chan struct{}, runtime.GOMAXPROCS(0)),
	}
}

func (p *parallel) eh(msg string, args ...interface{}) { p.err(fmt.Errorf(msg, args...)) }

func (p *parallel) fail()   { atomic.AddInt32(&p.fails, 1) }
func (p *parallel) file()   { atomic.AddInt32(&p.files, 1) }
func (p *parallel) id() int { return int(atomic.AddInt32(&p.ids, 1)) }
func (p *parallel) ok()     { atomic.AddInt32(&p.oks, 1) }
func (p *parallel) skip()   { atomic.AddInt32(&p.skips, 1) }

func (p *parallel) exec(run func() error) {
	p.limit <- struct{}{}
	p.wg.Add(1)

	go func() {
		defer func() {
			p.wg.Done()
			<-p.limit
		}()

		p.err(run())
	}()
}

func (p *parallel) wait() error {
	p.wg.Wait()
	return p.errors.err()
}

func (p *parallel) err(err error) {
	if err == nil {
		return
	}

	p.Lock()
	p.errors.add(err)
	p.Unlock()
}

func extractPos(s string) (p token.Position, ok bool) {
	var prefix string
	if len(s) > 1 && s[1] == ':' { // c:\foo
		prefix = s[:2]
		s = s[2:]
	}
	// "testdata/parser/bug/001.c:1193:6: ..."
	a := strings.SplitN(s, ":", 4)
	// ["testdata/parser/bug/001.c" "1193" "6" "..."]
	if len(a) < 3 {
		return p, false
	}

	line, err := strconv.Atoi(a[1])
	if err != nil {
		return p, false
	}

	col, err := strconv.Atoi(a[2])
	if err != nil {
		return p, false
	}

	return token.Position{Filename: prefix + a[0], Line: line, Column: col}, true
}

func buildDefs(D, U []string) string {
	var a []string
	for _, v := range D {
		if i := strings.IndexByte(v, '='); i > 0 {
			a = append(a, fmt.Sprintf("#define %s %s", v[:i], v[i+1:]))
			continue
		}

		a = append(a, fmt.Sprintf("#define %s 1", v))
	}
	for _, v := range U {
		a = append(a, fmt.Sprintf("#undef %s", v))
	}
	return strings.Join(a, "\n")
}

type ns map[string]struct{}

func (n *ns) take(s string) string {
	if *n == nil {
		*n = map[string]struct{}{}
	}
	m := *n
	if _, ok := m[s]; !ok {
		m[s] = struct{}{}
		return s
	}

	l := 0
	for i := len(s) - 1; i > 0; i++ {
		if c := s[i]; c < '0' || c > '9' {
			break
		}

		l++
	}
	num := 0
	if l != 0 {
		if n, err := strconv.Atoi(s[:len(s)-l]); err == nil {
			num = n
		}
	}
	for num++; ; num++ {
		s2 := fmt.Sprintf("%s%d", s, num)
		if _, ok := m[s2]; !ok {
			m[s2] = struct{}{}
			return s2
		}
	}
}
