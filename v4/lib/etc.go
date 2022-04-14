// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"fmt"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"

	"modernc.org/cc/v4"
)

var (
	extendedErrors bool // true: Errors will include origin info.

	reservedNames = nameRegister{
		// Keywords
		"break":       {},
		"case":        {},
		"chan":        {},
		"const":       {},
		"continue":    {},
		"default":     {},
		"defer":       {},
		"else":        {},
		"fallthrough": {},
		"for":         {},
		"func":        {},
		"go":          {},
		"goto":        {},
		"if":          {},
		"import":      {},
		"interface":   {},
		"map":         {},
		"package":     {},
		"range":       {},
		"return":      {},
		"select":      {},
		"struct":      {},
		"switch":      {},
		"type":        {},
		"var":         {},

		// Predeclared identifiers
		"any":        {},
		"append":     {},
		"bool":       {},
		"byte":       {},
		"cap":        {},
		"close":      {},
		"comparable": {},
		"complex":    {},
		"complex128": {},
		"complex64":  {},
		"copy":       {},
		"delete":     {},
		"error":      {},
		"false":      {},
		"float32":    {},
		"float64":    {},
		"imag":       {},
		"int":        {},
		"int16":      {},
		"int32":      {},
		"int64":      {},
		"int8":       {},
		"iota":       {},
		"len":        {},
		"make":       {},
		"new":        {},
		"nil":        {},
		"panic":      {},
		"print":      {},
		"println":    {},
		"real":       {},
		"recover":    {},
		"rune":       {},
		"string":     {},
		"true":       {},
		"uint":       {},
		"uint16":     {},
		"uint32":     {},
		"uint64":     {},
		"uint8":      {},
		"uintptr":    {},
	}
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
		return fmt.Errorf("%s (%v: %v)", s, origin(3), origin(2))
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

type dict map[string]string

func (d *dict) put(k, v string) {
	if *d == nil {
		*d = map[string]string{}
	}
	(*d)[k] = v
}

type nameSpace struct {
	reg  nameRegister
	dict dict
}

func (n *nameSpace) registerNameSet(l *linker, set nameSet, tld bool) {
	var linkNames []string
	for nm := range set {
		linkNames = append(linkNames, nm)
	}

	sort.Slice(linkNames, func(i, j int) bool {
		return symKind(linkNames[i]) < symKind(linkNames[j]) || linkNames[i] < linkNames[j]
	})
	for _, linkName := range linkNames {
		switch k := symKind(linkName); k {
		case external:
			if !tld {
				break
			}

			n.registerName(l, linkName)
		case staticNone:
			if tld {
				panic(todo("", linkName))
			}

			n.dict.put(linkName, l.tld.registerName(l, linkName))
		case automatic, ccgoAutomatic:
			if tld {
				panic(todo("", linkName))
			}

			if _, ok := n.dict[linkName]; !ok {
				n.registerName(l, linkName)
			}
		case preserve:
			// nop
		case field:
			if _, ok := l.fields.dict[linkName]; !ok {
				l.fields.registerName(l, linkName)
			}
		case typename, taggedEum, taggedStruct, taggedUnion:
			// nop
		default:
			if k >= 0 {
				panic(todo("%q %v", linkName, symKind(linkName)))
			}
		}
	}
}

func (n *nameSpace) registerName(l *linker, linkName string) (goName string) {
	goName = l.goName(linkName)
	goName = n.reg.put(goName)
	n.dict.put(linkName, goName)
	return goName
}

type nameRegister map[string]struct{}

// Colliding names will be adjusted by adding a numeric suffix.
//TODO quadratic when repeatedly adding the same name!
func (n *nameRegister) put(nm string) (r string) {
	// defer func(mn string) { trc("%q -> %q", nm, r) }(nm)
	if *n == nil {
		*n = map[string]struct{}{}
	}
	m := *n
	if !reservedNames.has(nm) && !m.has(nm) {
		m[nm] = struct{}{}
		return nm
	}

	l := 0
	for i := len(nm) - 1; i > 0; i-- {
		if c := nm[i]; c < '0' || c > '9' {
			break
		}

		l++
	}
	num := 0
	if l != 0 {
		if n, err := strconv.Atoi(nm[:len(nm)-l]); err == nil {
			num = n
		}
	}
	for num++; ; num++ {
		s2 := fmt.Sprintf("%s%d", nm, num)
		if _, ok := m[s2]; !ok {
			m[s2] = struct{}{}
			return s2
		}
	}
}

func (n *nameRegister) has(nm string) bool { _, ok := (*n)[nm]; return ok }

type nameSet map[string]struct{}

func (n *nameSet) add(s string) (ok bool) {
	if *n == nil {
		*n = map[string]struct{}{s: {}}
		return true
	}

	m := *n
	if _, ok = m[s]; ok {
		return false
	}

	m[s] = struct{}{}
	return true
}

func symKind(s string) name {
	for i, v := range tags {
		if strings.HasPrefix(s, v) {
			return name(i)
		}
	}
	return -1
}

func visit(n cc.Node, visitor func(cc cc.Node) bool) bool {
	if n == nil {
		return true
	}

	if x, ok := n.(cc.Token); ok {
		return visitor(x)
	}

	if !visitor(n) {
		return true
	}

	typ := reflect.TypeOf(n)
	val := reflect.ValueOf(n)
	var zero reflect.Value
	if typ.Kind() == reflect.Pointer {
		typ = typ.Elem()
		val = val.Elem()
		if val == zero {
			return true
		}
	}

	if typ.Kind() != reflect.Struct {
		return true
	}

	nf := typ.NumField()
	for i := 0; i < nf; i++ {
		f := typ.Field(i)
		if !f.IsExported() {
			continue
		}

		if strings.HasPrefix(f.Name, "Token") {
			if x, ok := val.Field(i).Interface().(cc.Token); ok {
				if !visitor(x) {
					return true
				}
			}
			continue
		}

		if val == zero || val.IsZero() {
			continue
		}

		if x, ok := val.Field(i).Interface().(cc.Node); ok {
			if !visit(x, visitor) {
				return true
			}
		}
	}
	return true
}

func binary(s string) string {
	switch runtime.GOOS {
	case "windows":
		if !strings.HasSuffix(s, ".exe") {
			return s + ".exe"
		}
	default:
		if strings.HasSuffix(s, ".exe") {
			return s[:len(s)-len(".exe")]
		}
	}
	return s
}
