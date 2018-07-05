// Copyright 2017 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo

import (
	"bufio"
	"bytes"
	"compress/gzip"
	"fmt"
	"go/scanner"
	"go/token"
	"io"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/cznic/cc/v2"
)

/*

var <mangled name> = Lb(n)		allocate BSS(n)
const Lp<mangled name> = "type"		function prototype
const Lh<helper><num> = "args"		helper function

*/

const (
	objVersion = 1
)

var (
	objMagic = []byte{0xc6, 0x1f, 0xd0, 0xb5, 0xc4, 0x39, 0xad, 0x56}
)

func objWrite(out io.Writer, goos, goarch string, binaryVersion uint64, magic []byte, in io.Reader) (err error) {
	w := gzip.NewWriter(out)
	w.Header.Comment = "ccgo object file"
	var buf bytes.Buffer
	buf.Write(magic)
	fmt.Fprintf(&buf, "%s|%s|%v", goos, goarch, binaryVersion)
	w.Header.Extra = buf.Bytes()
	w.Header.ModTime = time.Now()
	w.Header.OS = 255 // Unknown OS.
	if _, err := io.Copy(w, in); err != nil {
		return err
	}

	return w.Close()
}

func objRead(out io.Writer, goos, goarch string, binaryVersion uint64, magic []byte, in io.Reader) (err error) {
	r, err := gzip.NewReader(in)
	if err != nil {
		return err
	}

	if len(r.Header.Extra) < len(magic) || !bytes.Equal(r.Header.Extra[:len(magic)], magic) {
		return fmt.Errorf("unrecognized file format")
	}

	buf := r.Header.Extra[len(magic):]
	a := bytes.Split(buf, []byte{'|'})
	if len(a) != 3 {
		return fmt.Errorf("corrupted file")
	}

	if s := string(a[0]); s != goos {
		return fmt.Errorf("invalid platform %q", s)
	}

	if s := string(a[1]); s != goarch {
		return fmt.Errorf("invalid architecture %q", s)
	}

	v, err := strconv.ParseUint(string(a[2]), 10, 64)
	if err != nil {
		return err
	}

	if v != binaryVersion {
		return fmt.Errorf("invalid version number %v", v)
	}

	if _, err := io.Copy(out, r); err != nil {
		return err
	}

	return r.Close()
}

// Object writes a linker object file produced from in to out.
func Object(out io.Writer, goos, goarch string, in *cc.TranslationUnit) (err error) {
	returned := false

	defer func() {
		e := recover()
		if !returned && err == nil {
			err = fmt.Errorf("PANIC: %v\n%s", e, debugStack())
		}
	}()

	defer func() {
		if x, ok := out.(*bufio.Writer); ok {
			if e := x.Flush(); e != nil && err == nil {
				err = e
			}
		}

	}()

	var buf bytes.Buffer
	g := newNGen(&buf, in)
	if err := g.gen(); err != nil {
		returned = true
		return err
	}

	err = objWrite(out, goos, goarch, objVersion, objMagic, &buf)
	returned = true
	return err
}

// Linker produces Go files from object files.
type Linker struct {
	errMu  sync.Mutex
	errs   scanner.ErrorList
	fn     string
	goarch string
	goos   string
	out    io.Writer
	tld    []string
}

// NewLinker returns a newly created Linker writing to out.
func NewLinker(out io.Writer, goos, goarch string) *Linker {
	return &Linker{
		goarch: goarch,
		goos:   goos,
		out:    out,
	}
}

func (l *Linker) err(msg string, args ...interface{}) {
	l.errMu.Lock()
	l.errs.Add(token.Position{}, fmt.Sprintf(msg, args...))
	l.errMu.Unlock()
}

// Link incerementaly links objects files.
func (l *Linker) Link(fn string, obj io.Reader) (err error) {
	returned := false

	defer func() {
		e := recover()
		if !returned && err == nil {
			err = fmt.Errorf("PANIC: %v\n%s", e, debugStack())
		}
	}()

	l.fn = fn
	l.link(obj)
	if len(l.errs) != 0 {
		err = l.errs
	}

	returned = true
	return err
}

// Close finihes the linking.
func (l *Linker) Close() (err error) {
	returned := false

	defer func() {
		e := recover()
		if !returned && err == nil {
			err = fmt.Errorf("PANIC: %v\n%s", e, debugStack())
		}
	}()

	defer func() {
		if x, ok := l.out.(*bufio.Writer); ok {
			if e := x.Flush(); e != nil && err == nil {
				err = e
			}
		}

	}()

	panic("TODO")
	returned = true
	return err
}

func (l *Linker) link(obj io.Reader) {
	r, w := io.Pipe()

	go func() {
		defer func() {
			if err := w.Close(); err != nil {
				l.err(err.Error())
			}
		}()

		if err := objRead(w, l.goos, l.goarch, objVersion, objMagic, obj); err != nil {
			l.err("%v", err)
		}
	}()

	sc := bufio.NewScanner(r)

	const (
		skipBlank = iota
		collectComments
		copy
		copyFunc
	)

	state := skipBlank
	for sc.Scan() {
		s := sc.Text()
		switch state {
		case skipBlank:
			if len(s) == 0 {
				break
			}

			l.tld = l.tld[:0]
			state = collectComments
			fallthrough
		case collectComments:
			l.tld = append(l.tld, s)
			if strings.HasPrefix(s, "//") {
				break
			}

			switch {
			case strings.HasPrefix(s, "const L"):
				l.lConst(s)
				state = skipBlank
			case strings.HasPrefix(s, "var"):
				state = copy
			case strings.HasPrefix(s, "func"):
				if strings.HasSuffix(s, "}") {
					l.emit(w)
					state = skipBlank
					break
				}

				state = copyFunc
			default:
				panic(fmt.Sprintf("%q", s))
			}
		case copy:
			if len(s) == 0 {
				l.emit(w)
				state = skipBlank
				break
			}
		case copyFunc:
			l.tld = append(l.tld, s)
			if s == "}" {
				l.emit(w)
				state = skipBlank
			}
		default:
			panic(state)
		}
	}
	if err := sc.Err(); err != nil {
		l.err(err.Error())
	}
}

func (l *Linker) emit(w *io.PipeWriter) (err error) {
	if _, err = fmt.Fprintln(l.out); err != nil {
		l.err(err.Error())
		w.Close()
		return err
	}

	for _, v := range l.tld {
		if _, err = fmt.Fprintln(l.out, v); err != nil {
			l.err(err.Error())
			w.Close()
			return err
		}
	}

	l.tld = l.tld[:0]
	return nil
}

func (l *Linker) lConst(s string) {
	panic("TODO")
}