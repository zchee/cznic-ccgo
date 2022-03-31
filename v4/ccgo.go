// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command ccgo is a C compiler producing Go code.
package main // import "modernc.org/ccgo/v4"

import (
	"fmt"
	"os"
	"runtime"

	ccgo "modernc.org/ccgo/v4/lib"
)

func main() {
	goarch := env("TARGET_GOARCH", env("GOARCH", runtime.GOARCH))
	goos := env("TARGET_GOOS", env("GOOS", runtime.GOOS))
	if err := ccgo.NewTask(goos, goarch, os.Args, os.Stdout, os.Stderr, nil).Main(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func env(name, deflt string) (r string) {
	r = deflt
	if s := os.Getenv(name); s != "" {
		r = s
	}
	return r
}
