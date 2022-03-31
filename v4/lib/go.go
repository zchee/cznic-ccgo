// Copyright 2022 The CCGO Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package ccgo // import "modernc.org/ccgo/v4/lib"

import (
	"modernc.org/cc/v4"
)

// errHandler is a function called on error.
type errHandler func(msg string, args ...interface{})

type ctx struct {
	cfg  *cc.Config
	eh   errHandler
	task *Task
}

func newCtx(task *Task, eh errHandler) *ctx {
	return &ctx{
		cfg:  task.cfg,
		eh:   eh,
		task: task,
	}
}

func (c *ctx) err(err error) { c.eh(err.Error()) }

func (c *ctx) compile(ifn, ofn string) error {
	sources := []cc.Source{
		{Name: "<predefined>", Value: c.cfg.Predefined},
		{Name: "<builtin>", Value: builtin},
	}
	if c.task.defs != "" {
		sources = append(sources, cc.Source{Name: "<command-line>", Value: c.task.defs})
	}
	sources = append(sources, cc.Source{Name: ifn, FS: c.cfg.FS})
	ast, err := cc.Translate(c.cfg, sources)
	if err != nil {
		return err
	}

	_ = ast
	c.err(errorf("%v: TODO", ifn))
	return nil
}
