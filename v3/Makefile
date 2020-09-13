# Copyright 2019 The GOCC Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

.PHONY:	all bench clean cover cpu editor internalError later mem nuke todo edit devbench

grep=--include=*.go
ngrep='TODOOK\|internalError\|testdata'
host=$(shell go env GOOS)-$(shell go env GOARCH)
testlog=testdata/testlog-$(shell echo $$GOOS)-$(shell echo $$GOARCH)$(shell echo $$CCGO_TEST_SUFFIX)

all:
	touch log
	cp log log-0
	date
	go version 2>&1 | tee log
	uname -a | tee -a log
	./unconvert.sh
	gofmt -l -s -w *.go
	go install -v
	go test -i
	go test -v 2>&1 -timeout 24h | tee -a log
	go vet 2>&1 | grep -v $(ngrep) || true
	golint 2>&1 | grep -v $(ngrep) || true
	make todo
	misspell *.go
	staticcheck | grep -v 'lexer\.go' || true
	maligned || true
	grep -n 'files.*, ok' log
	grep -n --color=never 'FAIL\|PASS' log
	grep -n --color=always 'FAIL' log
	grep -n --color=always 'nil pointer' log
	grep -c 'exit status 1' log || true
	grep -c 'exit status 2' log || true
	LC_ALL=C date 2>&1 | tee -a log

test:
	go version | tee $(testlog)
	uname -a | tee -a $(testlog)
	go test -v -timeout 24h | tee -a $(testlog)
	grep -ni fail $(testlog) | tee -a $(testlog) || true
	LC_ALL=C date | tee -a $(testlog)
	grep -ni --color=always fail $(testlog) || true

test_linux_amd64:
	GOOS=linux GOARCH=amd64 make test

test_linux_386:
	GOOS=linux GOARCH=386 make test

test_linux_arm:
	GOOS=linux GOARCH=arm make test

test_linux_arm64:
	GOOS=linux GOARCH=arm64 make test

test_windows_amd64:
	GOOS=windows GOARCH=amd64 make test

devbench:
	date 2>&1 | tee log-devbench
	go test -timeout 24h -dev -run @ -bench . 2>&1 | tee -a log-devbench
	grep -n 'FAIL\|SKIP' log-devbench || true

bench:
	date 2>&1 | tee log-bench
	go test -timeout 24h -v -run '^[^E]' -bench . 2>&1 | tee -a log-bench
	grep -n 'FAIL\|SKIP' log-bench || true

clean:
	go clean
	rm -f *~ *.test *.out

cover:
	t=$(shell mktemp) ; go test -coverprofile $$t && go tool cover -html $$t && unlink $$t

cpu: clean
	go test -run @ -bench . -cpuprofile cpu.out
	go tool pprof -lines *.test cpu.out

edit:
	touch log
	gvim -p Makefile *.go &

editor:
	gofmt -l -s -w *.go
	go install -v

later:
	@grep -n $(grep) LATER * || true
	@grep -n $(grep) MAYBE * || true

mem: clean
	# go test -run @ -dev -bench . -memprofile mem.out -timeout 24h
	go test -run TestSQLiteGo -memprofile mem.out -timeout 24h
	go tool pprof -lines -web -alloc_space *.test mem.out

nuke: clean
	go clean -i

todo:
	@grep -nr $(grep) ^[[:space:]]*_[[:space:]]*=[[:space:]][[:alpha:]][[:alnum:]]* * | grep -v $(ngrep) || true
	@grep -nr $(grep) 'TODO\|panic' * | grep -v $(ngrep) || true
	@grep -nr $(grep) BUG * | grep -v $(ngrep) || true
	@grep -nr $(grep) [^[:alpha:]]println * | grep -v $(ngrep) || true
	@grep -nir $(grep) 'work.*progress' || true
