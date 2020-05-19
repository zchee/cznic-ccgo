# ccgo

Package ccgo translates [cc](https://modernc.org/cc/) ASTs to Go. (Work In Progress)

Installation

    $ O111MODULE=on go get -t -v -x modernc.org/ccgo/v3

After this, assuming binaries built by the Go downloader are on your $PATH, you shold be able to run

    $ ccgo hello.c -o hello.go

on a C "Hello, World" program and get an interesting result.

This directory also containds the older v2 and v1 versions. You almost certainly want the version in v3.

