# ccgo/v1

Package ccgo translates [cc](https://modernc.org/cc/) ASTs to Go. (Work In Progress)

The v1 and v2 versions are no longer maintained. Please see the v3 version at

	https://modernc.org/ccgo/v3

Installation

    $ GO111MODULE=on go get -t -v -x modernc.org/ccgo/v3

After this, assuming binaries built by the Go downloader are on your $PATH, you should be able to run

    $ ccgo hello.c -o hello.go

on a C "Hello, World" program and get an interesting result.

This directory also contains the older v2 and v1 versions. You almost certainly want the version in v3.

- Compile hello.c

        jnml@e5-1650:~/tmp> ll
        total 0
        jnml@e5-1650:~/tmp> cat << EOF > hello.c
        #include <stdio.h>
        
        int main() {
            printf("Hello World!\n");
        }
        EOF
        jnml@e5-1650:~/tmp> ccgo hello.c -o hello.go
        jnml@e5-1650:~/tmp> go run hello.go
        Hello World!
        jnml@e5-1650:~/tmp>

- Run `$ go test -download` to (once) get required test resources. Add -dev for
  additional 100MB of gcc sources.
