set -e
rm -f log-ccgo
make distclean || true
make clean || true
./configure --with-tcl=../tcl8.6.8/unix \
	CC=ccgo \
	CFLAGS='--ccgo-full-paths --ccgo-struct-checks --ccgo-go -I../tcl8.6.8/generic -D_GNU_SOURCE --ccgo-use-import os.DevNull,exec.Command,atomic.Value{} --ccgo-import os,os/exec,sync/atomic' \
	LDFLAGS='--warn-unresolved-libs'
make -j 4 tcltest
go version
date
