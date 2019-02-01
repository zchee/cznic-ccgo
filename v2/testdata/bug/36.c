// +build ignore

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct s {
	char c;
	int a[];
};

struct s2 {
	char c;
	double a[];
};

struct s s;
struct s2 s2;

typedef unsigned mode_t;

struct fdop {
	double next, prev;
	int cmd, fd, srcfd, oflag;
	mode_t mode;
	char path[];
};

struct fdop fdop;

int main() {
	int sz = sizeof(struct s);
	printf("sizeof(struct s): %u\n", sz);
	assert(sz > 1);
	assert(sz <= sizeof(int));

	sz = sizeof(struct s2);
	printf("sizeof(struct s2): %u\n", sz);
	assert(sz > 1);
	assert(sz <= sizeof(int*));

	sz = sizeof(struct fdop);
	printf("sizeof(struct fdop): %u\n", sz);
	printf("offsetof(fdop.next): %u\n", offsetof(struct fdop, next));
	printf("offsetof(fdop.prev): %u\n", offsetof(struct fdop, prev));
	printf("offsetof(fdop.cmd): %u\n", offsetof(struct fdop, cmd));
	printf("offsetof(fdop.fd): %u\n", offsetof(struct fdop, fd));
	printf("offsetof(fdop.srcfd): %u\n", offsetof(struct fdop, srcfd));
	printf("offsetof(fdop.oflag): %u\n", offsetof(struct fdop, oflag));
	printf("offsetof(fdop.mode): %u\n", offsetof(struct fdop, mode));
	printf("offsetof(fdop.path): %u\n", offsetof(struct fdop, path));

	struct fdop *p = calloc(1, sizeof(struct fdop)+100);

	char *q = (char*)&p->path;
	assert(q-(char*)p == offsetof(struct fdop, path));
	assert(!*q);

	*q = 'a';
	assert(p->path[0] == 'a');
	assert(!strcmp(p->path, "a"));

	q = &p->path[0];
	assert(q-(char*)p == offsetof(struct fdop, path));
	assert(*q == 'a');
	*q = 'b';
	assert(p->path[0] == 'b');
	assert(!strcmp(p->path, "b"));

	q = &p->path[1];
	assert(q-(char*)p == offsetof(struct fdop, path) + 1);
	assert(!*q);
	*q = 'c';
	assert(p->path[0] == 'b');
	assert(p->path[1] == 'c');

	assert(!strcmp(p->path, "bc"));
}
