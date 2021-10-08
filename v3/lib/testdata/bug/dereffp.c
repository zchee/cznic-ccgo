void f(int i) {}
void g(int i) {}
void x(int i) {}

int h(void (*proc)(int)) {
	if (proc != *proc) {
		__builtin_printf("oops\n");
		return 255;
	}

	if (*proc == f) {
		return 'f';
	}

	if (*proc == g) {
		return 'g';
	}

	return 0;
}

int h2(void (**proc)(int)) {
	if (*proc == f) {
		return 'f';
	}

	if (*proc == g) {
		return 'g';
	}

	return 0;
}

int main() {
	if (h(f) != 'f') {
		return __LINE__;
	}

	if (h(&f) != 'f') {
		return __LINE__;
	}

	void (*p)(int);
	void (**q)(int) = &p;
	p = f;
	if (h2(q) != 'f') {
		return __LINE__;
	}

	p = &f;
	if (h2(q) != 'f') {
		return __LINE__;
	}

	if (h(g) != 'g') {
		return __LINE__;
	}

	if (h(&g) != 'g') {
		return __LINE__;
	}

	p = g;
	if (h2(q) != 'g') {
		return __LINE__;
	}

	p = &g;
	if (h2(q) != 'g') {
		return __LINE__;
	}
}
