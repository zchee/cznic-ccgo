union u {
	char *a;
	int b;
};

union u f(int i) {
	return (union u){b: i};
}

int main() {
	int i = 0;
	union u u;
	while ((u = f(i)).b < 5) {
		__builtin_printf("%d\n", i++);
	}
	__builtin_printf("%d\n", u.b);
	return 0;
}
