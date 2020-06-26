int f(int n) {
	return 2*n;
}

int (*fp1)(int) = f;
int (*fp2)(int) = &f;

int main() {
	__builtin_printf("%i\n", fp1(10));
	__builtin_printf("%i\n", (*fp1)(20));
	__builtin_printf("%i\n", (**fp1)(30));
	__builtin_printf("%i\n", fp2(40));
	__builtin_printf("%i\n", (*fp2)(50));
	__builtin_printf("%i\n", (**fp2)(60));
}
