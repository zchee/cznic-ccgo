int f(int i) {
	if (i)
		goto label; 
	if (i) {
label:
		f(0);
		return i;
	}
}

int main() {}
