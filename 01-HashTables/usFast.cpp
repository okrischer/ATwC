#pragma GCC optimize "Ofast"
#include <bits/stdc++.h>
using namespace std;

#define MAX_I 4800000
char _i[MAX_I], _, __; int i0 = 0;
#define scanu(x) do{while((x=_i[i0++])<'0');for(x-='0';'0'<=(_=_i[i0++]);x=x*10+_-'0');}while(0)
#define scan(x) do{while((__=_i[i0++])<45);if(__-45)x=__;else x=_i[i0++];for(x-=48;47<(_=_i[i0++]);x=x*10+_-48);if(__<46)x=-x;}while(0)


struct sofake {
	int a[6];
	int s = 0;
};

bool alike(sofake a, sofake b) {
	for (int i = 0; i < 6; i++) {
		bool ok = true;
		for (int j = 0; j < 6; j++) {
			if (a.a[(i + j) % 6] != b.a[j]) {
				ok = false;
				break;
			}
		}
		if (ok) return true;
		ok = true;
		for (int j = 0; j < 6; j++) {
			if (a.a[(i + j) % 6] != b.a[5 - j]) {
				ok = false;
				break;
			}
		}
		if (ok) return true;
	}
	return false;
}


int N;
sofake S[100010];

int main() {
#ifdef __DEBUG
	freopen("stdin.dat", "r", stdin);
#endif
	_i[fread(_i, 1, MAX_I, stdin)] = 0;
	scanu(N);
	for (int i = 0; i < N; i++) {
		for (int j = 0; j < 6; j++) {
			int t; scanu(t);
			S[i].a[j] = t, S[i].s += t;
		}
	}
	S[N].s = -1;
	std::sort(S, S + N, [](sofake a, sofake b) { return a.s < b.s; });
	for (int i = 0; i < N; i++) {
		if (S[i + 1].s == S[i].s) {
			int j = i + 2; while (S[j].s == S[i].s) j++;
			for (int u = i; u < j; u++) for (int v = i; v < u; v++) {
				if (alike(S[u], S[v])) {
					printf("Twin snowflakes found.\n");
					return 0;
				}
			}
			i = j - 1;
		}
	}
	printf("No two snowflakes are alike.\n");
	return 0;
}