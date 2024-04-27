#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "../testing/test.h"

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

int fib(int n) {
		if (n == 1 || n == 0) return 1;
		return fib(n - 1) + fib(n - 2);
}

void init_array(int* arr, int n) {
		for (int i = 0; i < n; i++) {
				arr[i] = -1;
		}
}

int desc_fib_opt_aux(int* cache, int n) {
		if (n == 0 || n == 1) {
				return 1;
		}

		if (cache[n] != -1) { 
				return cache[n];
		} else {
				int tmp = desc_fib_opt_aux(cache, n - 1) + desc_fib_opt_aux(cache, n - 2);
				cache[n] = tmp;
				return tmp;
		}
}

int desc_fib_opt(int n) {
		int* cache = malloc(sizeof(int) * (n+1));
		init_array(cache, n + 1);
		int tmp = desc_fib_opt_aux(cache, n);
		free(cache);
		return tmp;
}

int asc_fib_opt(int n) {
		int* cache = malloc(sizeof(int) * (n+1));
		cache[0] = cache[1] = 1;
		for (int i = 2; i <= n; i++) {
				cache[i] = cache[i - 1] + cache[i - 2];
		}
		int tmp = cache[n];
		free(cache);
		return tmp;
}

int subarray_sum(int* cache, int i, int n) {
		int acc = 0;
		for (int k = i; k < n; k++)
				acc += cache[k];
		return acc;
}

int max_exhaustive_contiguous_sum(int* cache, int n) {
		int max = 0;
		for (int i = 0; i < n; i++) {
				for (int j = 0; j < n; j++) {
						int tmp = subarray_sum(cache, i, j);
						if (tmp > max) {
								max = tmp;
						}
				}
		}
		return max;
}

int f(int* arr, int i, int n) {
		if (i == n - 1) {
				return arr[i];
		}
		return arr[i] + MAX(f(arr, i+1, n), 0);
}


int main() {
		test(desc_fib_opt(4), 5);
		test(asc_fib_opt(4), 5); 
		return 0;
}
