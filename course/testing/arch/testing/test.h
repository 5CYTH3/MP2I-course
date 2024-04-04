#pragma once
#include <time.h>
#include <stdbool.h>
#include <stdio.h>

#define test_void(f) \
({ \
		printf("==================================\n"); \
		printf("[Test] of \"%s\"\n", #f); \
		printf("Type: void\n"); \
		clock_t fst = clock(); \
		f(); \
		clock_t snd = clock(); \
		printf("Function run time: %f s\n", (snd-fst) * 1.0 / CLOCKS_PER_SEC); \
		printf("==================================\n"); \
})

#define test_value(x, s) \
({ \
		printf("==================================\n"); \
		printf("[Test] of \"%s\"\n", #x); \
		printf("Type: typed\n"); \
		clock_t fst = clock(); \
		if (x == s) { \
				printf("Test: PASSED.\n"); \
		} else { \
				printf("Test: FAILED.\n"); \
		} \
		clock_t snd = clock(); \
		printf("Function run time: %f s\n", (snd-fst) * 1.0 / CLOCKS_PER_SEC); \
		printf("=================================="); \
})

#define test(f, s) \
({ \
		printf("==================================\n"); \
		printf("[Test] of \"%s\"\n", #f); \
		printf("Type: typed\n"); \
		clock_t fst = clock(); \
		if (f() == s) { \
				printf("Test: PASSED.\n"); \
		} else { \
				printf("Test: FAILED.\n"); \
		} \
		clock_t snd = clock(); \
		printf("Function run time: %f s\n", (snd-fst) * 1.0 / CLOCKS_PER_SEC); \
		printf("=================================="); \
})
