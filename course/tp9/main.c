#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// Question 6
int compare_abs(const  void* first, const void* sec) {
		int firstInt = * (const int *) first;
		int secondInt = * (const int *) sec;
		return abs(firstInt) - abs(secondInt);
}

void tri_3(int* arr, int n) {
		qsort(arr, n, sizeof(arr), *compare_abs);
}

// Question 7
int compare_fabs(const void* fst, const void* snd) {
		double fstDouble = ** (const double **) fst;
		double sndDouble = ** (const double **) snd;
		return fabs(fstDouble) - fabs(sndDouble);
}

void tri_4(double** arr, int n) {
		qsort(arr, n, sizeof(double*), *compare_fabs);
}

// Question 8
typedef struct {
		double re;
		double im;
} complex; 

double re(complex c) {
		return c.re;
}

double im(complex c) {
		return c.im;
}

double module(complex c) {
		return sqrt(c.re * c.re + c.im * c.im);
}

int compare_module(const void* fst, const void* snd) {
		complex fstComplex = * (const complex *) fst;
		complex sndComplex = * (const complex *) snd;
		return module(sndComplex) - module(fstComplex);
}

void tri_5(complex* arr, int n) {
		qsort(arr, n, sizeof(complex), compare_module);
}

// Exercice 2


int main() {
		
		return 0;
}
