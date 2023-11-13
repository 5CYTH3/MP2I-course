#include <stdio.h>
#include <stdbool.h>
#include <math.h>
#include <stdlib.h>

int absolue(int x) {
		if (x < 0) {
				return -x;
		} else {
				return x;
		}
}

// Question 6
void affiche_k_fois(int n, char c) {
		for (int i = 0; i < n; i++)
				printf("%c", c);
		printf("\n");
}

void figure_a(int n) {
		for (int i = 0; i < n; i++)
				affiche_k_fois(5, '*');
}

void figure_b(int n) {
		for (int i = 0; i < n; i++)
				affiche_k_fois(i+1, '*');
}

void figure_c(int n) {
		for (int i = 0; i < n; i++)
				affiche_k_fois(n-i, '*');
}

void figure_d(int n) {
		figure_b(n/2);
		figure_c(n - n/2);
}

void figure_e() {}

void figure_f() {}

double fast_exp(double x, int n) {
		if (n == 0) {
				return 1.0;
		} else if (n % 2 == 0) {
				return fast_exp(x*x, n/2);
		} else {
				return x * fast_exp(x*x, n/2);
		}
}


bool is_prime(int n) {
		// First case and the only exception
		if (n == 1 || n == 2) return true;
		// If the number is even -> automatically not prime
		if (n % 2 == 0) return false;
		
		for (int i = 3; i <= sqrt(n); i += 2) {
				if (n % i == 0) return false;
		}

		return true;
}

void show_dividers(int n) {
		for (int i = 1; i <= n; i++) 
				if (n % i == 0)
						printf("%d ", i);
}

#define ARRAY_LENGTH 25
int tab[ARRAY_LENGTH];

void init(void) {
		for (int i = 0; i < ARRAY_LENGTH - 1; i++) {
				tab[i] = 0;
		}
}

void show(void) {
		for (int i = 0; i < ARRAY_LENGTH - 1; i++) {
				printf("%d ", tab[i]);
		}
}

// Wtf? Every time I recompile, I get the same sequence haha
void tab_rand(void) {
		for (int i = 0; i < ARRAY_LENGTH - 1; i++) {
				tab[i] = rand();
		}
}

int indice_min(int start, int end) {
		int acc = start;
		for (int i = start; i < end; i++) {
				if (tab[i] < tab[acc])
						acc = i;
				else continue;
		}
		return acc;
}

void swap(int x, int y) {
		int arrx = tab[x];
		tab[x] = tab[y];
		tab[y] = arrx;
}

void select_sort() {
		for (int i = 0; i < ARRAY_LENGTH - 1; i++) {
				swap(indice_min(i, ARRAY_LENGTH), i);
		}
}

int main() {
		
		return 0;
}


