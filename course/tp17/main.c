#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

int SUDOKU[9][9] = {
		{2, 5, 0, 0, 3, 0, 9, 0, 1}, 
		{0, 1, 0, 0, 0, 4, 0, 0, 0}, 
		{4, 0, 7, 0, 0, 0, 2, 0, 8}, 
		{0, 0, 5, 2, 0, 0, 0, 0, 0}, 
		{0, 0, 0, 0, 9, 8, 1, 0, 0},
		{0, 4, 0, 0, 0, 3, 0, 0, 0},
		{0, 0, 0, 3, 6, 0, 0, 7, 2},
		{0, 7, 0, 0, 0, 0, 0, 0, 3},
		{9, 0, 3, 0, 0, 0, 6, 0, 4}
};

void show_grid(int grid[9][9]) {
		for (int j = 0; j < 9; j++) {
				printf("|");
				for (int i = 0; i < 9; i++) {
						if (grid[j][i] == 0)
								printf(" ");
						else printf("%d", grid[j][i]);
						printf("|");
				}
				printf("\n");
		}
}

void next(int grid[9][9], int i, int j, int* x, int* y) {
		for (int k = j; k > 0; k--) { // y axis
				for (int p = i; p < 9; p++) { // x axis
						if (grid[k][p] == 0) {
								*x = p;
								*y = k;
								return;
						}
				}
		}
		*x = -1;
		*y = -1;
}

bool is_full(int grid[9][9]) {
		int x, y;
		next(grid, 0, 9, &x, &y);
		return x == -1 && y == -1;
}

bool valid(int grid[9][9], int i, int j) {
		for (int k = 0; k < 9; k++) {
				if (k != i && grid[k][j] == grid[i][j])
						return false;
		}
		for (int k = 0; k < 9; k++) {
				if (k != j && grid[i][k] == grid[i][j])
						return false;
		}

		int x_inf = i - (i % 3);
		int y_inf = j - (j % 3);
		for (int k = x_inf; k < x_inf + 3; k++) {
				for (int k2 = y_inf; k2 < y_inf + 3; k2++) {
						if ((k != i || k2 != j) && grid[k][k2] == grid[i][j])
								return false;
				}
		}
		return true;
}

// Naive
void resolve_aux(int grid[9][9], int i, int j) {
		int x, y;
		next(grid, i, j, &x, &y);
		if (x == -1) return;
		for (int c = 1; c <= 9; c++) {
				grid[x][y] = c;
				if (valid(grid, x, y)) {
						resolve_aux(grid, x, y);
						if (is_full(grid)) return;
				}
		}
		grid[x][y] = 0;
		return;
}

bool resolve_aux_optimised(int grid[9][9], int i, int j) {
		int x, y;
		next(grid, i, j, &x, &y);
		if (x == -1) return true;
		for (int c = 1; c <= 9; c++) {
				grid[x][y] = c;
				if (valid(grid, x, y)) {
						if (resolve_aux_optimised(grid, x, y)) return true;
				}
		}
		grid[x][y] = 0;
		return false;
}

void resolve(int grid[9][9]) {
		resolve_aux(grid, 0, 0);
		// OR
		// resolve_aux_optimised(grid, 0, 0);
		return;
}

/////////////////////
//// Exercice 2 /////
/////////////////////

// Question 1

void fusion(int t[], int deb, int mil, int fin) {
		int* res = malloc(sizeof(int) * (fin - deb));
		int k = 0, i = deb;
		while (mil != fin) {
				if (deb < fin) {
						if (mil >= fin && t[deb] <= t[mil]) {
								res[k] = res[deb];
								deb++;
						} else {
								res[k] = t[mil];
								mil++;
						}
						k++;
				}
		}
		
		for (int j = i; j < fin; j++) {
				t[j] = res[j-i];
		}

		free(res);
}

void fusion_sort(int t[], int deb, int fin) {
		if (fin <= deb) return; 
		fusion_sort(t, deb, (fin+deb)/2);
		fusion_sort(t, (fin+deb)/2, fin);
		fusion(t, deb, (fin+deb)/2, fin);
}

int nb_inversions_brut(int* t, int n) {
		int c = 0;
		for (int k = 0; k <= n - 2; k++) {
				for (int j = k + 1; j <= n - 1; j++) {
						if (t[k] > t[j]) c++;
				}
		}
		return c;
}

int nb_inversions_croise(int* t1, int n, int* t2, int m) {
		int phi = 0, i = 0, j = 0;
		while (i < n && j < m) {
				if (t1[i] <= t2[j]) { 
						i++;
				} else {
						phi += n - i;
						j++;
				} 
		}
		return phi;
}



int main() {
		int arr[10] = {0, 5, 3, 2, 6, 7, 1, 4, 8, 9};
		fusion_sort(arr, 0, 9);
		for (int i = 0; i < 10; i++) {
				printf("%d", arr[i]);
		}
}
