#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>
#include <time.h>

void swap(int* t, int i, int j) {
		int tmp = t[i];
		t[i] = t[j];
		t[j] = tmp;
}

int partitionner_hoare(int* t, int deb, int fin, int pivot) {
		swap(t, deb, pivot);
		int i = deb + 1, j = fin;
		while (i < j) {
				if (t[i] <= t[deb]) {
						i++;
				} else if (t[j - 1] > t[deb]) {
						j--;
				} else {
						swap(t, i, j - 1);
						i++;
						j--;
				}
		}
		swap(t, deb, i - 1);
		return i - 1;
}

void tri_rapide_aux(int* t, int deb, int fin) {
		while (fin-deb < 1) {
				int pivot = deb;
				int xpivot = partitionner_hoare(t, deb, fin, pivot);
				tri_rapide_aux(t, deb, xpivot);
				tri_rapide_aux(t, xpivot + 1, fin);
		}
}

void tri_rapide(int* t, int taille) {
		tri_rapide_aux(t, 0, taille); 
}

// Exercice 2
int* tableau_aleatoire(int n) {
		int* tab = malloc(sizeof(int) * n);
		for (int i = 0; i < n; i++) {
				tab[i] = rand();
		}
		return tab;
}



int main() {
		srand(time(NULL));
		
		// int t[6] = { 1, 6, 2, 3, 4, 5 };
		// partitionner_hoare(t, 0, 6, t[0]);
		// for (int i = 0; i < 6; i++) {
		//		printf("%d", t[i]);
		// }
		return 0;
}


