#include <linux/limits.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>

typedef struct {
		int size;
		int capacity;
		int *data;
} heap; 

////////////////////
//// EXERCICE 1	////
////////////////////

heap* create_heap(int capacity) {
		int* data = malloc(sizeof(int) * capacity);
		heap* hp = malloc(sizeof(heap));
		hp->data = data;
		hp->capacity = capacity;
		hp->size = 0;
		return hp;
}

void free_heap(heap* h) {
		free(h->data);
		free(h);
}
bool is_empty(heap* h) {
		return h->size == 0;
}

int father(int i) {
		return (i-1)/2;
}

int left(int i) {
		return 2*i+1;
}

int right(int i) {
		return 2*i+2;
}

void swap(heap* h, int i, int j) {
		int tmp = h->data[i];
		h->data[i] = h->data[j];
		h->data[j] = tmp;
}

void move_up(heap* h, int i) { 
		int x = h->data[i];
		while (i > 0) {
				int y = h->data[father(i)];
				if (x <= y) break; 
				swap(h, i, father(i));
				i = father(i);
		}
}

void move_down(heap* h, int i) {
		int r = right(i);
		int l = left(i);
		if (r < h->size && h->data[r]  > h->data[i] && h->data[l] < h->data[r]) {
				swap(h, i, r);
				move_down(h, r);
		} else if (l < h->size && h->data[l] > h->data[r]) {
				swap(h, i, l);
				move_down(h, l);
		}
}

void insert(heap* h, int x) {
		assert(h->size < h->capacity);
		h->data[h->size] = x;
		move_up(h, h->size);
		h->size++;
}

int extract_max(heap* h) {
		assert(!is_empty(h));
		int n = h->data[0];
		swap(h, 0, h->size - 1);
		h->size--;
		move_down(h, 0);
		return n;
}

void edit(heap* h, int i,  int x) {
		h->data[i] = x;
		// Ici ces fonctions ne font rien si jamais la condition première qui est x < father(x) ou x > father(x) donc on peut juste call les 2, et l'une des deux ne fera rien
		move_down(h, i);
		move_up(h, i);
}

////////////////////
//// EXERCICE 2 ////
////////////////////

void heap_sort(int* arr, int n) {
		heap* h = create_heap(n);
		for (int i = 0; i < n; i++) {
				insert(h, arr[i]);
		}
		for (int i = 0; i < n; i++) {
				arr[n-1-i] = extract_max(h);
		}
		free_heap(h);
}

void heap_sort2(int* arr, int n) {
		heap h = {
				.capacity = n,
				.size = 0,
				.data = arr
		};
		for (int i = 0; i < n; i++) {
				move_up(&h, i);
		}
		while (h.size > 0) {
				extract_max(&h);
		}
}


int main() {
		int arr[] = {4, 15, 2, 6, 7, 1};
		heap_sort(arr, 6);
		for (int i = 0; i < 6; i++) {
				printf(" %d ", arr[i]);
		}
		return 0;
}
