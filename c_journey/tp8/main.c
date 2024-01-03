#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

typedef struct Chainon {
		double head;
		struct Chainon* tail;
} liste;

// Question 1
liste* cons(double t, liste* q) {
		liste* new = malloc(sizeof(liste));
		new->head = t;
		new->tail = q;
		return new;
}

// Question 3
liste* depuis_tableau(double* tab, int n) {
		liste* l = NULL;
		for (int i = 0; i < n; i++) {
				l = cons(tab[n-i-1], l);
		}
		return l;
}

// Question 4
double head(liste* l) {
		assert(l != NULL);
		return l->head;
}

liste* tail(liste* l) {
		assert(l != NULL);
		return l->tail;
}

// Question 5
// Intéressant, à check
void free_list(liste* l) {
		if (l == NULL) return;
		free_list(l->tail);
		free(l);
}

// TODO: Question 6

// Question 7
int length(liste* l) {
		int n = 0;
		while (l != NULL) {
				n++;
				l = l->tail;
		}
		return n;
}

// TODO: Question 8

// TODO: Question 9

// Question 10
bool are_equal(liste* l1, liste* l2) {
		if (length(l1) != length(l2)) return false;
		while (l1 != NULL && l2 != NULL) {
				if (l1->head != l2->head) return false;				
				l1 = l1->tail;
				l2 = l2->tail;
		}
		return (l1 == NULL && l2 == NULL);
}

// Question 11 : Ici, on peut modifier la tête/queue alors qu'en OCAML non.

bool is_sorted(liste* l) {
		if (length(l)) return false;
		double tmp = l->head;
		l = l->tail;
		while (l != NULL) {
				if (l->head < tmp) return false;
				l = l->tail;
		}
		return true;
}

// Question 12
liste* insert(double x, liste* l) {
		liste* l1 = malloc(sizeof(liste));
		l1->head = x;
		l1->tail = l;
		return l1;
}

// TODO: Question 16

////////////////////////////////////////
/// EXERCICE 2
////////////////////////////////////////

typedef struct {
		liste* data;
		int length;
} stack;

stack* s_create() {
		stack* s = malloc(sizeof(stack));
		liste* l = malloc(sizeof(liste));
		s->data = l;
		s->length = length(l);		
		return s;
}

bool s_is_empty(stack* s) {
		if (s->length == 0)
				return true;
		return false;
}

void s_push(stack* s, double x) {
		s->data = cons(x, s->data); 
		s->length = s->length + 1;
}

double s_pop(stack* s) {
		assert(!s_is_empty(s));
		double hd = head(s->data);
		s->data = tail(s->data);
		s->length = s->length - 1;
		return hd;
}

////////////////////////////////////////
/// EXERCICE 4
////////////////////////////////////////

#define TAILLE_MAX 1000

typedef struct {
		int deb;
		int fin;
		int* data;
} queue;

// Question 1
queue* q_create() {
		queue* p = malloc(sizeof(queue));
		p->deb = 0;
		p->fin = 0;
		p->data = malloc(sizeof(int) * TAILLE_MAX);
		return p;
}

bool q_is_empty(queue* f) {
		return f->deb == f->fin;
}

void q_push(queue* f, int x) {
		assert((f->fin+1) % TAILLE_MAX != f->deb);
		f->data[f->fin] = x;
		f->fin = (f->fin + 1) % TAILLE_MAX;
}

// WELL..
// int q_pop(queue* q) {}


