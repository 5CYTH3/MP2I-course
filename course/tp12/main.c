#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>

// Exercice 4

typedef struct bst {
		int val;
		struct bst *lhs;
		struct bst *rhs;
} bst;

bst* node(int val, bst* lhs, bst* rhs) {
		bst* ret = malloc(sizeof(bst));
		ret->lhs = NULL;
		ret->rhs = NULL;
		ret->val = val;
		return ret;
}

int min(bst* t) {
		assert(t != NULL);
		if (t->lhs == NULL) return t->val;
		else return min(t->lhs);
}

int max(bst* t) {
		assert(t != NULL);
		if (t->rhs == NULL) return t->val;
		else return max(t->rhs);
}

bool is_bst(bst* t) {
		return max(t->lhs) <= t->val <= min(t->rhs) && is_bst(t->lhs) && is_bst(t->rhs);
}

bool find(int x, bst* t) {
		if (x == t->val) {
				return true;
		} else if (x < t->val) {
				return find(x, t->lhs);
		} else {
				return find(x, t->rhs);
		} 
		return false;
}

bst* insert(int val, bst* t) {
		
}
