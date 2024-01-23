#include <stdlib.h>
#include <stdbool.h>
#include <math.h>

typedef struct tree {
		int value;
		struct tree* left;
		struct tree* right;
} tree;

// Question 1 : Les arbres binaires étiquetés sont représentables par ce type.

// Question 2
tree* buid_tree(int value, tree* left, tree* right) {
		tree* res = malloc(sizeof(tree));
		res->left = left;
		res->right = right;
		res->value = value;
		return res;
}


// Question 3
// Utility function to use in `height`
int max(int x, int y) {
		if (x > y) return x;
		return y;
}

int height(tree* t) {
		if (t == NULL) return 0;
		return 1 + max(height(t->left), height(t->right));
}

int size(tree* t) {
		if (t == NULL) return 0; 
		return 1 + size(t->left) + size(t->right);
}

// Question 4
bool is_leaf(tree* t) {
		return t->left == NULL && t->right == NULL;
} 

// Question 5
int leaf_count(tree* t) {

}

