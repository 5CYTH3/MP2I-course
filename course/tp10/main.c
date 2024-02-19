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
		return t != NULL && (t->left == NULL && t->right == NULL);
} 

// Question 5
int leaf_count(tree* t) {
		if (is_leaf(t))
				return 1;
		return leaf_count(t->left) + leaf_count(t->right);
}

// Question 6
int leaf_sum(tree* t) {
		if (is_leaf(t))
				return t->value;
		return leaf_sum(t->left) + leaf_sum(t->right);
}

// Question 7
bool is_strict(tree* t) {
		if (is_leaf(t)) {
				return true;
		} else if (is_strict(t->left) && is_strict(t->right)) {
				return true;
		}
		return false;
}

// Question 8
bool is_in_tree(int value, tree* t) {
		if (t->value == value) {
				return true;
		} else if (is_in_tree(value, t->left) || is_in_tree(value, t->right)) {
				return true;
		}
		return false;
}

// Question 9
void add_value(int value, tree* t) {
		if (t == NULL)
				return;
		t->value = value;
		add_value(value, t->left);
		add_value(value, t->right);
		return;
}

// Question 10
void mirror(tree* t) {
		if (t == NULL)
				return;
		mirror(t->left);
		mirror(t->right);
		tree* tmp = t->left;
		t->right = t->left;
		t->left = tmp;
}

// Question 11
void free_tree(tree* t) {
		if (t == NULL)
				return;
		free_tree(t->right);
		free_tree(t->left);
		free(t);
}

int main () { 
		
		return 0;
}
