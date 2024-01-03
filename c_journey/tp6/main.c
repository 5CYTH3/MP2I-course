#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

void exo1(void) {
		int k = -25;
		int* ptr_k = NULL;
		ptr_k = &k;
		k = 2;
		*ptr_k = 55;
		printf("%d\n", k);
		printf("%d\n", *ptr_k);
		printf("%p\n", ptr_k);
}

//////////////////////////
/// Exo 2
//////////////////////////

// Swap the copied values that's passed in parameter as x and y 
void shallow_swap_int(int x, int y) {
		int temp = x;
		x = y;
		y = temp;
}

void deep_swap_int(int* ptr_x, int* ptr_y) {
		int* temp = ptr_x;
		*ptr_x = *ptr_y;
		*ptr_y = *temp;
}

/////////////////////////
/// Exo 3
/////////////////////////

// This function also swap the values that x and y points to
void mystery_function(int* x, int* y) {
		*x = *x - *y;
		*y = *x + *y;
		*x = *y - *x;
}

// It does work if x and y are equal but does not if we pass &x twice.

////////////////////////
/// Exo 4
////////////////////////

double* create_arr(int n, double x) {
		double* ptr_arr = malloc(sizeof(double) * n);
		for (int i = 0; i < n; i++) {
				ptr_arr[i] = x;
		}
		return ptr_arr; 
}

void print_arr(double* arr, int n) {
		for (int i = 0; i < n; i++) {
				printf("[%f] ", arr[i]);
		}
		printf("\n");
}

// Main entrypoint of all the functions of array, used to test the functionality
void test_arr_funcs() {
		double* arr = create_arr(4, 10);
		print_arr(arr, 4);
		free(arr);
}

/////////////////////////
/// Exo 5
/////////////////////////

struct S {
		int a;
		double b;
		char *c;
};

void print_struct(struct S s) {
		printf("{ %d, %f, %s }\n", s.a, s.b, s.c);
}

void test_struct_funcs() {
		struct S s = { .a = 25, .b = 3.25e19, .c = "BONSOIR" };
		print_struct(s);
}

typedef struct {
		double re;
		double im;
} Complex; 

Complex conjugate(Complex c) {
		Complex c_g = { .re = c.re, .im = -c.im };
		return c_g;
}

// need to impl arithmetic

void conjugate_inplace(Complex* c) {
		c->im = -(c->im);
}

double re(Complex c) {
		return c.re;
}

double im(Complex c) {
		return c.im;
}

///////////////////////
/// Exo 6
///////////////////////

typedef struct {
		int length;
		int* data;
} int_array;

int get(int_array t, int k) {
		assert(k <= t.length && k > 0);
		int kth = t.data[k];
		return kth;
}

void set(int_array t, int k, int new_val) {
		t.data[k] = new_val; 
}

int_array* create_int_arr(int length, int initial_value) {
		int_array* arr = malloc(sizeof(int_array));
		int* data = malloc(sizeof(int) * length);
		arr->length = length;
		arr->data = data;
		for (int i = 0; i < arr->length; i++) {
				arr->data[i] = initial_value;
		}
		return arr;
}

void free_int_arr(int_array* t) {
		free(t->data);	
		free(t);
}

void print_int_arr(int_array t) {
		printf("[length]: %d\n", t.length);
		for (int i = 0; i < t.length; i++) {
				printf("[%d] ", t.data[i]);
		}
}

void test_structarr_funcs() {
		int_array* t = create_int_arr(3, 2);
		print_int_arr(*t);
		free_int_arr(t);
}



int main() {
		test_arr_funcs();
		test_struct_funcs();
		test_structarr_funcs();
		return 0;
}
