#include <stdlib.h>

typedef struct Cons {
  int val;
  struct Cons *tail;
} Cons;

Cons *cons(int val, Cons *tail) {
    Cons *cell = malloc(sizeof(Cons));
    cell->val = val;
    cell->tail = tail;
    return cell;
}

int add(int x, int y) {
  return x + y;
}

int main() {
  /* char a = 'a'; */
  /* char b = 'b'; */
  /* char c = 'c'; */
  /* char d = 'd'; */
  /* char e = 'e'; */
  /* char f = 'e'; */
  /* char g = 'e'; */
  /* char h = 'e'; */
  /* int (*fn)(int, int) = &add; */
  /* int sum = (*fn)(1, 2); */
  cons(1, NULL);
  return 0;
}
