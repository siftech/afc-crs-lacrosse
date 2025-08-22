#include <math.h>

int easy_example(int a, int b) {
  b <<= 1;
  return a + b;
}

#define MAGIC_NUMBER 12

int with_a_macro(int x) {
  const unsigned mask = (1 << MAGIC_NUMBER) - 1;
  return (x >> MAGIC_NUMBER) | (x & mask);
}

#define DEFUN(NAME, ARG) int NAME(int ARG)

DEFUN(like_a_syscall, x) {
  x += 2;
  return x;
}

#define TERRIBLE_ADD_N(NAME) NAME##n

int TERRIBLE_ADD_N(mai)(void) {
  int a = 2, b = 3;
  int c = easy_example(a, b);
  int d = with_a_macro(c);
  int e = like_a_syscall(d);
  return e;
}

struct {
  int x;
  int y;
} TERRIBLE_ADD_N(type_on_prev_lines_)(float theta) {
  float x = cos(theta);
  float y = cos(theta);
}
