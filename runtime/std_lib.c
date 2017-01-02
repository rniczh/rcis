#include <stdio.h>
#include <stdlib.h>
#include "tags.h"

ptr rt_system(ptr m) {
  char *s = STRINGDATA(m);
  if (system(s) == 0)
    return _true;
  else
    return _false;

}

void rt_print_addr(ptr x) {
  printf("%s", (char*)x);
}

void rt_error(ptr m) {
  char *s = STRINGDATA(m);
  printf("Error: %s\n", s);
  exit(-1);
}

void rt_exit(ptr n) {
  exit(UNFIX(n));
}
