#include <stdio.h>
#include "lscheme.h"

void rt_print4(ptr n) {
  int k = (int)UNFIX(n);
  printf("%04d", k);
}
