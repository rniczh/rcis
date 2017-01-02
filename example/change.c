#include <stdio.h>
#include "lscheme.h"

ptr change_var(ptr a, ptr b) {
  ptr c;
  c = DEREF(b);
  DEREF(b) = DEREF(a);
  DEREF(a) = c;
  return _true;
}
