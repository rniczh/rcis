#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <signal.h>
#include <ucontext.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <ctype.h>
#include "tags.h"
#include "printer.h"

#ifndef SCHEME_PRINTER
#define SCHEME_PRINTER

#define MAXDEPTH 100
#define MAXLENGTH 1000

const char *byte_to_binary(long x)
{
    static char b[9];
    b[0] = '\0';
    for (long z = 2305843009213693952; z > 0; z >>= 1)
    {
        strcat(b, ((x & z) == z) ? "1" : "0");
    }
    return b;
}
void fprint1(FILE *fp, ptr x, int d) {
  if (TAG(x, mask_fixnum) == tag_fixnum) {
    fprintf(fp, "%ld", (long)UNFIX(x));
  }
  else if (TAG(x, mask_string) == tag_string) {
    char *s = STRINGDATA(x);
    fprintf(fp, "%s", s);
  }
  else if (TAG(x, mask_pair) == tag_pair) {
    int len = 0;
    ptr y;

    if (d > MAXDEPTH) {
      fprintf(fp, "(...)");
      return;
    }
    fprintf(fp, "(");
    fprint1(fp, CAR(x), d+1);
    y = CDR(x);
    while (TAG(y, mask_pair) == tag_pair && (len < MAXLENGTH-1)) {
      fprintf(fp, " ");
      fprint1(fp, CAR(y), d+1);
      y = CDR(y);
      len++;
    }
    if (y != _nil){
      if (len == MAXLENGTH-1)
        fprintf(fp, " ...");
      else {
        fprintf(fp, " . ");
        fprint1(fp, y, d+1);
      }
    }
    fprintf(fp, ")");
  }
  else if (TAG(x, mask_vector) == tag_vector) {
    long i, n;
    ptr *p;
    if (d > MAXDEPTH) {
      fprintf(fp, "#(...)");
      return;
    }
    fprintf(fp, "#(");
    n = UNFIX(VECTORLENGTH(x));
    p = (ptr *)VECTORDATA(x);
    i = n > MAXLENGTH ? MAXLENGTH : n;
    if (i != 0) {
      fprint1(fp, *p, d+1);
      while (--i) {
        fprintf(fp, " ");
        fprint1(fp, *++p, d+1);
      }
    }
    if (n > MAXLENGTH) fprintf(fp, " ...");
    fprintf(fp, ")");
  }
  else if (TAG(x, mask_procedure) == tag_procedure) {
    long i, n, max;
    ptr *p;
    if (d > MAXDEPTH) {
      fprintf(fp, "#<procedure>");
      return;
    }
    fprintf(fp, "#<procedure ");
    n = UNFIX( PROCLENGTH(x) );
    fprintf(fp, "fc: %ld fv: ", n);
    p = PROCDATA(x);
    max = n > MAXLENGTH ? MAXLENGTH : n;
    for (i = 0; i < max; i++) {
      if (TAG(p[i], mask_procedure)) {
        fprintf(fp," #<proc>");
      }
      else {
        fprintf(fp," ");
        fprint1(fp, p[i], d+1);
      }
    }
    if (n > MAXLENGTH) fprintf(fp, " ...");
    fprintf(fp, ">");
  }
  else if (TAG(x, mask_box) == tag_box) {
    ptr *p;
    if (d > MAXDEPTH) {
      fprintf(fp, "#&(...)");
      return;
    }
    fprintf(fp, "#&");
    p = (ptr *)BOXDATA(x);
    fprint1(fp, *p, d+1);
  }
  else if (x == _false) {
    fprintf(fp, "#f");
  }
  else if (x == _true) {
    fprintf(fp, "#t");
  }
  else if (x == _nil) {
    fprintf(fp, "()");
  }
  else if (x == _void) {
    fprintf(fp, "#<void>");
  }
  else {
    fprintf(fp, "print (runtime.c): invalid ptr #x%x\n", (unsigned int) x);
    exit(1);
  }
  fflush(fp);
}

void fprint(FILE *fp, ptr x) {
  fprint1(fp, x, 0);
}


void print(ptr x) {
  fprint1(stdout, x, 0);
}


ptr parse(char* word, int len) {
  /* number */
  if (*word == '-' || (*word >= '0' && *word <= '9')) {
    ptr n;
    for (int i = 1; i < len; i++) {
      if (!isdigit(word[i])) {
        fprintf(stderr, "Exception in read: Invalid input \"%s\" not a fixnum\n", word);
        exit(1);
      }
    }
    n = atoi(word);
    return FIX(n);
  }

  fprintf(stderr, "Error: Invalid input \"%s\"\n", word);
  exit(1);
}

ptr read_ptr() {
  char word[256];
  int len;
  if (!fgets(word, sizeof(word), stdin)) {
    fprintf(stderr, "Error: Failed on fgets() in read_ptr");
    exit(1);
  }
  len = strlen(word);
  word[--len] = 0; /* insert EOF at end of array */

  /* parse the string */
  return parse(word, len);
}

#else /* SCHEME_PRINTER */

void print(long x) {
  printf("%ld", x);
}

ptr read_ptr() {
  ptr i;
  scanf("%ld", &i);
  return i;
}


#endif /* SCHEME_PRINTER */
