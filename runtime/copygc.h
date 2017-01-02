#ifndef COPYGC_H
#define COPYGC_H

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
#include "collector.h"
#include "printer.h"
#include "tags.h"

typedef struct {
  ptr *sp;
  ptr *hp;
  ptr *stack;
  ptr *heap;
  ptr *tospace;
  ptr *next;
  ptr *limit;
  long pagesize;
  long stacksize;
  long heapsize;
} gcModel;

#endif
