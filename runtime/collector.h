#ifndef COLLECTOR_H
#define COLLECTOR_H
#include "tags.h"

typedef struct {
  ptr *sp;
  ptr *hp;
  ptr *stack;
  ptr *heap;
  ptr *limit;
} memModel;

/* This is the interface that the runtime is allowed to use */
void gc_init(void);
void gc_cleanup(void);
void gc_print_info(void);
ptr* getStack(void);
ptr* getHeap(void);
ptr* getLimit(void);
void checkAddr(ptr* addr);

/* This is the interface for the compiled code */
memModel *mem;

ptr* collect(ptr *sp, ptr *hp, long size);
#endif
