#include <signal.h>
#include <ucontext.h>
#include <time.h>
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
#include "copygc.h"
static gcModel *model;

/* Returns a ptr that points to the location of the new object */
ptr forward_pair(rt_ptr p) {
  /* Copy pair to next allocation point */
  rt_pair *n = (rt_pair *)(model->next);
  n->car = p->pair.car;
  n->cdr = p->pair.cdr;

  /* Set the forward pointer */
  p->fwd_ptr = (long)(model->next) + tag_pair;

  /* bump the allocation pointer */
  model->next += size_pair;
  return p->fwd_ptr;
}

ptr forward_procedure(rt_ptr p) {
  /* Copy procedure to next allocation point */
  rt_procedure *n = (rt_procedure*) (model->next);
  n->code   = p->procedure.code;
  n->length = p->procedure.length;

  int length = UNFIX(p->procedure.length);
  for(int i = 0; i < length; i++)
    n->data[i] = p->procedure.data[i];

  /* Set the forward pointer */
  ptr fwd_ptr = (long) (model->next) + tag_procedure;
  p->fwd_ptr = fwd_ptr;

  /* bump the allocation pointer  code + length + data */
  model->next = model->next + 2 + length;
  return fwd_ptr;
}

ptr forward_vector(rt_ptr p) {
  /* Copy vector to next allocation point */
  rt_vector *n = (rt_vector*) (model->next);
  n->length = p->vector.length;

  int length = UNFIX(p->vector.length);
  for(int i = 0; i < length; i++)
    n->data[i] = p->vector.data[i];

  /* Set the forward pointer */
  ptr fwd_ptr = (long) (model->next) + tag_vector;
  p->fwd_ptr = fwd_ptr;

  /* bump the allocation pointer length + data */
  model->next = model->next + 1 + length;
  return fwd_ptr;
}

ptr forward_box(rt_ptr p) {
  /* Copy box to next allocation point */
  rt_box *n = (rt_box *)(model->next);
  n->data = p->box.data;

  /* Set the forward pointer */
  p->fwd_ptr = (long)(model->next) + tag_box;

  /* bump the allocation pointer */
  model->next += size_box;
  return p->fwd_ptr;
}

int forwarded(rt_ptr p) {
  return (p->fwd_p  >= model->tospace && p->fwd_p < model->limit);
}

void forward(ptr *scan, ptr **stop){
  for(;scan < *stop; scan++){
    switch (TAG(*scan, basic_ptr_tag_mask)) {
    case tag_fixnum:
      break;
    case tag_pair:
      {
        rt_ptr p = AS_RT_PTR(*scan);
        if (forwarded(p))
          *scan = p->fwd_ptr;
        else
          *scan = forward_pair(p);
      }
      break;
    case tag_procedure:
      {
        rt_ptr p = AS_RT_PTR(*scan);
        if (forwarded(p))
          *scan = p->fwd_ptr;
        else
          *scan = forward_procedure(p);
      }
      break;
    case tag_vector:
      {
        rt_ptr p = AS_RT_PTR(*scan);
        if (forwarded(p))
          *scan = p->fwd_ptr;
        else
          *scan = forward_vector(p);
      }
      break;
    case tag_box:
      {
        rt_ptr p = AS_RT_PTR(*scan);
        if (forwarded(p))
          *scan = p->fwd_ptr;
        else
          *scan = forward_box(p);
      }
    case tag_boolean:
      break;
    case tag_string:
      break;
    default :
      printf ("Not in the tags.\n");
      exit(1);
    }
  }
}

ptr* getStack(void) {
  return model->stack;
}

ptr* getHeap(void) {
  return model->heap;
}

ptr* getLimit(void) {
  return model->limit;
}

/* allocate a chunk of memory with a guard page on either end */
static ptr* guarded_area(long n) {
  char *addr;
  /* allocate, leaving room for guard pages */
  addr = (char *)mmap(NULL, // the  kernel will chooses the address at which to create the mapping
                      (size_t)(n + 2 * model->pagesize),
                      PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS,
                      // set mmap anonymous for not backed by file
                      // private for neither be seen by other proc nor written to the file
                      // also copy-on-write mapping
                      -1, 0);
  if (addr == (void *)-1) {
    fprintf(stderr, "mmap failed: %s\n", strerror(errno));
    exit(2);
  }

  if (mprotect(addr, (size_t)model->pagesize, PROT_NONE) ||
      mprotect(addr + model->pagesize + n, (size_t)model->pagesize, PROT_NONE)) {
    fprintf(stderr, "mprotect failed: %s\n", strerror(errno));
    exit(3);
  }

  return (ptr *)(addr + model->pagesize);
}


void gc_init(void) {
  model = (gcModel *)malloc(sizeof(gcModel));

  model->pagesize = sysconf(_SC_PAGESIZE); /* $getconf PAGE_SIZE -> xxx bytes */
  model->stacksize = default_stacksize;
  model->heapsize = default_heapsize;

  /* round stack and heap size to even pages */
  model->stacksize = ((model->stacksize + model->pagesize - 1) / model->pagesize) * model->pagesize;
  model->heapsize = ((model->heapsize + model->pagesize - 1) / model->pagesize) * model->pagesize;

  model->stack = guarded_area(model->stacksize);
  model->heap = guarded_area(model->heapsize);
  model->tospace = guarded_area(model->heapsize);
  model->next = model->tospace;
  model->limit = (ptr *)((char *)model->heap + model->heapsize);
  return ;
}


void gc_cleanup(void) {
  munmap(((char *)(model->stack)) - model->pagesize, (size_t)(model->stacksize + 2 * model->pagesize));
  munmap(((char *)(model->heap)) - model->pagesize, (size_t)(model->heapsize + 2 * model->pagesize));
  munmap(((char *)(model->tospace)) - model->pagesize, (size_t)(model->heapsize + 2 * model->pagesize));
  free(model);
  return;
}

void checkAddr(ptr* addr) {
  ptr *heap = model->heap;
  ptr *stack = model->stack;
  if (heap - model->pagesize <= addr && addr < heap) {
    fprintf(stderr,"invalid access just below the heap\n");
  } else if (heap + model->heapsize <= addr && addr <= heap+model->heapsize+model->pagesize) {
    fprintf(stderr,"invalid access just above the heap\n");
  } else if (stack - model->pagesize <= addr && addr < stack) {
    fprintf(stderr,"invalid access just below the stack\n");
  } else if (stack + model->stacksize <= addr && addr < stack+model->stacksize+model->pagesize) {
    fprintf(stderr,"invalid access just above the stack\n");
  } else {
    fprintf(stderr, "| Segmentation violation at %p\n", addr);
  }
  fflush(stderr);
}

ptr* collect(ptr* sp, ptr* hp, long alloc_size) {
  if (hp + (alloc_size / sizeof(ptr)) <= model->limit) {
    return hp;
  }
  /* Init */
  model->sp = sp;
  model->hp = hp;
  model->limit = (ptr *)((char *)model->tospace + model->heapsize);

  /* Collect */
  forward(model->stack, &sp); //forward the stack
  forward(model->tospace, &(model->next)); //forward the heap
  hp = model->next;

  /* Change to next Heap */
  model->next = model->heap;
  model->heap = model->tospace;
  model->tospace = model->next;

  if (!(hp + (alloc_size / sizeof(ptr)) <= model->limit)) {
    fprintf(stderr, "Error: Not enough room after collection\n");
    gc_cleanup();
    exit(1);
  }

  return hp;
}
