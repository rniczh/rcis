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
#include <time.h>
#include "collector.h"
#include "printer.h"

#ifdef __APPLE__                /* MacOS */
#define SCHEME_ENTRY scheme_entry
#elif __linux__
#define SCHEME_ENTRY _scheme_entry
#endif

extern long SCHEME_ENTRY(ptr *, ptr *);

static void segv_handler(int signo, siginfo_t *info, void *ignore);
static void bus_handler(int signo);

int main(int argc, char *argv[]) {
  struct sigaction action;
  sigset_t s_set;

  gc_init();

  /* Setup segmentation fault signal handler
   * to catch stack and heap overflow and some memory faults */
  sigemptyset(&s_set);
#ifdef __linux__
  action.sa_sigaction = (void *)segv_handler;
#else
  action.sa_sigaction = segv_handler;
#endif
  action.sa_flags = SA_SIGINFO | SA_RESETHAND;
  action.sa_mask = s_set;
  if (sigaction(SIGSEGV, &action, NULL)) {
    fprintf(stderr, "sigaction failed: %s\n", strerror(errno));
    fprintf(stderr, "  overflow checking may not work\n");
  }

  /* Setup bus error signal handler to catch remaining memory faults */
  sigemptyset(&s_set);
  action.sa_handler = bus_handler;
  action.sa_mask = s_set;
  action.sa_flags = SA_RESETHAND;
  if (sigaction(SIGBUS, &action, NULL)) {
    fprintf(stderr, "sigaction failed: %s\n", strerror(errno));
  }

  /* run the Scheme program */
#ifndef COMPILER_TEST
  SCHEME_ENTRY(getStack(), getHeap());
#else
  print(SCHEME_ENTRY(getStack(), getHeap()));
  printf("\n");
#endif

  gc_cleanup();
  return 0;
}

/* Signal handler that traps SIGSEGV and checks if the violation
 * might have been caused by stack or heap overflow */
static void segv_handler(int signo, siginfo_t *info, void *ingore) {
  ptr *addr;
  addr = (ptr *)info->si_addr;

  printf("Segmentation Violation!\nSig: %d\tError: %d\tCode: %d\tAddr: %p\t\n",
         info->si_signo, info->si_errno, info->si_code, (ptr *)info->si_addr);

  checkAddr(addr);
  gc_cleanup();
  exit(-1);
}

/* Signal handler for bus errors */
static void bus_handler(int signo) {
  fprintf(stderr, "Bus error\n");
  exit(-1);
}
