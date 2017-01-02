(library (compiler setting)
  (export
    *enable-optimize-jumps*
    *enable-optimize-direct-call*
    *enable-optimize-closures*
    *enable-optimize-fold-constants*
    *enable-optimize-nops*
    *enable-optimize-deadcode*
    *enable-gc*
    *intermediate-output*
    *assembly-output*
    )
  (import (chezscheme))

  (define *enable-optimize-jumps* (make-parameter #t))
  (define *enable-optimize-direct-call* (make-parameter #t))
  (define *enable-optimize-closures* (make-parameter #t))
  (define *enable-optimize-nops* (make-parameter #t))
  (define *enable-optimize-fold-constants* (make-parameter #t))
  (define *enable-optimize-deadcode* (make-parameter #t))
  (define *enable-gc* (make-parameter #t))
  (define *intermediate-output* (make-parameter #f))
  (define *assembly-output* (make-parameter #f))
  )
