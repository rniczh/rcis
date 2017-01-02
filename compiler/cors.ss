#!chezscheme
(library (compiler cors)
  (export driver driver-step
    ;; compiler options
    foreign-files
    output-file
    runtime-files
    gcc-optimize-level
    scheme-optimize-level
    scheme-debug
    intermediate-mode
    assembly-mode
    ;; testing options
    load-test-suite
    )
  (import
    (chezscheme)
    ;; framework
    (framework helpers)
    (framework match)
    (framework driver)

    ;; compiler
    (compiler frontend)
    (compiler backend)
    (compiler options))

  (define (assemble thunk)
    (with-output-to-file (string-append (output-file) ".s")
      thunk
      'replace)
    (if (assembly-mode)
        (with-output-to-file (assembly-mode)
          thunk
          'replace))
    (unless (and (system (format "gcc -std=c11 -Wall -D_GNU_SOURCE ~{~a ~} -O~a -o ~a ~a.s ~a ~a"
                           (scheme-debug)
                           (gcc-optimize-level)
                           (output-file)
                           (output-file)
                           (runtime-files)
                           (foreign-files)
                           ))
                 (unless (equal?
                           (string-append (output-file) ".s")
                           (assembly-mode))
                   (system (format "rm ~a.s" (output-file)))))
      (error 'assemble "assembly failed")))

  ;; (driver driver-step (pass->wrapper)) /
  ;; (driver (pass-wrapper)) /
  ;; (driver driver-step) /
  ;; (driver)
  (define-compiler (driver driver-step)
    ;; pre-processing
    (handle-loads)
    (rewrite-define)
    (parse-scheme)

    ;; datum process
    (convert-complex-datum)

    ;; context process
    (uncover-assigned)
    (purify-letrec)
    ;; (optimize-inline-function) ;; FIXME
    (optimize-direct-call)
    (constant-propagation)
    (convert-assignments)
    (remove-anonymous-lambda)
    (sanitize-binding-forms)

    ;; closures
    (convert-closures)
    (introduce-procedure-primitives)

    ;; normalize
    (lift-letrec)
    (normalize-context)
    (specify-representation)
    (ucloc&remlet)

    ;; backend
    (verify-uil)
    (introduce-cform)
    (remove-complex-opera*)
    (impose-calling-conventions)

    ;; register allocate
    (uncover-frame-conflict)
    (pre-assign-frame)
    (assign-new-frame)
    (iterate
      (finalize-frame-locations)
      (select-instructions)
      (uncover-register-conflict)
      (assign-registers)
      (break/when everybody-home?)
      (assign-frame))
    (finalize-locations)

    ;; code-gen
    (expose-frame-var)
    (expose-basic-blocks)
    (optimize-jumps)
    (flatten-program)
    (collect-strings)
    (generate-x86-64 assemble))
  ;; End library
  )
