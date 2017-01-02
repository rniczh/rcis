(import
  (chezscheme)
  (compiler cors)
  (framework match)
  )
(define input-file #f)
(define interp-file #f)
(define c-file '())
(define call-with-input-file
  (lambda (filename proc)
    (let ((p (open-input-file filename)))
      (let ((v (proc p)))
        (close-input-port p)
        v))))
(define (handle-options cl)
  (define (version)
    (display "0.1\n"))
  (define (help)
    (display "usage: rcis [options and files]\n")
    (display "options:\n")
    (display "  -o <path1> <path2> <c-file*>                   compile file to path1\n")
    (display "  -I <path> | --inter <path>                     display the IR code to the file\n")
    (display "  -A <path> | --asm   <path>                     display the Assembly code to the file\n")
    (display "  -S <size(byte)>                                adjust stack size\n")
    (display "  -H <size(byte)>                                adjust heap size\n")
    (display "  --opt <0 | 1> | -O <0 | 1>                     set scheme optimize-level, default is 1\n")
    (display "  -v | --version                                 version\n")
    )
  (let loop ([args (cdr (command-line))])
    (unless (null? args)
      (case (car args)
        [("--version" "-v") (version) (exit)]
        [("--help") (help) (exit)]
        [("--opt" "-O")
         (if (null? (cdr args)) (errorf 'handle-options "optimize-level should not empty")
             (begin
               (scheme-optimize-level (cadr args))
               (set! args (cdr args))))]
        [("--inter" "-I")
         (if (null? (cdr args)) (errorf 'handle-options "intermediate ouput file should not empty")
             (begin
               (intermediate-mode (cadr args))
               (set! args (cdr args))))]

        [("--asm" "-A")
         (if (null? (cdr args)) (errorf 'handle-options "assembly ouput file should not empty")
             (begin
               (assembly-mode (cadr args))
               (set! args (cdr args))))]

        [("-S" "--stack")
         (if (null? (cdr args)) (errorf 'handle-options "stacksize should not empty")
             (begin
               (scheme-debug (format "-D D_STACK_SIZE=~a" (cadr args)))
               (set! args (cdr args))))]

        [("-H" "--heap")
         (if (null? (cdr args)) (errorf 'handle-options "heapsize should not empty")
             (begin
               (scheme-debug (format "-D D_HEAP_SIZE=~a" (cadr args)))
               (set! args (cdr args))))]

        [("-o")
         (cond
           [(null? (cdr args)) (errorf 'handle-options "output path should not empty")]
           [(null? (cddr args)) (errorf 'handle-options "input path should not empty")]
           [else (begin (output-file (cadr args))
                        (if input-file
                            (errorf 'handle-options "input path already existed")
                            (set! input-file (caddr args)))
                        (set! args (cddr args)))])]
        [else
          (if input-file
              (if (file-exists? (car args))
                  (set! c-file (cons (car args) c-file)))
              ;; (errorf 'handle-options "input path already existed")
              (if (file-exists? (car args))
                  (set! input-file (car args))
                  (errorf 'handle-options "invalid file path, please check ~s exist" (car args))))])
      (loop (cdr args)))))

(scheme-optimize-level "1")
(gcc-optimize-level "2")
(handle-options (command-line))
(foreign-files c-file)
(define (parse-file input)
      (define (read-file fp)
        (let ([exp (read fp)])
          (if (eof-object? exp) '()
              (cons exp (read-file fp)))))
      (call-with-input-file input read-file))
(if input-file
    (driver (parse-file input-file))
    (errorf 'compile "require compile file"))
