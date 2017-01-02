;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#!chezscheme
(library (compiler frontend)
  (export
    handle-loads
    rewrite-define
    parse-scheme

    convert-complex-datum
    uncover-assigned
    purify-letrec
    constant-propagation
    convert-assignments
    optimize-inline-function
    optimize-direct-call
    remove-anonymous-lambda
    sanitize-binding-forms

    uncover-dynamic
    uncover-free
    convert-closures
    introduce-procedure-primitives

    lift-letrec
    normalize-context
    specify-representation
    ucloc&remlet ;; uncover-locals and remove-let
    )
  (import
    (chezscheme)
    (compiler setting)
    (framework match)
    (framework helpers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-loads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; read the (load "xxx.ss")

(define handle-loads
  (lambda (x)
    (define (parse-file input)
      (define (read-file fp)
        (let ([exp (read fp)])
          (if (eof-object? exp) '()
              (cons exp (read-file fp)))))
      (call-with-input-file input read-file))
    (match x
      [,x (guard (atom? x)) x]
      [((load ,[parse-file -> a*]) . ,[d*])
       `(,@(handle-loads `(,a* ...)) ,d* ...)]
      [(,[a] . ,[d]) `(,a . ,d)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rewrite-define
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define -> letrec*
;; rewrite the define form into letrec* form
;; eg:
;; (define a 1)
;; (define f (lambda (x) (+ x 1)))
;; ...
;; ->
;; (letrec* ([a 1] [f (lambda (x) (+ x 1))])
;;    ...)

(define rewrite-define
  (lambda (x)
    (define rewrite
      (lambda (exp ct C)
        (match exp
          [((define ,u ,e) ,t* ...)
           (let loop ([ls exp] [letrec^ '()] [rest^ '()])
             (if (null? ls)
                 (if (null? rest^)
                     (C `((void)))
                     (C `((letrec* (,@(reverse letrec^)) ,@(reverse rest^)))))
                 (match (car ls)
                   [(define (,f ,a* ...) ,e)
                    (let ([ve* (rewrite `(,e) #f id)])
                      (loop (cdr ls) (cons `(,f . ((lambda (,a* ...) ,@ve*))) letrec^) rest^))]
                   [(define ,u ,e)
                    (let ([ve* (rewrite `(,e) #f id)])
                      (loop (cdr ls) (cons `(,u . ,ve*) letrec^) rest^))]
                   [,x (loop (cdr ls) letrec^ (cons x rest^))])))]
          [((begin ,e* ...))
           (let ([ve* (rewrite `(,e* ...) ct id)])
             (C `((begin ,ve* ...))))]
          [((if ,t ,c ,a))
           (let* ([ec* (rewrite `(,c) #f id)]
                  [ea* (rewrite `(,a) #f id)])
             (rewrite `(,t) #f
               (lambda (et*) (C `((if ,@et* ,@ec* ,@ea*))))))]
          [((,let/rec* ((,x* ,v*) ...) ,e* ...))
           (guard (memq let/rec* '(letrec letrec* let let*)))
           (let ([ve* (rewrite `(,e* ...) #f id)]
                 [vv* (rewrite `(,v* ...) #f id)])
             (C `((,let/rec* ((,x* ,vv*) ...) ,@ve*))))]
          [((lambda (,x* ...) ,e* ...))
           (let ([ve* (rewrite `(,e* ...) #f id)])
             (C `((lambda (,x* ...) ,@ve*))))]
          [((set! ,x ,y))
           (rewrite `(,x) #f
             (lambda (ex*)
               (rewrite `(,y) #f
                 (lambda (ey*) (C `((set! ,@ex* ,@ey*)))))))]
          [(,a ,a* ...) (guard (eq? ct 'arg*))
           (rewrite `(,a) 'arg
             (lambda (ea)
               (rewrite `(,a* ...) 'arg*
                 (lambda (ea*) (C `(,@ea ,@ea*))))))]
          [(,h ,t ,t* ...)
           (rewrite `(,h) #f
             (lambda (eh*) `(,@eh* ,@(rewrite `(,t ,t* ...) ct C))))]
          [((,f ,a* ...))
           (rewrite `(,f) 'func
             (lambda (ef*)
               (rewrite `(,a* ...) 'arg*
                 (lambda (ea*)
                   (C `((,@ef* ,@ea*)))))))]
          [,exp (C exp)])))
    (if (null? x)
        `(void)
        (let ([new-bd (rewrite x #f id)])
          (match new-bd
            [((begin ,a* ...)) (guard (null? a*)) `(begin)]
            [,other (make-begin new-bd)])))))

;; `(,new-bd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse-scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;   Program → Exp
;;   Exp     → var
;;           | constant
;;           | (quote Datum)
;;           | (if Exp Exp)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (letrec ([uvar Exp]*) Exp)
;;           | (lambda (uvar*) Exp)
;;           | (prim Exp*)
;;           | (Exp Exp* )
;;   Datum   → immediate
;;           | (Datum . Datum)
;;           | #(Datum*)
;;           | #&Datum

(define-who parse-scheme
  (define (unique-check ls)
    (cond
      [(null? ls) '()]
      [(not (symbol? (car ls)))
       (errorf who "parameter must be a symbol, but got ~a" (car ls))]
      [(memq (car ls) (cdr ls))
       (errorf who "duplicated parameter ~a" (car ls))]
      [else (cons (car ls) (unique-check (cdr ls)))]))

  (define primitives
    '((+ . 2) (- . 2) (* . 2) (/ . 2) (mod . 2) (div . 2) ;; for now '/ is same as 'div
      (logand . 2) (logor . 2) (sra . 2) (ash . 2)
      (<= . 2) (< . 2) (= . 2) (>= . 2) (> . 2)
      (boolean? . 1) (eq? . 2) (fixnum? . 1) (procedure? . 1) (box? . 1)
      (pair? . 1) (null? . 1) (vector? . 1)
      (cons . 2) (car . 1) (cdr . 1) (set-car! . 2) (set-cdr! . 2)
      (box . 1) (unbox . 1) (set-box! . 2)
      (make-vector . 1) (vector-length . 1)
      (vector-ref . 2) (vector-set! . 3)
      ;; TODO: char
      (string? . 1)
      (void . 0) (display . 1) (newline . 0) (read . 0)))

  (define (constant? x)
    (or (memq x '(#t #f ()))
        (string? x)
        (and (and (integer? x) (exact? x))
             (or (fixnum-range? x)
                 (errorf who "integer ~a is out of fixnum range" x)))))

  (define (datum? x)
    (or (constant? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x))))
        (box? x)
        (string? x)
        (char? x)))

  (define prim-helper
    (lambda (env exp)
      (match exp
        [(,prim ,x* ...) (guard (assq prim primitives))
         (let ([len (length x*)])
           (unless (= len (cdr (assq prim primitives)))
             (errorf who "incorrect argument count ~a in call ~a" len prim))
           `(,prim ,(map (parse env) `(,x* ...)) ...))]
        [,prim (guard (assq prim primitives))
          (let* ([len (cdr (assq prim primitives))]
                 [sets (let loop ([n len] [sets '()])
                        (if (= n 0)
                            (reverse sets)
                            (loop (- n 1) (cons (unique-name 'u) sets))))])
            `(lambda (,@sets) (,prim ,@sets)))]
        [,x (errorf who "Malformed error ~a" x)])))

  (define (var-helper sym uv)
    (lambda (env exp)
      (match exp
        [,id (guard (eq? sym id)) uv]
        [(,id ,[(parse env) -> x*] ...) (guard (eq? sym id))
         `(,uv ,x* ...)]
        [,x (errorf who "Malformed error ~a" x)])))

  (define (ext-env* v* env)
    (let* ([uv* (map unique-name (unique-check v*))]
           [new-bind (map (lambda (x y) `(,x . ,(var-helper x y))) v* uv*)]
           [new-env (append new-bind env)])
      (values uv* new-env)))

  (define env0
    `([quote . ,(lambda (env exp)
                  (match exp
                    [(quote ,datum)
                     (unless (datum? datum)
                       (errorf who "invlalid datum ~a" datum))
                     `(quote ,datum)]
                    [,x (errorf who "invalid syntax ~a" x)]))]
      [not . ,(lambda (env exp)
              (match exp
                [(not ,[(parse env) -> e])
                 `(if ,e '#f '#t)]
                [,x (errorf who "invalid syntax ~a" x)]))]
      [if . ,(lambda (env exp)
               (match exp
                 [(if ,[(parse env) -> t] ,[(parse env) -> c])
                  `(if ,t ,c (void))]
                 [(if ,[(parse env) -> t] ,[(parse env) -> c] ,[(parse env) -> a])
                  `(if ,t ,c ,a)]
                 [,x (errorf who "invalid syntax ~a" x)]))]
      [and . ,(lambda (env exp)
                (define (loop e*)
                  (cond
                    [(null? e*) '#t]
                    [(null? (cdr e*)) (car e*)]
                    [else
                      (let ([e (car e*)])
                        `(if ,e ,(loop (cdr e*)) '#f))]))
                (match exp
                  [(and ,[(parse env) -> e*] ...) (loop e*)]
                  [,x (errorf who "invalid syntax ~a" x)]))]
      [or . ,(lambda (env exp)
               (define (loop e*)
                 (cond
                   [(null? e*) '#f]
                   [(null? (cdr e*)) (car e*)]
                   [else
                     (let ([t (unique-name 'tmp)]
                           [e (car e*)])
                       `(let ([,t ,e]) (if ,t ,t ,(loop (cdr e*)))))]))
               (match exp
                 [(or ,[(parse env) -> e*] ...) (loop e*)]
                 [,x (errorf who "invalid syntax ~s" x)]))]
      [begin . ,(lambda (env exp)
                  (match exp
                    [(begin ,[(parse env) -> e*] ...)
                     (cond
                       [(null? e*) `(void)]
                       [else (make-begin `(,e* ...))])]))]
      [lambda . ,(lambda (env exp)
              (match exp
                [(lambda (,x* ...) ,e1 ,e* ...)
                 (let-values ([(uv* new-env) (ext-env* `(,x* ...) env)])
                   (let ([new-bd (map (parse new-env) `(,e1 ,e* ...))])
                     `(lambda (,uv* ...) ,(make-begin new-bd))))]
                [,x (errorf who "invalid syntax ~a" x)]))]
      [let . ,(lambda (env exp)
                (match exp
                  [(let ([,x* ,[(parse env) -> v*]] ...) ,e1 ,e* ...)
                   (let-values ([(uv* new-env) (ext-env* `(,x* ...) env)])
                     (let ([new-bd (map (parse new-env) `(,e1 ,e* ...))])
                       `(let ([,uv* ,v*] ...)
                          ,(make-begin new-bd))))]
                  [,x (errorf who "invalid syntax ~a" x)]))]

      [let* . ,(lambda (env exp)
                 (match exp
                   [(let* ([,x1 ,[(parse env) -> v1]]) ,e1 ,e* ...)
                    (let-values ([(uv new-env) (ext-env* `(,x1) env)])
                      (let ([new-bd (map (parse new-env) `(,e1 ,e* ...))])
                        `(let ([,@uv ,v1])
                           ,(make-begin new-bd))))]
                   [(let* ([,x1 ,v1] [,x* ,v*] ...) ,e1 ,e* ...)
                    (let-values ([(uv new-env) (ext-env* `(,x1) env)])
                      `(let ([,@uv ,((parse env) v1)])
                         ,((parse new-env) `(let* ([,x* ,v*] ...) ,e1 ,e* ...))))]
                   [,x (errorf who "invalid syntax ~a" x)]))]

      [letrec . ,(lambda (env exp)
                   (match exp
                     [(letrec ([,x* ,v*] ...) ,e1 ,e* ...)
                      (let-values ([(uv* new-env) (ext-env* `(,x* ...) env)])
                        (let ([new-v* (map (parse new-env) v*)]
                              [new-bd (map (parse new-env) `(,e1 ,e* ...))])
                          `(letrec ([,uv* ,new-v*] ...)
                             ,(make-begin new-bd))))]
                     [,x (errorf who "invalid syntax ~a" x)]))]
      [letrec* . ,(lambda (env exp)
                    (match exp
                      [(letrec* ([,x* ,v*] ...) ,e1 ,e* ...)
                       (let-values ([(uv* new-env) (ext-env* `(,x* ...) env)])
                         (let ([new-v* (map (parse new-env) v*)]
                               [new-bd (map (parse new-env) `(,e1 ,e* ...))])
                           `(letrec* ([,uv* ,new-v*] ...)
                              ,(make-begin new-bd))))]
                      [,x (errorf who "invalid syntax ~a" x)]))]
      [set! . ,(lambda (env exp)
                 (match exp
                   [(set! ,[(parse env) -> x] ,[(parse env) -> e])
                    (unless (uvar? x)
                      (errorf who "invalid syntax ~a" exp))
                    `(set! ,x ,e)]
                   [,x (errorf who "invalid syntax ~a" x)]))]
      [ccall . ,(lambda (env exp)
                  (match exp
                    [(ccall ,str ,[(parse env) -> pram*] ...) (guard (string? str))
                     `(ccall ,str ,pram* ...)]
                    [,x (errorf who "invalid syntax ~a" x)]))]
      ,@(map (lambda (x) `(,(car x) . ,prim-helper)) primitives)
      ))


  (define (apply-env x env)
    (cond
      [(assq x env) => cdr]
      [else (errorf who "unbound variable ~a" x)]))
  (define (parse env)
    (lambda (x)
      (match x
        [,x (guard (constant? x)) `(quote ,x)]
        [,id (guard (symbol? id)) ;; (apply-env id env)
          ((apply-env id env) env id)]
        [(,id ,e* ...) (guard (symbol? id))
         ((apply-env id env) env `(,id ,e* ...))]
        [(,[e1] ,[e*] ...) `(,e1 ,e* ...)]
        [,x (errorf who "invalid program ~a" x)])))
  (lambda (x)
    (unique-name-count 0)
    ((parse env0) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert-complex-datum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ---------------------------------------------------------------------
;; | INPUT BNF
;; ---------------------------------------------------------------------
;;   Program → Exp
;;   Exp     → var
;;           | constant
;;           | (quote Datum)
;;           | (if Exp Exp)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (letrec ([uvar Exp]*) Exp)
;;           | (lambda (uvar*) Exp)
;;           | (prim Exp*)
;;           | (Exp Exp* )
;;   Datum   → immediate
;;           | (Datum . Datum)
;;           | #(Datum*)
;;           | #&Datum
;; ---------------------------------------------------------------------
;; | OUTUT BNF
;; ---------------------------------------------------------------------
;;   Program → Exp
;;   Exp     → var
;;           | (quote immediate)
;;           | (if Exp Exp)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (letrec ([uvar Exp]*) Exp)
;;           | (lambda (uvar*) Exp)
;;           | (prim Exp*)
;;           | (Exp Exp* )


(define convert-complex-datum
  (lambda  (x)
    (define constants '())
    (define process-datum
      (lambda (x)
        (match x
          [() (quote '())]
          [#(,[a*] ...)
           (let ([tmp (unique-name 'vec)]
                 [len (length `(,a* ...))])
             (let loop ([ls `(,a* ...)] [sets '()] [n 0])
               (if (null? ls)
                   `(let ([,tmp (make-vector (quote ,len))])
                      ,(make-begin `(,@(reverse sets) ,tmp)))
                   (loop
                     (cdr ls)
                     (cons `(vector-set! ,tmp (quote ,n) ,(car ls)) sets)
                     (add1 n)))))]
          [,a (guard (box? a))
            (let ([dtm (process-datum (unbox a))])
              `(box ,dtm))]
          [(,[a] . ,[d]) `(cons ,a ,d)]
          [,x `(quote ,x)])))
    (define convert
      (lambda (x)
        (match x
          [,x (guard (atom? x)) x]
          [(quote ,imm) (guard (immediate? imm))
           `(quote ,imm)]
          [(quote ,datum)
           (let ([tmp (unique-name 'dtm)]
                 [dtm (process-datum datum)])
             (set! constants (cons `(,tmp ,dtm) constants))
             tmp)]
          [(,[a] . ,[d]) `(,a . ,d)])))
    (let ([x* (convert x)])
      (if (null? constants) x* `(let ,constants ,x*)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncover-assigned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; preserve the assign form for later we deal with the set!

;; let, letrec, lambda will bind with assigned form
;; (let ([uvar Exp]*) Exp)    -> (let ([uvar Exp]*) (assigned (uvar*) Exp))
;; (letrec ([uvar Exp]*) Exp) -> (letrec ([uvar Exp]*) (assigned (uvar*) Exp))
;; (lambda (uvar*) body)      -> (lambda (uvar*) (assigned (uvar*) Exp))

(define uncover-assigned
  (lambda (x)
    (define uncover
      (lambda (x)
        (match x
          [(,letrec/* ([,u* ,[e* assign*]] ...) ,[e assign])
           (guard (memq letrec/* '(letrec letrec*)))
           (let* ([as* (union assign (apply union assign*))]
                  [uas* (intersection u* as*)])
             (values
               `(,letrec/* ([,u* ,e*] ...) (assigned ,uas* ,e))
               (difference as* u*)))]
          [(let ([,u* ,[e* assign*]] ...) ,[e assign])
           (let ([uas* (intersection u* assign)])
             (values
               `(let ([,u* ,e*] ...) (assigned ,uas* ,e))
               (union (apply union assign*) (difference assign u*))))]
          [(lambda (,u* ...) ,[e assign])
           (let ([uas* (intersection u* assign)])
             (values `(lambda (,u* ...) (assigned ,uas* ,e)) assign))]
          [(if ,[t as-t] ,[c as-c] ,[a as-a])
           (values `(if ,t ,c ,a) (union as-t as-c as-a))]
          [(begin ,[e* assign*])
           (values `(begin ,e* ...) (apply union assign*))]
          [(set! ,x ,[e assign])
           (values `(set! ,x ,e) (set-cons x assign))]
          [(,[f assign] ,[a* assign*] ...)
           (values `(,f ,a* ...) (union assign (apply union assign*)))]
          [,x (values x '())])))
    (let-values ([(exp _) (uncover x)])
      exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; purify-letrec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; conform any letrec binding forms associated with lambda
;; and also eliminate the letrec* form
;; letrec -> simple | lambda | complex
;; [x e]
;;   | [xs es] simple: if x is not assigned and e is simple expression
;;   | [xl el] lambda: if x is not assigned and e is lambda expression
;;   | [xc ec] complex: otherwise
;; eg:
;; (letrec ([uvar Exp]*) (assigned (uvar*) Exp))
;; -> (letrec ([uvar (lambda (uvar*) (assigned (uvar*) Exp))]*) Exp)

;; ref: "fixing letrec" by ghuloum & kent

;; letrec :
;;bd4 | (let ([xs es] ...)
;;    |   (assigned (as* ...)
;;bd3 |     (let ([xc (void)] ...)
;;    |       (assigned (xc ...)
;;bd2 |         (letrec ([xl el] ...)
;;bd1 |           (let ([xt ec] ...)
;;    |             (assigned ()
;;    |               (set! xc xt)
;;    |               ...))
;; bd |           body)))))

;; letrec* :
;;bd4 | (let ([xs es] ...)
;;    |   (assigned (as* ...)
;;bd3 |     (let ([xc (void)] ...)
;;    |       (assigned (xc ...)
;;bd2 |         (letrec ([xl el] ...)
;;bd1 |           (begin
;;    |             (set! xc ec)
;;    |              ...)
;; bd |            body)))))

(define purify-letrec
  (lambda (x)
    (define (simple? vx* ve)
      (define (not-simple? vx* ve lam?)
        (match ve
          [(quote ,imm) #f]
          [(if ,[t] ,[c] ,[a]) (or t c a)]
          [(begin ,[e*] ...) (ormap id e*)]
          [(let ([,x* ,[e*]] ...) ,[e])
            (or (and (null? (intersection x vx*)) e) (ormap id x*))]
          [(,letrec/* ([,x* ,[e*]] ...) ,[e])
           (guard (memq letrec/* '(letrec letrec*)))
           (and (null? (intersection x* vx*)) (or (ormap id e*) e))]
          [(lambda (,x* ...) ,e)
           (and (null? (intersection x vx*)) (not-simple? x* e #t))]
          [(set! ,[x] ,[v]) (or x v)]
          [(,f ,[a*] ...) (guard (prim? f)) (ormap id a*)]
          [(,[x*] ...) (or (not lam?) (ormap id x*))]
          [,e (memq e vx*)]))
      (not (not-simple? vx* ve #f)))
    (define purify
      (lambda (x* def* assign*)
        (let loop ([def* def*] [simple* '()] [lambda* '()] [complex* '()])
          (cond
            [(null? def*) (values (reverse simple*) (reverse lambda*) (reverse complex*))]
            [else
              (match (car def*)
                [(,lbl (lambda (,x* ...) ,bd)) (guard (memq lbl assign*))
                 (loop (cdr def*) simple* lambda* (cons (car def*) complex*))]
                [(,lbl (lambda (,x* ...) ,bd))
                 (loop (cdr def*) simple* (cons (car def*) lambda*) complex*)]
                [(,lbl ,e) (guard (simple? x* e))
                 (loop (cdr def*) (cons (car def*) simple*) lambda* complex*)]
                [,other (loop (cdr def*) simple* lambda* (cons (car def*) complex*))])]))))
    (match x
      [,x (guard (atom? x)) x]
      [(,letrec/* ([,u* ,[e*]] ...) (assigned (,assign* ...) ,[bd]))
       (guard (memq letrec/* '(letrec letrec*)))
       (let-values ([(simple* lambda* complex*)
                     (purify u* `([,u* ,e*] ...) assign*)])
         (match complex*
           [([,x* ,e*] ...)
            (let* ([bd1 (if (null? complex*) bd
                            (if (eq? letrec/* 'letrec*)
                                `(begin
                                   (set! ,x* ,e*) ...
                                   ,bd)
                                (let ([xt* (map (lambda (x) (unique-name 'tmp)) x*)])
                                  `(begin
                                     (let ([,xt* ,e*] ...)
                                       (assigned () (begin (set! ,x* ,xt*) ...)))
                                     ,bd))))]
                   [bd2 (if (null? lambda*) bd1
                            `(letrec ,lambda* ,bd1))]
                   [bd3 (if (null? complex*) bd2
                            `(let ([,x* (void)] ...)
                               (assigned (,x* ...) ,bd2)))])
              (if (null? simple*) bd3
                  (let ([asv (intersection assign* (map car simple*))])
                    `(let ,simple* (assigned ,asv ,bd3)))))]))]
      [(,[a] . ,[d]) `(,a . ,d)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimize-inline-function ;; FIXME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define optimize-inline-function
  (lambda (x)
    ;;TODO rename the expression -> similar as constant propagation
    (define (rename env)
      (lambda (x)
        (match x
          [,x (guard (uvar? x))
            (cond
              [(assq x env) => cadr]
              ;; [else x]
              [else (errorf 'optimize-inline-function "Malformed error ~a" x)]
              )]
          [,x (guard (atom? x)) x]
          ;; u* -> new-u*
          [(letrec ([,u* ,e*] ...) ,e)
           (let* ([bind* (map (lambda (x) `(,x ,(unique-name x))) u*)]
                  [new-env (append bind* env)])
             (let ([lhe* (map (rename new-env) e*)]
                   [rhe  ((rename new-env) e)])
               `(letrec ([,(map cadr bind*) ,lhe*] ...) ,rhe)))]

          [(let ([,u* ,e*] ...) (assigned (,asv* ...) ,e))
           (let* ([bind* (map (lambda (x) `(,x ,(unique-name x))) u*)]
                  [new-env (append bind* env)])
             (let ([lhe* (map (rename env) e*)]
                   [rhe  ((rename new-env) e)]
                   [new-asv* (map (rename new-env) asv*)])
               `(let ([,(map cadr bind*) ,lhe*] ...) (assigned (,new-asv* ...) ,rhe))))]

          [(lambda (,x* ...) (assigned (,asv* ...) ,e))
           (let* ([bind* (map (lambda (x) `(,x ,(unique-name x))) x*)]
                  [new-env (append bind* env)])
             (let ([rhe ((rename new-env) e)]
                   [new-asv* (map (rename new-env) asv*)])
               `(lambda (,(map cadr bind*) ...) (assigned (,new-asv* ...) ,rhe))))]

          [(begin ,[(rename env) -> e*] ...)
           `(begin ,e* ...)]

          [(set! ,[(rename env) -> x] ,[(rename env) -> v]) `(set! ,x ,v)]

          [(if ,[(rename env) -> t]
               ,[(rename env) -> c]
               ,[(rename env) -> a])
           `(if ,t ,c ,a)]
          [(quote ,imm) `(quote ,imm)]
          [(,[(rename env) -> a] . ,[(rename env) -> d]) `(,a . ,d)])))

    (define env0 '())
    (define (inline env)
      (lambda (x)
        (match x
          [(letrec ([,u* ,[(inline env) -> fv* e*]] ...) ,e)
           (let loop ([lhu* u*] [lhe* e*] [new-env env] [lfv* fv*])
             (if (null? lhu*)
                 (let-values ([(fv rhe) ((inline new-env) e)])
                   (values (difference (union (apply union fv*) fv) u*)
                     `(letrec ([,u* ,e*] ...) ,rhe))) ;; [,out-u* ,out-e*]
                 (let ([lfv (car lfv*)])
                      (if (null? lfv)
                          (loop (cdr lhu*) (cdr lhe*) (cons `(,(car lhu*) . ,(car lhe*)) new-env) (cdr lfv*))
                          (loop (cdr lhu*) (cdr lhe*) new-env (cdr lfv*))))))]

          [(let ([,u* ,[(inline env) -> fv* e*]] ...) (assigned (,asv* ...) ,e))
           (let loop ([lhu* u*] [lhe* e*] [new-env env] [lfv* fv*] [out-u* '()] [out-e* '()])
             (if (null? lhu*)
                 (let-values ([(fv rhe) ((inline new-env) e)])
                   (values (union (apply union fv*) (difference fv u*))
                     `(let ([,out-u* ,out-e*] ...) (assigned (,asv* ...) ,rhe)))) ;; [,out-u* ,out-e*]
                 (match (car lhe*)
                   [(lambda (,x* ...) ,v*)
                    (let ([lfv (car lfv*)])
                      (if (null? lfv)
                          (loop (cdr lhu*) (cdr lhe*) (cons `(,(car lhu*) . ,(car lhe*)) new-env)
                            (cdr lfv*) out-u* out-e*)
                          (loop (cdr lhu*) (cdr lhe*) new-env (cdr lfv*) (cons (car lhu*) out-u*) (cons (car lhe*) out-e*))))]
                   [,other
                     (loop (cdr lhu*) (cdr lhe*) new-env (cdr lfv*) (cons (car lhu*) out-u*) (cons (car lhe*) out-e*))])))]
          [(lambda (,u* ...) (assigned (,asv* ...) ,[(inline env) -> fv* e]))
           (let ([fv (difference fv* u*)])
             (values fv `(lambda (,u* ...) (assigned (,asv* ...) ,e))))]

          [(begin ,[(inline env) -> fv* e*] ...)
           (values (apply union fv*) `(begin ,e* ...))]
          [(if ,[(inline env) -> tf te]
               ,[(inline env) -> cf ce]
               ,[(inline env) -> af ae])
           (values (union tf cf af) `(if ,te ,ce ,ae))]
          [(set! ,x ,[(inline env) -> fv e])
           (values (set-cons x fv) `(set! ,x ,e))]
          [(quote ,imm)
           (values `() `(quote ,imm))]
          [(,prim ,[(inline env) -> fv* e*] ...) (guard (prim? prim))
           (values (apply union fv*) `(,prim ,e* ...))]
          [(,[(inline env) -> fv f] ,[(inline env) -> fv* a*] ...)
           (values (apply union `(,fv ,fv* ...)) `(,f ,a* ...))]
          [,x (guard (string? x)) (values `() x)]
          [,x (values `(,x)
                (cond
                  [(assq x env) => (lambda (x) ((rename '()) (cdr x)))]
                  [else x]))])))
    (let-values ([(_ exp) ((inline env0) x)]) exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimize-direct-call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ((lambda (uvar*) expr) val*) can be transform to (let ([uvar val]*) expr)

(define optimize-direct-call
  (lambda (x)
    (define opt
      (lambda (x)
        (match x
          [((lambda (,x* ...) (assigned (,asv* ...) ,[e])) ,[v*] ...)
           (if (not (eq? (length x*) (length v*)))
               (errorf 'optimize-direct-call "incorrect argument count in call ~a" x))
           `(let ([,x* ,v*] ...) (assigned (,asv* ...) ,e))]
          [(lambda (,x* ...) (assigned (,asv* ...) ,[e]))
           `(lambda (,x* ...) (assigned (,asv* ...) ,e))]
          [(let ([,x* ,[v*]] ...) (assigned (,asv* ...) ,[e]))
           `(let ([,x* ,v*] ...) (assigned (,asv* ...) ,e))]
          [(letrec ([,u* (lambda (,x* ...) ,[e*])] ...) (assigned (,asv* ...),[e]))
           `(letrec ([,u* (lambda (,x* ...) ,e*)] ...) (assigned (,asv* ...) ,e))]
          [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
          [(begin ,[e*] ...) `(begin ,e* ...)]
          [(quote ,imm) `(quote ,imm)]
          [(,[f] ,[a*] ...)
           (match f
             [(,let/rec ,def* (assigned (,asv* ...) ,e))
              (guard (memq let/rec '(let letrec)))
              `(,let/rec ,def* (assigned (,asv* ...) ,(opt `(,e ,a* ...))))]
             ;; [(if ,t ,c ,a)  ;;FIXME c and a are all lambda-term?
             ;;  `(if ,t ,(opt `(,c ,a* ...)) ,(opt `(,a ,a* ...)))]
             [(begin ,e* ... ,e)
              `(begin ,e* ... ,(opt `(,e ,a* ...)))]
             [,other `(,f ,a* ...)])]

          [,x x]
          )))
    (if (*enable-optimize-direct-call*) (opt x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constant-propagation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define constant-propagation
  (lambda (x)
    (define (const? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (errorf 'constant-propagation "integer ~a out of fixnum range" x)))
          (string? x)
          ))
    (define (make-quote x)
      (if (or (const? x) (string? x)) `(quote ,x) x))
    (define env0
      `((+ . ,+) (- . ,-) (* . ,*) (/ . ,div) (div . ,div)
        (mod . ,mod) (ash . ,ash) (sra . ,sra) (logand . ,logand)
        (> . ,>) (>= . ,>=) (= . ,=) (<= . ,<=) (< . ,<)
        (boolean? . ,boolean?) (fixnum? . ,fixnum?) (null? . ,null?)
        (pair? . ,pair?) (vector? . ,vector?) (box? . ,box?) (eq? . ,eq?)
        (string? . ,string?)
        ))
    (define (lookup x env)
      (cond
        [(assq x env) => cdr]
        [else x]))

    (define (propagate env tail?)
      (lambda (x)
        (match x
          [(let ([,u* ,[(propagate env #f) -> e*]] ...) (assigned (,asv* ...) ,e))
           (let-values ([(env^ env_)
                         (partition
                           (lambda (x) (and (not (memq (car x) asv*))
                                       (or (const? (cdr x)) (uvar? (cdr x)))))
                           `((,u* . ,e*) ...))])
             (let ([new-bind* (map (lambda (l) `(,(car l) ,(make-quote (cdr l)))) env_)])
               `(let (,new-bind* ...)
                  (assigned (,asv* ...) ,((propagate (append env^ env) #t) e)))))]
          [(letrec ([,u* ,[(propagate env #f) -> e*]] ...) ,[(propagate env #t) -> e])
           `(letrec ([,u* ,e*] ...) ,e)]
          [(lambda (,x* ...) (assigned (,asv* ...) ,[(propagate env #t) -> e]))
           `(lambda (,x* ...) (assigned (,asv* ...) ,e))]
          [(begin ,[(propagate env #t) -> e*] ...)
           `(begin ,e* ...)]
          [(if ,[(propagate env #f) -> t]
               ,[(propagate env #t) -> c]
               ,[(propagate env #t) -> a])
           (cond
             [(const? t) (if t c a)]
             [else `(if ,(make-quote t) ,c ,a)])]
          [(ccall ,str ,[(propagate env #f) -> aa*] ...)
           `(ccall ,str ,(map make-quote aa*) ...)]
          [(set! ,x ,[(propagate env #t) -> v])
           `(set! ,x ,v)]
          [(quote ,imm)
           (if tail? (make-quote imm) imm)]
          [(,f ,[(propagate env #f) -> aa*] ...)
           (let ([ff ((propagate env #f) f)])
             (cond
               [(and (procedure? ff) (andmap const? aa*))
                (let ([fa (apply ff aa*)])
                  (if tail? (make-quote fa) fa))]
               [else
                 (let ([aa* (map make-quote aa*)])
                   `(,(if (procedure? ff) f ff) ,aa* ...))]))]
          [,x (let ([v (lookup x env)])
                (if tail? (make-quote v) v))])))
    (if (*enable-optimize-closures*)
        ((propagate env0 #t) x)
        x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert-assignments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove all assigned form, and convert each set! to box form

;; eg:
;; (assigned (x) (set! x e) ->
;; (let ([x (box new-x)]) (set-box! x e))

;; the result of this pass:
;;   Program → Exp
;;   Exp     → var
;;           | (quote immediate)
;;           | (if Exp Exp)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (letrec ([uvar (lambda (uvar*) body)]*) Exp)
;;           | (lambda (uvar*) body)
;;           | (prim Exp*)
;;           | (Exp Exp* )

(define convert-assignments
  (lambda (x)
    (define (gen-bindings ct)
      (lambda (asv* def*)
        (case ct
          [lambda (let loop ([def* def*] [gen '()] [env '()]) ;; (uvar* ...)
               (cond
                 [(null? def*) (values (reverse gen) (reverse env))]
                 [(memq (car def*) asv*)
                  (let ([new (unique-name (car def*))])
                    (loop (cdr def*)
                      (cons new gen)
                      (cons `(,(car def*) (box ,new)) env)))]
                 [else (loop (cdr def*) (cons (car def*) gen) env)]))]
          [let (let loop ([def* def*] [gen '()] [env '()]) ;; ([uvar* expr*] ...)
                 (cond
                   [(null? def*) (values (reverse gen) (reverse env))]
                   [(memq (caar def*) asv*)
                    (let ([new (unique-name (caar def*))])
                      (loop (cdr def*)
                        (cons `(,new ,(cadar def*)) gen)
                        (cons `(,(caar def*) (box ,new)) env)))]
                   [else (loop (cdr def*) (cons (car def*) gen) env)]))])))
    (define (convert env)
      (lambda (x)
       (match x
          [,x (guard (atom? x))
            (if (memq x env) `(unbox ,x) x)]
          [(let ([,x* ,[e*]] ...) (assigned (,asv* ...) ,e))
           (let-values ([(def* env*) ((gen-bindings 'let) asv* `((,x* ,e*) ...))])
             (let* ([new-exp ((convert (append asv* env)) e)]
                    [bd (if (null? env*) new-exp `(let ,env* ,new-exp))])
               (if (null? def*) bd `(let ,def* ,bd))))]
          [(lambda (,u* ...) (assigned (,asv* ...) ,e))
           (let-values ([(def* env*) ((gen-bindings 'lambda) asv* `(,u* ...))])
             (let* ([new-exp ((convert (append asv* env)) e)]
                    [bd (if (null? env*) new-exp `(let ,env* ,new-exp))])
               `(lambda ,def* ,bd)))]
          [(set! ,x ,[v])
           (if (memq x env)
               `(set-box! ,x ,v) `(set! ,x ,v))]
          [(,[a] . ,[d]) `(,a . ,d)])))
    ((convert '()) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-anonymous-lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove the anonymous lambda

;; ---------------------------------------------------------------------
;; | INPUT BNF
;; ---------------------------------------------------------------------
;;   Program → Exp
;;   Exp     → var
;;           | (quote immediate)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (letrec ([uvar (lambda (uvar*) body)]*) Exp)
;;           | (lambda (uvar*) body)
;;           | (prim Exp*)
;;           | (Exp Exp* )
;; ---------------------------------------------------------------------
;; | OUTPUT BNF
;; ---------------------------------------------------------------------
;;   Program → Exp
;;   Exp     → var
;;           | (quote immediate)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar < Exp | (lambda (uvar*) body) >]*) Exp)
;;           | (letrec ([uvar (lambda (uvar*) body)]*) Exp)
;;           | (prim Exp*)
;;           | (Exp Exp* )

(define remove-anonymous-lambda
  (lambda (x)
    (define (remove-def exp)
      (match exp
        [(lambda (,fml* ...) ,[remove -> e]) `(lambda (,fml* ...) ,e)]
        [(let ([,x* ,[e*]] ...) ,[remove -> e]) `(let ([,x* ,e*] ...) ,e)]
        [,x (remove x)]))
    (define remove
      (lambda (x)
        (match x
          [(lambda (,x* ...) ,[e])
           (let ([anon (unique-name 'anon)])
             `(letrec ([,anon (lambda (,x* ...) ,e)]) ,anon))]
          [(let ([,x* ,[remove-def -> e*]] ...) ,[e])
           `(let ([,x* ,e*] ...) ,e)]
          [(letrec ([,x* (lambda (,fml* ...) ,[e*])] ...) ,[e])
           `(letrec ([,x* (lambda (,fml* ...) ,e*)] ...) ,e)]
          [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
          [(begin ,[e*] ...) `(begin ,e* ...)]
          [(quote ,imm) `(quote ,imm)]
          [(,[f] ,[a*] ...) `(,f ,a* ...)]
          [,x x])))
    (remove x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sanitize-bindings-forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this pass is to ensure that 'let' form wouldn't bound to lambda expression

;; eg:
;; (let ([uvar < Exp | (lambda (uvar*) body) >]*) Exp)
;; -> (let ([uvar Exp]*) Exp)
;; -> (letrec ([uvar (lambda (uvar*) body)]) Exp)
;; (letrec ([uvar (lambda (uvar*) body)]*) Exp)
;; -> (letrec ([uvar (lambda (uvar*) Exp)]*) Exp)

(define sanitize-binding-forms
  (lambda (x)
    (define (sanitize u* v* exp)
      (let loop ([u* u*] [v* v*] [letrec^ '()] [let^ '()])
        (if (null? u*)
            (let* ([lets    (if (null? let^) exp `(let ,(reverse let^) ,exp))]
                   [letrecs (if (null? letrec^) lets `(letrec ,(reverse letrec^) ,lets))])
              letrecs)
            (cond
              [(not (pair? (car v*)))
               (loop (cdr u*) (cdr v*) letrec^
                 (cons `(,(car u*) ,(car v*)) let^))]
              [(eq? 'lambda (caar v*))
               (loop (cdr u*) (cdr v*) (cons `(,(car u*) ,(car v*)) letrec^)
                 let^)]
              [else
                (loop (cdr u*) (cdr v*) letrec^
                  (cons `(,(car u*) ,(car v*)) let^))]))))
    (match x
      [(let ([,x* ,[v*]] ...) ,[e])
       (if (null? x) e
           (sanitize x* v* e))]
      [(letrec ([,x* ,[v*]] ...) ,[e])
       (if (null? x*) e
           `(letrec ([,x* ,v*] ...) ,e))]
      [(lambda (,x* ...) ,[e]) `(lambda (,x* ...) ,e)]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(begin ,[e*] ...) (make-begin `(,e* ...))]
      [(quote ,imm) `(quote ,imm)]
      [(,[f] ,[a*] ...) `(,f ,a* ...)]
      [,x x])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uncover-free
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find the free-variable and build the lambda expression with free-form
;; (let ([X V] ...) M) → FV((λX.M)V) = (FV(M)-{X})∪FV(V)
;; (letrec ([X V] ...) M) → FV(V)∪FV(M)-{X}
;; transform: (letrec ([uvar (lambda (uvar*) Expr)]*) Expr)
;;         -> (letrec ([uvar (lambda (uvar*) (free (uvar*) Expr))]*) Expr)

(define uncover-free
  (lambda (x)
    (define uncover
      (lambda (x)
        (match x
          [(letrec ([,u* ,[fv* e*]] ...) ,[fv e])
           (values (difference (union (apply union fv*) fv) u*)
             `(letrec ([,u* ,e*] ...) ,e))]
          [(let ([,u* ,[fv* e*]] ...) ,[fv e])
           (values (union (apply union fv*) (difference fv u*))
             `(let ([,u* ,e*] ...) ,e))]
          [(lambda (,u* ...) ,[fv* e])
           (let ([fv (difference fv* u*)])
             (values fv `(lambda (,u* ...) (free ,fv ,e))))]
          [(begin ,[fv* e*] ...)
           (values (apply union fv*) `(begin ,e* ...))]
          [(if ,[tf te] ,[cf ce] ,[af ae])
           (values (union tf cf af) `(if ,te ,ce ,ae))]
          [(quote ,imm)
           (values `() `(quote ,imm))]
          [(,prim ,[fv* e*] ...) (guard (prim? prim))
           (values (apply union fv*) `(,prim ,e* ...))]
          [(,[fv f] ,[fv* a*] ...)
           (values (apply union `(,fv ,fv* ...)) `(,f ,a* ...))]
          [,x (guard (uvar? x)) (values `(,x) x)]
          [,x (values `() x)])))
    (let-values ([(_ exp) (uncover x)]) exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convert-closures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transform: (letrec ([uvar (lambda (uvar*) (free (uvar*) Expr))]*) Expr)
;;         -> (letrec ([uvar (lambda (uvar*) (bind-free (uvar*) Expr))]*)
;;                              (closures ([uvar label uvar*]*) Expr))
(define convert-closures-original
  (lambda (x)
    (define (new-label x)
      (values x (unique-label x)))
    (define (new-cp x)
      (values x (unique-name 'cp)))
    (define convert
      (lambda (x)
        (match x
          [(letrec ([,[new-label -> uvar* label*]
                     (lambda (,x* ...) (free ,[new-cp -> fv* cp*] ,[e*]))] ...) ,[e])
           `(letrec ([,label* (lambda (,cp* ,x* ...)
                                (bind-free (,cp* ,fv* ...) ,e*))] ...)
              (closures ([,uvar* ,label* ,fv* ...] ...) ,e))]
          [(let ([,u* ,[e*]] ...) ,[e]) `(let ([,u* ,e*] ...) ,e)]
          [(begin  ,[e*] ...) `(begin ,e* ...)]
          [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
          [(quote ,imm) `(quote ,imm)]
          [(,prim ,[x*] ...) (guard (prim? prim)) `(,prim ,x* ...)]
          [(,[f] ,[a*] ...)
           (cond
             [(uvar? f) `(,f ,f ,a* ...)]
             [else (let ([tmp (unique-name 'tmp)])
                     `(let ([,tmp ,f])
                        (,tmp ,tmp ,a* ...)))])]
          [,x x])))
    (convert x)))

(define uncover-dynamic
  (lambda (x)
    (define (find-dynamic uvar* fv-lam*)
      (let f ([uvar* uvar*] [fv-lam* fv-lam*] [dynamic* '()])
        (cond
          [(null? uvar*) dynamic*]
          [(null? (car fv-lam*)) (f (cdr uvar*) (cdr fv-lam*) dynamic*)]
          [else (f (cdr uvar*) (cdr fv-lam*) (cons (car uvar*) dynamic*))])))
    (define (uncover lbl*)
      (lambda (x)
        (match x
          [(letrec ([,u* (lambda (,x* ...) ,[(uncover (append u* lbl*)) -> fv* cv* e*])] ...)
             ,[(uncover (append u* lbl*)) -> fv cv e])
           ;; (letrec (U (lambda (x) M)) N)
           ;; free:{ FV(N) - {U} } U { FV(M) - {X} - {U} }
           (let* ([fv-lam*  (map difference fv* x*)]
                  [fv-bind* (map remq u* fv-lam*)]
                  [fv-all*  (union (apply union fv-lam*) fv)]
                  [fv-out*  (difference fv-all* u*)]
                  [cv-bind* (map remq u* cv*)]
                  [cv-out*  (difference (union (apply union cv*) cv) u*)]
                  [dynamic* (union (intersection u* fv-all*) (find-dynamic u* fv-lam*))])
             (values fv-out* cv-out*
               `(letrec ([,u* (lambda (,x* ...) (free ((,fv-bind* ...) (,cv-bind* ...)) ,e*))] ...)
                  (dynamic (,dynamic* ...) ,e))))]
          [(let ([,u* ,[(uncover lbl*) -> fv* cv* e*]] ...)
             ,[(uncover lbl*) -> fv cv e])
           (values
             (union (apply union fv*) (difference fv u*))
             (union (apply union cv*) (difference cv u*))
             `(let ([,u* ,e*] ...) ,e))]
          [(begin ,[(uncover lbl*) -> fv* cv* e*] ...)
           (values
             (apply union fv*)
             (apply union cv*)
             `(begin ,e* ...))]
          [(if ,[(uncover lbl*) -> t-fv t-cv t]
               ,[(uncover lbl*) -> c-fv c-cv c]
               ,[(uncover lbl*) -> a-fv a-cv a])
           (values
             (union t-fv c-fv a-fv)
             (union t-cv c-cv a-cv)
             `(if ,t ,c ,a))]
          [(quote ,imm)
           (values '() '() `(quote ,imm))]
          [(,prim ,[(uncover lbl*) -> fv* cv* a*] ...) (guard (prim? prim))
           (values
             (apply union fv*)
             (apply union cv*)
             `(,prim ,a* ...))]
          [(,f ,[(uncover lbl*) -> fv* cv* a*] ...) (guard (memq f lbl*))
           (values
             (apply union fv*)
             (set-cons f (apply union cv*))
             `(,f ,a* ...))]
          [(,[(uncover lbl*) -> fv cv f] ,[(uncover lbl*) -> fv* cv* a*] ...)
           (values
             (union fv (apply union fv*))
             (union cv (apply union cv*))
             `(,f ,a* ...))]
          [,x (guard (string? x)) (values `() `() x)]
          ;; [,x (guard (uvar? x)) (values `(,x) '() x)]
          [,x (values `(,x) '() x)])))
    (let-values ([(fv* cv* x*) ((uncover '()) x)]) x*)))

(define convert-closures
  (lambda (x)
    (define grow
      (lambda (node* dv*)
        (let loop ([node* node*] [node^ '()] [dv^ dv*] [go #t])
          (match node*
            [() (if go (loop node^ '() dv^ #f) (values dv^ node^))]
            [([,u ((,fv* ...) (,cv* ...))] ,t* ...)
             (let ([fv^ (intersection cv* dv^)])
               (if (null? fv^)
                   (loop `(,t* ...) (append node^ `(,(car node*))) dv^ #f)
                   (let ([fv+fv^ (union fv* fv^)]
                         [cv-fv^ (difference cv* fv^)])
                     (loop `(,t* ...)
                       (append node^ `((,u ((,fv+fv^ ...) (,cv-fv^ ...)))))
                       (set-cons u dv^)
                       #t))))]))))
    (define (convert lbl* dyn*)
      (lambda (map* x)
        (match x
          [(letrec ([,uvar* (lambda (,x* ...)
                                (free ((,fv* ...) (,cv* ...)) ,body*))] ...)
             (dynamic (,dv* ...) ,expr))
           (letv* ([dyn^ (append dv* dyn*)]
                   [(dv^ node^) (grow `([,uvar* ((,fv* ...) (,cv* ...))] ...) dyn^)]
                   [map^ (map (lambda (x) (if (memq x dv^) `((,x . ,(unique-name 'cp))) '()))
                           uvar*)]
                   [cp* (map (lambda (x) (if (null? x) '(dummy) `(,(cdar x)))) map^)]
                   [cpl* (map (lambda (x) (if (null? x) '() `(,(cdar x)))) map^)]
                   [labs (map unique-label uvar*)]
                   [lab^ (append (difference uvar* dv^) lbl*)]
                   [body^ (map (convert lab^ dyn^) map^ body*)]
                   [clo*
                     (begin
                      (apply append
                        (map (lambda (x)
                               (match x
                                 [(,u ((,fv ...) (,cv ...)))
                                  (if (memq u dv^)
                                      (let ([fv^ (map (lambda (x)
                                                        (let ([p (assq x map*)])
                                                          (if p (cdr p) x)))
                                                   fv)])
                                        `([,u ,(unique-label u) ,fv^ ...]))
                                      '())]))
                          node^)))])
             (match node^
               [([,u* ((,fv* ...) (,cv* ...))] ...)
                `(letrec ([,labs (lambda (,@cpl* ,x* ...)
                                   (bind-free (,@cp* ,fv* ...) ,body^)) ] ...)
                   (closures ,clo* ,((convert lab^ dyn^) map* expr)))]))]
          [(let ([,u* ,[e*]] ...) ,[e]) `(let ([,u* ,e*] ...) ,e)]
          [(begin ,[e*] ...) `(begin ,e* ...)]
          [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
          [(quote ,imm) `(quote ,imm)]
          [(,prim ,[a*] ...) (guard (prim? prim)) `(,prim ,a* ...)]
          [(,f ,[a*] ...) (guard (uvar? f))
           (cond
             [(memq f lbl*) `(,(unique-label f) ,a* ...)]
             [(assq f map*) => (lambda (p) `(,(unique-label f) ,(cdr p) ,a* ...))]
             [(memq f dyn*) `(,(unique-label f) ,f ,a* ...)]
             [else `(,f ,f ,a* ...)])]
          [(,[f] ,[a*] ...)
           (let ([tmp (unique-name 'tmp)])
             `(let ([,tmp ,f])
                (,tmp ,tmp ,a* ...)))]
          [,x (guard (string? x)) x]
          [,x
            (cond
              [(assq x map*) => cdr]
              [else x])])))
    (if (*enable-optimize-closures*)
        ((convert '() '()) '() (uncover-dynamic x))
        (convert-closures-original (uncover-free x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; introduce-procedure-primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transform closure form into procedure primitives

;; (lambda (uvar*) (bind-free (dummy) e))
;; -> (lambda (uvar*) new-e)
;;
;; (lambda (closure-point x*) (bind-free (closure-point fv*) e))
;; -> (lambda (closure-point x*) new-e)
;;
;; (closures ((uvar lable fv*)*) exp)
;; -> (let ((uvar (make-procedure label fv*-length)))
;;      (begin
;;        (procedure-set! uvar '0 fv0)
;;        (procedure-set! uvar '1 fv1)
;;        ...
;;        new-exp
;;        )
;;
;; (f f a*)
;; -> ((procedure-code f) f a*)
;;
;; uvar -> if in closure point variable then
;; -> (procedre-ref uvar

;;   Program → Exp
;;   Exp     → label
;;           | uvar
;;           | (quote Imm)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (letrec ([uvar (bind-free (uvar*)
;;           |                  (lambda (uvar*) (closures (cls*) Exp)))] *) Exp)
;;           | (prim Exp*)
;;           | (Exp Exp*)
;;   Imm     → fixnum | () | #t | #f

;;   Program → Exp
;;   Exp     → label
;;           | uvar
;;           | (quote Imm)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (letrec ([uvar (lambda (uvar*) Exp)]*) Exp)
;;           | (prim Exp*)
;;           | (Exp Exp*)
;;   Imm     → fixnum | () | #t | #f

(define introduce-procedure-primitives
  (lambda (x)
    (define (locate-fv cpv)
      (lambda (x)
        (cond
          [(index x (cdr cpv)) =>
           (lambda (i) `(procedure-ref ,(car cpv) ',i))]
          [else x])))

    (define (mk-ref cp)
      (lambda (x)
        (cond
          [(index x (cdr cp)) =>
           (lambda (i) `(procedure-ref ,(car cp) ',i))]
          [else x])))

    (define (mk-set! x free)
      (let loop ([n 0] [ls free] [out '()])
        (cond
          [(null? ls) (reverse out)]
          [else
            (loop (add1 n) (cdr ls)
              (cons `(procedure-set! ,x ',n ,(car ls)) out))])))

    (define (proc-lambda x)
      (match x
        [(lambda (,x* ...) (bind-free (dummy) ,[(intro '(dummy)) -> e]))
         `(lambda (,x* ...) ,e)]
        [(lambda (,cp ,x* ...) (bind-free (,cp ,fv* ...) ,[(intro `(,cp ,@fv*)) -> e]))
         `(lambda (,cp ,x* ...) ,e)]
        [,x (errorf 'introduce-procedure-primitives "invalid lambda ~a" x)]))

    (define (intro ct)
      (lambda (x)
        (match x
          [(letrec ([,lbl* ,[proc-lambda -> e*]] ...) ,[e])
           `(letrec ([,lbl* ,e*] ...) ,e)]
          [(let ([,u* ,[e*]] ...) ,[e])
           `(let ([,u* ,e*] ...) ,e)]
          [(closures ([,u* ,l* ,[fv*] ...] ...) ,[e])
           (let ([len*  (map length fv*)]
                 [pset! (map mk-set! u* fv*)])
             `(let ([,u* (make-procedure ,l* ',len*)] ...)
                (begin
                  ,pset! ... ... ,e)))]
          [(begin ,[e*] ...) `(begin ,e* ...)]
          [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
          [(quote ,imm) `(quote ,imm)]
          [(,f ,[a*] ...) (guard (or (prim? f) (label? f)))
           `(,f ,a* ...)]
          [(,[f] ,[f] ,[a*] ...)
           `((procedure-code ,f) ,f ,a* ...)]
          [,x ((mk-ref ct) x)])))
    ((intro '(dummy)) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lift-letrec
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove the internel letrec form into outermost letrec form

(define lift-letrec
  (lambda (x)
    (define top* '())
    (define add-top*
      (lambda (def) (set! top* (append def top*))))
    (define lift
      (lambda (x)
        (match x
          [,x (guard (atom? x)) x]
          [(letrec ([,lbl* (lambda (,u* ...) ,[e*])] ...) ,[e])
           (begin
             (add-top* `([,lbl* (lambda (,u* ...) ,e*)] ...))
             e)]
          [(,[a] . ,[d]) `(,a . ,d)])))
    (let ([new-bd (lift x)]) `(letrec ,top* ,new-bd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; normalize-context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; distinguiseh instructions into three context: Value, Pred, Effect

;; ---------------------------------------------------------------------
;; | INPUT BNF
;; ---------------------------------------------------------------------
;;   Program → (letrec ([label (lambda (uvar*) Exp)]*) Exp)
;;   Exp     → label
;;           | uvar
;;           | (quote Imm)
;;           | (if Exp Exp Exp)
;;           | (begin Exp* Exp)
;;           | (let ([uvar Exp]*) Exp)
;;           | (prim Exp*)
;;           | (Exp Exp*)
;;   Imm     → fixnum | () | #t | #f
;; ---------------------------------------------------------------------
;; | OUTPUT BNF
;; ---------------------------------------------------------------------
;;   Program → (letrec ([label (lambda (uvar*) Value)]*) Value)
;;   Value   → label
;;           | uvar
;;           | (quote Imm)
;;           | (if Pred Value Value)
;;           | (begin Effect* Value)
;;           | (let ([uvar Value]*) Value)
;;           | (Value-prim Value*)
;;           | (Value Value*)
;;   Pred    → (true)
;;           | (false)
;;           | (if Pred Pred Pred)
;;           | (begin Effect* Pred)
;;           | (let ([uvar Value]*) Pred)
;;           | (pred-prim Value*)
;;   Effect  → (nop)
;;           | (if Pred Effect Effect)
;;           | (begin Effect* Effect)
;;           | (let ([uvar Value]*) Effect)
;;           | (effect-prim Value*)
;;           | (Value Value*)
;;   Imm     → fixnum | () | #t | #f


(define normalize-context
  (lambda (x)
    (define v-prims
      '(* + - / mod div logand logor sra ash void
         car cdr cons
         make-vector vector-length vector-ref
         make-procedure procedure-ref procedure-code
         box unbox))
    (define p-prims
      '(< <= = >= > boolean? eq? fixnum? null? pair? vector? procedure? box? string?))
    (define e-prims
      '(set-car! set-cdr! vector-set! procedure-set! set-box!
         display newline))

    (define (make-nopless-begin x)
      (let ([x (remove '(nop) x)])
        (if (null? x)
            '(nop)
            (make-begin x))))

    (define (norm ct)
      (lambda (x)
        (match x
          ;; basic
          [(letrec ([,lab* (lambda (,u* ...) ,[(norm 'v) -> e*])] ...)
             ,[(norm 'v) -> e])
           `(letrec ([,lab* (lambda (,u* ...) ,e*)] ...) ,e)]
          [(let ([,u ,[(norm 'v) -> v*]] ...) ,[(norm ct) -> e])
           `(let ([,u ,v*] ...) ,e)]
          [(begin ,[(norm 'e) -> e*] ... ,[(norm ct) -> t])
           `(begin ,e* ... ,t)]
          [(if ,[(norm 'p) -> t] ,[(norm ct) -> c] ,[(norm ct) -> a])
           `(if ,t ,c ,a)]

          [(read)
           (case ct
             [v `(read)]
             [p `(if (eq? (read) '#f) (false) (true))]
             [e (make-begin `((read) (nop)))])]

          [(ccall ,x* ...)
           (case ct
             [v `(ccall ,@(map (norm 'v) `(,x* ...)))]
             [p `(if (eq? (ccall ,@(map (norm 'v) `(,x* ...))) '#f) (false) (true))]
             [e `(effect-ccall ,@(map (norm 'v) `(,x* ...)))])]

          ;; Value -> (quote imm)
          [(quote ,x)
           (case ct
             [v `(quote ,x)]
             [p (if (eq? x #f) `(false) `(true))]
             [e `(nop)])]

          ;; prims operation
          [(,vp ,x* ...) (guard (memq vp v-prims))
           (case ct
             [v `(,vp ,@(map (norm 'v) `(,x* ...)))]
             [p `(if (eq? (,vp ,@(map (norm 'v) `(,x* ...))) '#f) (false) (true))]
             [e (make-nopless-begin `(,@(map (norm 'e) `(,x* ...)) (nop)))])]

          [(,pp? ,x* ...) (guard (memq pp? p-prims))
           (case ct
             [v `(if (,pp? ,@(map (norm 'v) `(,x* ...))) '#t '#f)]
             [p `(,pp? ,@(map (norm 'v) `(,x* ...)))]
             [e (make-nopless-begin `(,@(map (norm 'e) `(,x* ...)) (nop)))])]

          [(,ep! ,[(norm 'v) ->  x*] ...) (guard (memq ep! e-prims))
           (case ct
             [v `(begin (,ep! ,x* ...) (void))]
             [p `(begin (,ep! ,x* ...) (true))]
             [e `(,ep! ,x* ...)])]

          ;; Value -> label | string
          [,x (guard (or (label? x) (string? x)))
            (case ct
              [v x]
              [p `(true)]
              [e `(nop)])]
          ;; Value -> uvar
          [,x (guard (uvar? x))
            (case ct
              [v x]
              [p `(if (eq? ,x '#f) (false) (true))]
              [e `(nop)])]

          ;; (V V*) : Effect and Value
          [(,[(norm 'v) -> f] ,[(norm 'v) -> a*] ...)
           (case ct
             [v `(,f ,a* ...)]
             [p `(if (eq? (,f ,a* ...) '#f) (false) (true))]
             [e `(,f ,a* ...)])]
          [,x x])))
    ((norm 'v) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; specify-representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; added the premitive operator into the langugae

;; predictates: eq? null? pair? boolean? fixnum? vector? procedure?
;; numbers    : (* m n) -> (* n (ash m 3))
;; pairs      : cons set-car! set-cdr! car cdr
;; vector     : make-vector vector-set! vector-ref vector-length
;; procedure  : make-procedure procedure-set! procedure-ref procedure-code
;; immdiates  : (quote imm) ~ imm include: number #f #t () (void)
;; void       : (void)
;; io         : read display

(define specify-representation
  (lambda (x)
    (define (trivial? x) (or (number? x) (memq x '($false $true $nil $void))))
    (define offset-car (- disp-car tag-pair))
    (define offset-cdr (- disp-cdr tag-pair))
    (define offset-vector-data (- disp-vector-data tag-vector))
    (define offset-vector-length (- disp-vector-length tag-vector))
    (define offset-procedure-code (- disp-procedure-code tag-procedure))
    (define offset-procedure-length (- disp-procedure-length tag-procedure))
    (define offset-procedure-data (- disp-procedure-data tag-procedure))
    (define offset-box (- disp-box-data tag-box))

    (define (Imm x)
      (match x
        [,n (guard (number? n))
          (ash n shift-fixnum)]
        [,s (guard (string? s))
          `(+ (ash ,s ,shift-fixnum) ,tag-string)
          ;; `(+ (* ,s ,(ash 1 shift-fixnum)) ,tag-string)
          ]
        [#f $false]
        [#t $true]
        [() $nil]
        [(void) $void]
        [,x (errorf 'specify-representation "incorrect immediate ~a" x)]))

    (define (specify x)
      (match x
        ;; basic
        [(if ,[test] ,[conseq] ,[alt]) `(if ,test ,conseq ,alt)]
        [(begin ,[e*] ...) `(begin ,e* ...)]
        [(let ([,u* ,[v*]] ...) ,[bd]) `(let ([,u* ,v*] ...) ,bd)]

        ;; predicates
        [(eq? ,[x] ,[y]) `(= ,x ,y)]
        [(null? ,[e]) (specify `(eq? ,e '()))]
        [(pair? ,[e]) `(= (logand ,e ,mask-pair) ,tag-pair)]
        [(box? ,[e]) `(= (logand ,e ,mask-box) ,tag-box)]
        [(boolean? ,[e]) `(= (logand ,e ,mask-boolean) ,tag-boolean)]
        [(fixnum? ,[e]) `(= (logand ,e ,mask-fixnum) ,tag-fixnum)]
        [(vector? ,[e]) `(= (logand ,e ,mask-vector) ,tag-vector)]
        [(procedure? ,[e]) `(= (logand ,e ,mask-procedure) ,tag-procedure)]
        [(string? ,[e]) `(= (logand ,e ,mask-string) ,tag-string)]

        ;; numbers
        [(,op ,[m] ,[n]) (guard (memq op '(+ - mod < <= >= > =)))
         `(,op ,m ,n)]
        [(sra ,[m] ,[n])
         (cond
           [(number? n) `(sra ,m ,(sra n shift-fixnum))]
           ;; FIXME
           [else (errorf 'specify-representation "not supported yet ~a"
                   `(sra ,m ,n))]
           )]
        [(ash ,[m] ,[n])
         (cond
           [(number? n) `(ash ,m ,(sra n shift-fixnum))]
           [else (errorf 'specify-representation "not supported yet ~a"
                   `(ash ,m ,n))])]
        [(* ,[m] ,[n])
         (cond
           [(number? m) `(* ,n ,(sra m shift-fixnum))]
           [(number? n) `(* ,m ,(sra n shift-fixnum))]
           [else `(* ,m (sra ,n ,shift-fixnum))])]
        [(,div ,[m] ,[n]) (guard (memq div '(/ div)))
         `(ash (,div ,m ,n) ,shift-fixnum)]

        ;; pairs
        [(cons ,[e1] ,[e2])
         (let* ([tmp (unique-name 'pair)]
                [tmp-car (if (trivial? e1) e1 (unique-name 'car))]
                [tmp-cdr (if (trivial? e2) e2 (unique-name 'cdr))]
                [bd-car (if (trivial? e1) '() `((,tmp-car ,e1)))]
                [bd-cdr (if (trivial? e2) '() `((,tmp-cdr ,e2)))]
                [body `(let ([,tmp (+ (alloc ,size-pair) ,tag-pair)])
                         (begin
                           (mset! ,tmp ,offset-car ,tmp-car)
                           (mset! ,tmp ,offset-cdr ,tmp-cdr)
                           ,tmp))])
           (if (and (null? bd-car) (null? bd-cdr)) body
               `(let (,@bd-car ,@bd-cdr) ,body)))]

        [(set-car! ,[e] ,[v]) `(mset! ,e ,offset-car ,v)]
        [(set-cdr! ,[e] ,[v]) `(mset! ,e ,offset-cdr ,v)]
        [(car ,[e]) `(mref ,e ,offset-car)]
        [(cdr ,[e]) `(mref ,e ,offset-cdr)]

        ;; vectors
        [(make-vector ,[k])
         (let ([tmp (unique-name 'vec)])
           (if (integer? k)
               `(let ([,tmp (+ (alloc ,(+ disp-vector-data k)) ,tag-vector)])
                  (begin
                    (mset! ,tmp ,offset-vector-length ,k)
                    ,tmp))
             (let ([tmp2 (unique-name 'vec)])
               `(let ([,tmp ,k]
                      [,tmp2 (+ (alloc (+ ,disp-vector-data ,k)) ,tag-vector)])
                    (begin
                      (mset! ,tmp2 ,offset-vector-length ,tmp)
                      ,tmp2)))))]
        [(vector-set! ,[vec] ,[k] ,[v])
         (if (integer? k)
             `(mset! ,vec ,(+ offset-vector-data k) ,v)
             `(mset! ,vec (+ ,offset-vector-data ,k) ,v))]
        [(vector-ref ,[vec] ,[k])
         (if (integer? k)
             `(mref ,vec ,(+ offset-vector-data k))
             `(mref ,vec (+ ,offset-vector-data ,k)))]
        [(vector-length ,[vec]) `(mref ,vec ,offset-vector-length)]

        ;; procedure
        [(make-procedure ,label ,[size])
         (let ([tmp (unique-name 'proc)])
           `(let ([,tmp (+ (alloc ,(+ disp-procedure-data size)) ,tag-procedure)])
              (begin
                (mset! ,tmp ,offset-procedure-code ,label)
                (mset! ,tmp ,offset-procedure-length ,size)
                ,tmp)))]
        [(procedure-code ,[e]) `(mref ,e ,offset-procedure-code)]
        [(procedure-set! ,[e] ,[k] ,[v]) `(mset! ,e ,(+ offset-procedure-data k) ,v)]
        [(procedure-ref ,[e] ,[k]) `(mref ,e ,(+ offset-procedure-data k))]

        ;; boxes
        [(box ,[e])
         (let* ([tmp   (unique-name 'box)]
                [val (if (trivial? e) e (unique-name 'val))]
                [def (if (trivial? e) '() `((,val ,e)))]
                [exp `(let ([,tmp (+ (alloc ,size-box) ,tag-box)])
                        (begin
                          (mset! ,tmp ,offset-box ,val)
                          ,tmp))])
           (if (null? def) exp `(let ,def ,exp)))]
        [(set-box! ,[e1] ,[e2]) `(mset! ,e1 ,offset-box ,e2)]
        [(unbox ,[e]) `(mref ,e ,offset-box)]

        ;; immediates
        [(quote ,n) (Imm n)]
        [(void) $void]
        [(,[f] ,[a*] ...) `(,f ,a* ...)]
        [,x x]))
    (match x
      [(letrec ([,lbl* (lambda (,u* ...) ,[specify -> bd*])] ...) ,[specify -> bd])
       `(letrec ([,lbl* (lambda (,u* ...) ,bd*)] ...) ,bd)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ucloc&remlet : uncover-locals & remove-let
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uncover the loc* in let form
;; add these into the locals form and remove the let in language
;; let -> | uncover locals | -> | transform to begin | -> begin set!

(define ucloc&remlet
  (lambda (x)
    (define locals* '())
    (define add-locals (lambda (u*) (set! locals* (append u* locals*))))
    (define (uncover1 x)
      (set! locals* '())
      (let ([x^ (make-begin `(,(uncover x)))])
        (values locals* x^)))
    (define uncover
      (lambda (exp)
        (match exp
          [,x (guard (atom? x)) x]
          [(let ([,u* ,[v*]] ...) ,[bd])
           (add-locals u*)
           (let ([new*`((set! ,u* ,v*) ...)])
             (make-begin `(,@new* ,bd)))]
          [(,[a] . ,[d]) `(,a . ,d)])))
    (match x
      [(letrec ([,label* (lambda (,u* ...) ,[uncover1 -> new* bd*])] ...)
         ,[uncover1 -> new bd])
       `(letrec ([,label* (lambda (,u* ...) (locals ,new* ,bd*))] ...)
          (locals ,new ,bd))])))




)
