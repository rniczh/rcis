#!chezscheme
(library (compiler backend)
  (export
    verify-uil
    introduce-cform
    remove-complex-opera*
    impose-calling-conventions

    uncover-frame-conflict
    pre-assign-frame
    assign-new-frame
    finalize-frame-locations
    select-instructions
    uncover-register-conflict
    assign-registers
    everybody-home?
    assign-frame
    finalize-locations

    expose-frame-var
    expose-basic-blocks
    optimize-jumps
    flatten-program
    collect-strings
    generate-x86-64)
  (import
    (chezscheme)
    (compiler setting)
    (framework match)
    (framework helpers)
    )
;;   Program → (letrec ([label (lambda (uvar*) Body)]*) Body)
;;      Body → (locals (uvar*) Tail)
;;      Tail → Triv
;;           | (read)
;;           | (newline)
;;           | (display <uvar|int>)
;;           | (ccall <str> Triv*)
;;           | (alloc Value)
;;           | (mref Value Value)
;;           | (binop Value Value)
;;           | (Value Value*)
;;           | (if Pred Tail Tail)
;;           | (begin Effect* Tail)
;;      Pred → (true)
;;           | (false)
;;           | (read)
;;           | (ccall <str> Triv*)
;;           | (Relop Value Value)
;;           | (if Pred Pred Pred)
;;           | (begin Effect* Pred)
;;    Effect → (nop)
;;           | (newline)
;;           | (display <uvar|int>)
;;           | (effect-ccall <str> Triv*)
;;           | (set! uvar Value)
;;           | (mset! Value Value Value)
;;           | (Value Value*)
;;           | (if Pred Effect Effect)
;;           | (begin Effect* Effect)
;;     Value → Triv
;;           | (read)
;;           | (ccall <str> Triv*)
;;           | (alloc Value)
;;           | (mref Value Value)
;;           | (binop Value Value);
;;           | (Value Value*)
;;           | (if Pred Value Value)
;;           | (begin Effect* Value)
;;      Triv → uvar | int | label

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; verify-uil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-who verify-uil
  (define (binop? x) (memq x '(+ - * / mod div sra ash logand logor mref)))
  (define (relop? x) (memq x '(< > <= >= =)))
  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (cond
          [(null? x*) (void)]
          [(not (x? (car x*))) (errorf who "invalid ~a ~a found" what (car x*))]
          [else (let ([idx (extract-suffix (car x*))])
                  (when (member idx idx*)
                    (errorf who "non-unique ~s suffix ~s cond" what idx))
                  (loop (cdr x*) (cons idx idx*)))]))))
  (define Triv
    (lambda (label* uvar*)
      (lambda (t)
        (cond
          [(and (integer? t) (exact? t))
           (unless (int64? t) (errorf who "integer out of 64-bit range ~a" t))]
          [(uvar? t)
           (unless (memq t uvar*) (errorf who "unbound var ~a" t))]
          [(label? t)
           (unless (memq t label*) (errorf who "unbound label ~a" t))]
          [(string? t) (void)]
          [else (errorf who "invalid Triv ~s" t)])
        (values))))
  (define Value
    (lambda (label* uvar*)
      (lambda (val)
        (match val
          [(read) (values)]
          [(ccall ,fnc ,[(Value label* uvar*) ->] ...)
           (guard (string? fnc)) (values)]
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(alloc ,[]) (values)]
          [(mref ,[] ,[]) (values)]
          [(sra ,[] ,y)
           (unless (uint6? y)
             (errorf who "invalid sra operand ~a" y))
           (values)]
          [(ash ,[] ,y)
           (unless (uint6? y)
             (errorf who "invalid ash operand ~a" y))
           (values)]
          [(,binop ,[] ,[])
           (guard (binop? binop))
           (values)]
          [(,[] ,[] ...) (values)]
          [,[(Triv label* uvar*) ->] (values)]))))
  (define Effect
    (lambda (label* uvar*)
      (lambda (e)
        (match e
          [(nop) (values)]
          [(newline) (values)]
          [(read) (values)]
          [(display ,[(Value label* uvar*) ->]) (values)]
          [(effect-ccall ,fnc ,[(Value label* uvar*) ->] ...)
           (guard (string? fnc)) (values)]
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[] ... ,[]) (values)]
          [(set! ,var ,[(Value label* uvar*) ->])
           (unless (memq var uvar*)
             (errorf who "unbound var ~a" var))
           (values)]
          [(mset! ,[(Value label* uvar*) ->]
             ,[(Value label* uvar*) ->]
             ,[(Value label* uvar*) ->])
           (values)]
          [(,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->] ...) (values)]
          [,e (errorf who "invalid Effect ~a" e)]))))
  (define Pred
    (lambda (label* uvar*)
      (lambda (pr)
        (match pr
          [(true) (values)]
          [(false) (values)]
          [(ccall ,fnc ,[(Value label* uvar*) ->] ...)
           (guard (string? fnc)) (values)]
          [(if ,[] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(,relop ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (guard (relop? relop))
           (values)]
          [,pr (errorf who "invalid Pred ~a" pr)]))))
  (define Tail
    (lambda (label* uvar*)
      (lambda (tail)
        (match tail
          [(read) (values)]
          [(newline) (values)]
          [(display ,[(Value label* uvar*) ->]) (values)]
          [(effect-ccall ,fnc ,[(Value label* uvar*) ->] ...)
           (guard (string? fnc)) (values)]
          [(ccall ,fnc ,[(Value label* uvar*) ->] ...)
           (guard (string? fnc)) (values)]
          [(if ,[(Pred label* uvar*) ->] ,[] ,[]) (values)]
          [(begin ,[(Effect label* uvar*) ->] ... ,[]) (values)]
          [(alloc ,[(Value label* uvar*) ->]) (values)]
          [(mref ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->]) (values)]
          [(sra ,[(Value label* uvar*) ->] ,y)
           (unless (uint6? y)
             (errorf who "invalid sra operand ~a" y))
           (values)]
          [(ash ,[(Value label* uvar*) ->] ,y)
           (unless (uint6? y)
             (errorf who "invalid sra operand ~a" y))
           (values)]
          [(,binop ,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->])
           (guard (binop? binop))
           (values)]
          [(,[(Value label* uvar*) ->] ,[(Value label* uvar*) ->] ...) (values)]
          [,[(Triv label* uvar*) ->] (values)]))))
  (define Body
    (lambda (label* fml*)
      (lambda (x)
        (match x
          [(locals (,local* ...) ,tail)
           (let ([uvar* `(,fml* ... ,local* ...)])
             (verify-x-list uvar* uvar? 'uvar)
             ((Tail label* uvar*) tail)
             (values))]
          [,x (errorf who "invalid Body ~s" x)]))))
  (define lambda-exp
    (lambda (label*)
      (lambda (x)
        (match x
          [(lambda (,fml* ...) ,[(Body label* fml*) ->]) (values)]
          [,x (errorf who "invalid Lambda ~a" x)]))))
  (lambda (x)
    (match x
      [(letrec ([,label* ,[(lambda-exp label*) ->]] ...) ,[(Body label* '()) ->])
       (verify-x-list label* label? 'label)
       (if (*intermediate-output*)
           (call-with-output-file (*intermediate-output*)
             (lambda (p)
               (let f ((ls `(,x)))
                 (if (not (null? ls))
                     (begin
                       (pretty-print (car ls) p)
                       (newline p)
                       (f (cdr ls))))))
             'replace))]
      [,x (errorf who "invalid Program ~s" x)]) x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; introduce-cform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define introduce-cform
  (lambda (p)
    (define ccalls '(display newline read ccall effect-ccall))
    (define (setup-call exp)
      (match exp
        [(display ,x) `(begin (ccall rax "print" ,x))]
        [(newline) `(begin (set! rax 0) (ccall rax "puts" ""))]
        [(read) `(begin (set! rax 0) (ccall rax "read_ptr") rax)]
        [(effect-ccall ,str ,params* ...)
         `(begin (set! rax 0) (ccall rax ,str ,params* ...))]
        [(ccall ,str ,params* ...)
         `(begin (set! rax 0) (ccall rax ,str ,params* ...) rax)]))
    (define intro
      (lambda (exp)
        (match exp
          [,x (guard (atom? x)) x]
          [(,e ,[x] ...) (guard (memq e ccalls))
           (setup-call `(,e ,x ...))]
          [(,[a] . ,[d]) `(,a . ,d)])))
    (match p
      [(letrec ([,lbl* (lambda (,fml* ...) ,[bd*])] ...) ,[bd])
       `(letrec ([,lbl* (lambda (,fml* ...) ,bd*)] ...) ,bd)]
      [(locals (,u* ...) ,[intro -> tail])
       `(locals (,u* ...) ,tail)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-complex-opera*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove the complex operation, eg: (+ (+ 1 2) 3)
;; 1. Begin
;;    (set! a (begin b c d)) -> (begin b c (set! a d))
;;    (f$1 a b (begin c d) e) -> (begin c (set! t.n d) (f$1 a b t.n e))
;;
;; 2. Non tail calls
;;    (f a* ...) -> f into 'func ct, a* ... into 'arg* ct
;;    if the arg be the non tail calls again will be refine: (set! t.n (f a* ...))
;;    and the origin will be (f a1* ... t.n a2* ...)
;;
;; 3. remove the alloc into (set rdx ...)

(define remove-complex-opera*
  (lambda (p)
    (define temp* #f)
    (define (new-t)
      (let ([t (unique-name 't)])
        (set-box! temp* (cons t (unbox temp*)))
        t))
    (define (remove1 p)
      (set! temp* (box '()))
      (let ([tail (make-begin (remove `(,p) #f id))])
        (values (unbox temp*) tail)))
    (define remove
      (lambda (exp ct C)
        (match exp
          ;; argumnet*
          [(,a ,a* ...) (guard (eq? ct 'arg*))
           (remove `(,a) 'arg
             (lambda (ea)
               (remove `(,a* ...) 'arg*
                 (lambda (ea*) (C `(,@ea ,@ea*))))))]

          ;; will be call by begin form
          [(,h ,t ,t* ...)
           (remove `(,h) #f (lambda (eh*) `(,@eh* ,@(remove `(,t ,t* ...) ct C))))]

          [((begin ,e* ... ,t))
           (let ([vt* (remove `(,t) ct
                        (lambda (vt*)
                          (case ct
                            [(func mref arg)
                             (let ([t (new-t)])
                               `((set! ,t ,@vt*) ,@(C `(,t))))]
                            [else (C vt*)])))])
             (remove `(,e* ...) #f (lambda (ve*) `(,@ve* ,@vt*))))]
          [((set! ,x ,y))
           (remove `(,x) #f
             (lambda (ex*)
               (remove `(,y) 'rhs
                 (lambda (ey*) (C `((set! ,@ex* ,@ey*)))))))]
          [((if ,test ,conseq ,alt))
           (case ct
             [(func arg mref)
              (let* ([t (new-t)]
                     [C^ (lambda (et*) `((set! ,t ,@et*)))]
                     [ec* (make-begin (remove `(,conseq) 'rhs C^))]
                     [ea* (make-begin (remove `(,alt) 'rhs C^))])
                (remove `(,test) #f
                  (lambda (et*) `((if ,@et* ,ec* ,ea*) ,@(C `(,t))))))]
             [rhs
               (let ([ec* (make-begin (remove `(,conseq) ct C))]
                     [ea* (make-begin (remove `(,alt) ct C))])
                 (remove `(,test) #f
                   (lambda (et*) `((if ,@et* ,ec* ,ea*)))))]
             [else
               (let ([ec* (make-begin (remove `(,conseq) #f id))]
                     [ea* (make-begin (remove `(,alt) #f id))])
                 (remove `(,test) #f
                   (lambda (et*) (C `((if ,@et* ,ec* ,ea*))))))])]

          ;; allocate n words and return the address
          [((alloc ,msize))
           (if (*enable-gc*)
               (remove `(,msize) 'arg
                 (lambda (em*)
                   (let ([addr (new-t)])
                     `((ccall ,allocation-pointer-register "collect"
                         ,frame-pointer-register
                         ,allocation-pointer-register
                         ,@em*)
                       (set! ,addr ,allocation-pointer-register)
                       (set! ,allocation-pointer-register
                         (+ ,allocation-pointer-register ,@em*))
                       ,@(C `(,addr))))))
               (remove `(,msize) 'arg
                 (lambda (em*)
                   (let ([addr (new-t)])
                     `((set! ,addr ,allocation-pointer-register)
                       (set! ,allocation-pointer-register
                         (+ ,allocation-pointer-register ,@em*))
                       ,@(C `(,addr)))))))]

          ;; exectue the base-expr, offset-expr and expr to generate
          ;; base, offset, val, and store the val at base+offset
          [((mset! ,base ,off ,val))
           (remove `(,base) 'mref
             (lambda (eb*)
               (remove `(,off) 'mref
                 (lambda (eo*)
                   (remove `(,val) 'rhs
                     (lambda (ev*)
                       (C `((mset! ,@eb* ,@eo* ,@ev*)))))))))]
          ;; execute the base-expr, offset-expr get base and offset
          ;; get the value from base+offset addr
          ;; case : 1 | 2
          ;; 1. ct is mref or function -> gen a new uloc
          ;; 2. otherwise ...
          [((mref ,base ,off))
           (remove `(,base) 'mref
             (lambda (eb*)
               (remove `(,off) 'mref
                 (lambda (eo*)
                   (case ct
                     [(func mref)
                      (let ([t (new-t)])
                        `((set! ,t (mref ,@eb* ,@eo*)) ,@(C `(,t))))]
                     [else (C `((mref ,@eb* ,@eo*)))])))))]

          ;; deal with function, eg: (+ ...) (f ...)
          [((,f ,a* ...))
           (remove `(,f) 'func
             (lambda (ef*)
               (remove `(,a* ...) 'arg*
                 (lambda (ea*)
                   (case ct
                     [(func arg mref)
                      (let ([t (new-t)])
                        `((set! ,t (,@ef* ,@ea*)) ,@(C `(,t))))]
                     [else (C `((,@ef* ,@ea*)))])))))]
          [,exp (C exp)])))
    (match p
      [(letrec ([,label* (lambda (,fml* ...) ,[bd*])] ...) ,[bd])
       `(letrec ([,label* (lambda (,fml* ...) ,bd*)] ...) ,bd)]
      [(locals (,loc* ...) ,[remove1 -> new* tail])
       `(locals (,loc* ... ,new* ...) ,tail)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; impose-calling-conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; impose the calling convention on caller/callee

;; 1. (lambda (,fml* ...)
;;      (locals (,loc* ...) ,bd))
;; => (lambda ()
;;      (locals (,loc* ...  rp x0 ... xn-1 xn ... xn+m-1)
;;        (begin
;;          (set! rp ra)
;;          (set! x0 p0)
;;          ...
;;          (set! xn-1 pn-1)
;;          (set! xn fv0)
;;          ...
;;          (set! xn+m-1 fvm-1)
;;          ,bd)))
;; 2. (proc e0 ... en-1 en ... en+k-1) :: tail call
;; => (begin
;;      (set! fv0 en)
;;      ...
;;      (set! fvk-1 en+k-1)
;;      (set! p0 e0)
;;      ...
;;      (set! pn-1 en-1)
;;      (set! ra rp)
;;      (proc fp ra p0 ... pn-1 fv0 ... fvk-1))
;; 3. (proc e0 ... en-1 en ... en+k-1) :: non-tail call
;; => (return-point rp-label
;;      (begin
;;        (set! nfv0 en)
;;        ...
;;        (set! nfvk-1 en+k-1)
;;        (set! p0 e0)
;;        ...
;;        (set!  pn-1 en-1)
;;        (set! ra rp-label)
;;        (proc fp ra p0 ... pn-1 nfv0 ... nfvk-1)))
;; 4. expr
;; => (begin
;;      (set! rv expr)
;;      (rp fp rv))


(define impose-calling-conventions
  (lambda (p)
    (define (call? f ct tail?)
      (and (not (binop? f)) (not (eq? f 'mref)) (eq? (eq? ct 'tail) tail?)))
    (define (new-fv n)
      (unique-name 'nfv))
    (define (load-param fml* fnc pr)
      (let loop ([fml* fml*] [pr pr] [idx 0]
                 [regs '()] [fvs '()])
        (cond
          [(null? fml*) (values (reverse regs) (reverse fvs))]
          [(null? pr)
           (loop (cdr fml*) '() (add1 idx)
             regs (cons `(set! ,(car fml*) ,(fnc idx)) fvs))]
          [else
            (loop (cdr fml*) (cdr pr) idx
              (cons `(set! ,(car fml*) ,(car pr)) regs) fvs)])))
    (define (rev-load regs fvs)
      (let* ([sw (lambda (l) `(set! ,(caddr l) ,(cadr l)))]
             [rr (map sw regs)]
             [rf (map sw fvs)])
        (append rf rr))) ;; reverse the loads

    (define impose
      (lambda (rp ct fv*)
        (lambda (p)
          (match p
            [(begin ,[(impose rp #f fv*) -> e*] ... ,[tail])
             `(begin ,e* ... ,tail)]
            [(if ,[test] ,[conseq] ,[alt])
             `(if ,test ,conseq ,alt)]
            [(,m/set! ,x ... (,op/fnc ,a* ...)) (guard (memq m/set! '(set! mset!)))
             (cond
               [(or (binop? op/fnc) (eq? op/fnc 'mref))
                `(,m/set! ,x ... (,op/fnc ,a* ...))]
               [else
                 (make-begin
                   `(,((impose rp #f fv*) `(,op/fnc ,a* ...))
                     (,m/set! ,x ... ,return-value-register)))])]
            [(,m/set! ,v ... ,vn) (guard (memq m/set! '(set! mset!)))
             `(,m/set! ,v ... ,vn)]
            [(,x) (guard (memq x '(nop true false))) `(,x)]
            [(,relop ,x ,y) (guard (relop? relop))
             `(,relop ,x ,y)]
            [(ccall ,reg ,label ,a* ...) (guard (call? 'ccall ct #t)) ;;tail call
             (let ([rl* (with-values (load-param a* index->frame-var '(rdi rsi rdx rcx r8 r9))
                          rev-load)])
               `(begin
                  ,@rl*
                  (set! ,return-address-register ,rp)
                  (ccall ,reg ,label
                    ,frame-pointer-register
                    ,return-address-register
                    ,@(map cadr rl*))))]
            [(ccall ,reg ,label ,a* ...) ;;non-tail call
             (letv* ([(regs fvs) (load-param a* new-fv '(rdi rsi rdx rcx r8 r9))])
               (let ([rl* (rev-load regs fvs)]
                     [ret (unique-label 'ret)])
                 (set-box! fv* (cons (map caddr fvs) (unbox fv*)))
                 `(return-point ,ret
                    (begin
                      ,@rl*
                      (set! ,return-address-register ,ret)
                      (ccall ,reg ,label
                        ,@(map cadr rl*)
                        ,frame-pointer-register
                        ,return-address-register
                        )))))]
            [(,f ,a* ...) (guard (call? f ct #t)) ;;tail call
             (let ([rl* (with-values (load-param a* index->frame-var parameter-registers)
                          rev-load)])
               `(begin
                  ,@rl*
                  (set! ,return-address-register ,rp)
                  (,f ,frame-pointer-register
                    ,return-address-register
                    ,allocation-pointer-register
                    ,@(map cadr rl*))))]
            [(,f ,a* ...) (guard (call? f ct #f)) ;;non-tail call
             (letv* ([(regs fvs) (load-param a* new-fv parameter-registers)])
               (let ([rl* (rev-load regs fvs)]
                     [ret (unique-label 'ret)])
                 (set-box! fv* (cons (map caddr fvs) (unbox fv*)))
                 `(return-point ,ret
                    (begin
                      ,@rl*
                      (set! ,return-address-register ,ret)
                      (,f ,frame-pointer-register
                        ,return-address-register
                        ,allocation-pointer-register
                        ,@(map cadr rl*))))))]
            [,exp `(begin (set! ,return-value-register ,exp)
                          (,rp ,frame-pointer-register
                            ,return-value-register
                            ,allocation-pointer-register))]))))
    (define Body
      (lambda (bd fml*)
        (match bd
          [(locals (,loc* ...) ,tail)
           (let* ([loads (with-values
                             (load-param fml* index->frame-var parameter-registers)
                           append)]
                  [rp (unique-name 'rp)]
                  [fv* (box '())]
                  [tail ((impose rp 'tail fv*) tail)])
             `(locals (,loc* ... ,@fml* ,rp ,@(apply append (unbox fv*)))
                (new-frames ,(unbox fv*)
                  ,(make-begin
                     `((set! ,rp ,return-address-register)
                       ,@loads
                       ,tail)))))])))

    (match p ;; each lambda-term is callee
      [(letrec ([,label* (lambda (,fml* ...) ,bd*)] ...) ,bd)
       (let ([bd* (map Body bd* fml*)]
             [bd (Body bd '())])
         `(letrec ([,label* (lambda () ,bd*)] ...) ,bd))])))


;; ---- register allocations ----
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; uncover-register-conflict / uncover-frame-conflict
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define uncover-conflict-helper
  (lambda (exp ct with?)
    (define sp* '())
    (define clv* '())
    ;;  add ls into the conflict set of x
    (define add-conflicts
      (lambda (x ls ct)
        (let ([add-ct
                (lambda (x ls ct)
                  (cond
                    [(not (uvar? x)) (void)]
                    [(assq x ct)=>
                     (lambda (slot)
                       (set-cdr! slot (union (cdr slot) (filter with? ls))))]
                    [else (void)]))])
          (add-ct x ls ct)
          (when (with? x)
            (for-each (lambda (y) (add-ct y `(,x) ct)) ls)))))
    (define uncover
      (lambda (exp live* f-live*)
        (match exp
          [((begin ,s ...)) (uncover `(,s ...) live* f-live*)]
          [((if ,test ,conseq ,alt))
           (let ([lc* (uncover `(,conseq) live* f-live*)]
                 [la* (uncover `(,alt) live* f-live*)])
             (uncover `(,test) lc* la*))]
          [((set! ,x (,op ,y ,z)))
           (let ([dx* (difference live* `(,x))])
             (add-conflicts x dx* ct)
             (union dx*
               (uncover `(,y) live* f-live*)
               (uncover `(,z) live* f-live*)))]
          [((set! ,x ,y))
           (let* ([ly* (uncover `(,y) live* f-live*)]
                  [dx* (difference live* `(,x))])
             (add-conflicts x (difference dx* ly*) ct)
             (union dx* ly*))]
          [((mset! ,base ,off ,val))
           (union live*
             (uncover `(,base) live* f-live*)
             (uncover `(,off) live* f-live*)
             (uncover `(,val) live* f-live*))]
          [((return-point ,label ,tail))
           (let ([ul (filter uvar? live*)]
                 [ufl (filter (lambda (x) (or (uvar? x) (frame-var? x))) live*)])
             (set! sp* (union sp* ul))
             (set! clv* (union clv* ufl)))
           (uncover `(,tail) live* f-live*)]
          [((mref ,base ,off))
           (union live*
             (uncover `(,base) live* f-live*)
             (uncover `(,off) live* f-live*))]
          [((true)) live*]
          [((false)) f-live*]
          [((,relop ,x ,y)) (guard (relop? relop))
           (union live* f-live*
             (uncover `(,x) live* f-live*)
             (uncover `(,y) live* f-live*))]
          [((ccall ,reg ,label ,a* ...))
           (let* ([la* (uncover `(,a* ...) live* f-live*)]
                  [dx* (difference live* `(,reg))])
             (add-conflicts reg (difference dx* la*) ct)
             (union dx* la*))]
          [((,t ,cl* ...))
           (union live* (uncover `(,t) live* f-live*) cl*)]
          [(,h ,t ,t* ...)
           (let ([lt* (uncover `(,t ,t* ...) live* f-live*)])
             (uncover `(,h) lt* f-live*))]
          [(,x) (guard (with? x)) `(,x)]
          [,exp '()])))
    (let ([lv* (uncover exp '() '())])
      (values lv* sp* clv*))))

(define uncover-register-conflict
  (lambda (p)
    (match p
      [(letrec ([,label* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [(locals (,loc* ...)
         (ulocals (,uloc* ...)
           (locate (,home* ...)
             (frame-conflict ,fct ,tail))))
       (let* ([ct (map (lambda x x) (union loc* uloc*))]
              [tail (eliminate-deadcode tail uloc*)]
              [with? (lambda (x) (or (uvar? x) (register? x)))]
              [live* (uncover-conflict-helper tail ct with?)])
         `(locals (,loc* ...)
            (ulocals (,uloc* ...)
              (locate (,home* ...)
                (frame-conflict ,fct
                  (register-conflict ,ct ,tail))))))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)])))

(define uncover-frame-conflict
  (lambda (p)
    (match p
      [(letrec ([,label* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [(locals (,uvar* ...)
         (new-frames (,frame* ...) ,tail))
       (let ([ct (map (lambda x x) uvar*)]
             [with? (lambda (x) (or (uvar? x) (frame-var? x)))]
             [tail (eliminate-deadcode tail '())])
         (with-values (uncover-conflict-helper tail ct with?)
           (lambda (lv* sp* clv*)
             `(locals (,@(difference uvar* sp*))
                (new-frames (,frame* ...)
                  (spills ,sp*
                    (frame-conflict ,ct
                      (call-live ,clv*
                        ,tail))))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assign-frame-helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define assign-frame-helper
  (lambda (sp* ct home*)
    (define (homes-of vars* homes*)
      (filter id
        (map (lambda (x)
               (cond
                 [(frame-var? x) x]
                 [(assq x homes*) => cadr]
                 [else #f])) vars*)))
    (define (find-avail used)
      (let loop ([idx 0])
        (let ([fv* (index->frame-var idx)])
          (cond
            [(memq fv* used) (loop (add1 idx))]
            [else fv*]))))
    (define assign
      (lambda (sp* ct home*)
        (let loop ([sp* sp*] [home* home*])
          (if (null? sp*) home*
              (let* ([sp-conflict  (cdr (assq (car sp*) ct))]
                     [spill-locate (homes-of sp-conflict home*)]
                     [avail-fv     (find-avail spill-locate)])
                (loop (cdr sp*)
                  (cons `(,(car sp*) ,avail-fv) home*)))))))
    (assign sp* ct home*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pre-assign-frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; find the avail fv.n for spill variable
(define pre-assign-frame
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [(locals (,loc* ...)
         (new-frames (,nfv* ...)
           (spills (,sp* ...)
             (frame-conflict ,ct
               (call-live (,clv* ...)
                 ,tail)))))
       (let ([locate* (assign-frame-helper sp* ct '())])
         `(locals (,loc* ...)
            (new-frames (,nfv* ...)
              (locate (,locate* ...)
                (frame-conflict ,ct
                  (call-live (,clv* ...)
                    ,tail))))))]
      [,x (errorf 'pre-assign-frame "invalid Program ~s" x)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assign-frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  placed spilled variables onto the stack frame
(define assign-frame
  (lambda (x)
    (match x
      [(letrec ([,lbl* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,lbl* (lambda () ,bd*)] ...) ,bd)]
      [(locals (,loc* ...)
         (ulocals (,uloc* ...)
           (spills (,spill* ...)
             (locate (,home* ...)
               (frame-conflict ,ct ,tail)))))
       (let ([locate* (assign-frame-helper spill* ct home*)])
         `(locals (,loc* ...)
            (ulocals (,uloc* ...)
              (locate (,locate* ...)
                (frame-conflict ,ct ,tail)))))]
      [(locate (,home* ...) ,bd) `(locate (,home* ...) ,bd)]
      [,p (errorf 'assign-frame "invalid program ~a" p)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assign-new-frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eliminate the call-live form and new-frames form

(define assign-new-frame
  (lambda (p)
    (define (frame-size clv* home*)
      (let ([idx* (map (lambda (x) (frame-var->index (cadr (assq x home*)))) clv*)])
        (if (null? idx*) 0 (add1 (apply max idx*)))))

    (define (do-assign fs)
      (lambda (nfv*)
        (let f ([nfv* nfv*] [idx fs] [assigned '()])
          (cond
            [(null? nfv*) assigned]
            [else (f (cdr nfv*) (add1 idx)
                    (cons `(,(car nfv*) ,(index->frame-var idx)) assigned))]))))

    (define Tail
      (lambda (fs p)
        (match p
          [,x (guard (atom? x)) x]
          [(return-point ,lbl ,tail)
           (let ([bn (fxsll fs align-shift)])
             `(begin (set! ,frame-pointer-register (+ ,frame-pointer-register ,bn))
                     (return-point ,lbl ,tail)
                     (set! ,frame-pointer-register (- ,frame-pointer-register ,bn))))]
          [(,[a] . ,[d]) `(,a . ,d)])))

    (match p
      [(letrec ([,lbl* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,lbl* (lambda () ,bd*)] ...) ,bd)]
      [(locals (,loc* ...)
         (new-frames (,nfv* ...)
           (locate (,home* ...)
             (frame-conflict ,ct
               (call-live (,clv* ...)
                 ,tail)))))
       (let ([fs (frame-size clv* home*)])
         `(locals (,(difference loc* `(,nfv* ... ...)) ...)
            (ulocals ()
              (locate (,home* ... ,(map (do-assign fs) nfv*) ... ...)
                (frame-conflict ,ct ,(Tail fs tail))))))]
      [,x (errorf 'assign-new-frame "invalid program ~s" x)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finalize-frame-locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define finalize-frame-locations
  (lambda (p)
    (define (lookup v env)
      (cond
        [(assq v env) => cdr]
        [else v]))
    (define finalize
      (lambda (p env)
        (match p
          [,u (guard (uvar? u)) (lookup u env)]
          [,x (guard (atom? x)) x]
          [(set! ,[a] ,[b])
           (if (eq? a b) `(nop) `(set! ,a ,b))]
          [(,[a] . ,[d]) `(,a . ,d)])))
    (match p
      [(letrec ([,lbl* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,lbl* (lambda () ,bd*)] ...) ,bd)]
      [(locals (,loc* ...)
         (ulocals (,uloc* ...)
           (locate ([,u* ,v*] ...)
             (frame-conflict ,ct ,tail))))
       (let ([tail^ (finalize tail `((,u* . ,v*) ...))])
         `(locals (,loc* ...)
            (ulocals (,uloc* ...)
              (locate ([,u* ,v*] ...)
                (frame-conflict ,ct ,tail^)))))]
      [(locate ([,u* ,v*] ...) ,tail)
       `(locate ([,u* ,v*] ...) ,(finalize tail `((,u* . ,v*) ...)))
       ])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; select-instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; conform the input language into x86-64 rules
(define select-instructions
  (lambda (x)
    (define temp* #f)
    (define (new-u)
      (let ([u (unique-name 'u)])
        (set-box! temp* (cons u (unbox temp*)))
        u))
    (define int64/label?
      (lambda (x)
        (or (and (int64? x) (not (int32? x)))
            (label? x))))
    (define (ur? x)
      (or (register? x) (uvar? x)))
    (define mem?
      (lambda (x)
        (or (frame-var? x) (mref? x))))
    (define (commut? op)
      (or (memq op '(+ * logor logand))
          (relop? op)))
    (define (flip-op op)
      (cond
        [(relop? op)
         (cdr (assq op '((= . =) (<= . >=) (>= . <=) (< . >) (> . <))))]
        [(binop? op) op]))

    (define reselect
      (lambda (exp) (select exp #f id)))
    (define select1
      (lambda (exp)
        (set! temp* (box '()))
        (let ([exp* (make-begin (select `(,exp) #f id))])
          (values (unbox temp*) exp*))))
    (define select
      (lambda (x ct C)
        (match x
          [((begin ,e* ...))
           (select `(,e* ...) #f C)]
          [(,h ,t ,t* ...)
           (select `(,h) #f
             (lambda (vh*) `(,@vh* ,@(select `(,t ,t* ...) #f C))))]
          [((if ,test ,conseq ,alt))
           (let ([ec (make-begin (select `(,conseq) #f id))]
                 [ea (make-begin (select `(,alt) #f id))])
             (select `(,test) #f
               (lambda (vt*) (C `((if ,@vt* ,ec ,ea))))))]
          [((return-point ,label ,tail))
           (C `((return-point ,label
                  ,(make-begin (reselect `(,tail))))))]
          [((set! ,x ,y))
           (C (select `(,x) #f
                (lambda (vx*)
                  (select `(,y) x
                    (lambda (vy*)
                      (let ([x (car vx*)] [y (car vy*)])
                        (cond
                          [(and (mem? x)
                                (or (mem? y) (int64/label? y)))
                           (let ((u (new-u)))
                             `((set! ,u ,y) (set! ,x ,u)))]
                          [else `((set! ,x ,y))])))))))]

          [((mref ,base ,off))
           (select `(,base) 'mref
             (lambda (vb*)
               (select `(,off) 'mref
                 (lambda (vo*)
                   (let ([b (car vb*)] [o (car vo*)])
                     (cond
                       [(and (or (integer? b) (frame-var? b))
                             (integer? o))
                        (let ([u (new-u)])
                          `((set! ,u ,b)
                            ,@(C `((mref ,u ,o)))))]
                       [else (C `((mref ,b ,o)))]))))))]

          [((mset! ,base ,off ,val))
           (select `(,base) 'mref
             (lambda (vb*)
               (select `(,off) 'mref
                 (lambda (vo*)
                   (select `(,val) `(mref ,@vb* ,@vo*)
                     (lambda (vv*)
                       (let ([b (car vb*)] [o (car vo*)] [v (car vv*)])
                         (cond
                           [(mem? v)
                            (let ([u (new-u)])
                              `((set! ,u ,v)
                                ,@(C `((mset! ,b ,o ,u)))))]
                           [else (C `((mset! ,b ,o ,v)))]))))))))]

          [((,div ,x ,y)) (guard (memq div '(/ mod div)))
           (select `(,x) #f
             (lambda (vx*)
               (select `(,y) #f
                 (lambda (vy*)
                   (let ([x (car vx*)] [y (car vy*)])
                     (cond
                       [(int64/label? x)
                        (let ([u (new-u)])
                          (reselect `((set! ,u ,x) ,@(C `((,div ,u ,y))))))]
                       [(int64/label? y)
                        (let ([u (new-u)])
                          (reselect `((set! ,u ,y) ,@(C `((,div ,x ,u))))))]
                       [(equal? ct x)
                        (cond
                          [(eq? x 'rax) (C `((/ ,x ,y)))]
                          [else
                            (let ([u (new-u)])
                              (cond
                                [(memq div '(/ div))
                                 (if (register? y)
                                     (reselect
                                       `((set! ,u ,y)
                                         (set! rax ,x)
                                         (set! rdx 0)
                                         (set! rax (,div rax ,u))
                                         (nop rdx rax) ;; for live
                                         ,@(C `(rax))))
                                     (reselect
                                       `((set! ,u ,y)
                                         (set! ,u (nop ,u ,u))
                                         (set! rax ,x)
                                         (set! rdx 0)
                                         (set! rax (,div rax ,u))
                                         (nop rdx rax) ;; for live
                                         ,@(C `(rax)))))]
                                [else ;; mod
                                  (if (register? y)
                                     (reselect
                                       `((set! ,u ,y)
                                         (set! rax ,x)
                                         (set! rdx 0)
                                         (set! rax (,div rax ,u))
                                         (nop rdx rax) ;; for live
                                         ,@(C `(rax))))
                                     (reselect
                                        `((set! ,u ,y)
                                          (set! ,u (nop ,u ,u))
                                          (set! rax ,x)
                                          (set! rdx 0)
                                          (set! rax (,div rax ,u))
                                          (set! rdx (nop rdx rax))
                                          ,@(C `(rdx))
                                          )))]))])]
                       [else
                         (let ([u (new-u)])
                           (reselect `((set! ,u ,x)
                                       (set! ,u (,div ,u ,y))
                                       ,@(C `(,u)))))]))))))]


          [((,binop ,x ,y)) (guard (binop? binop))
           (select `(,x) #f
             (lambda (vx*)
               (select `(,y) #f
                 (lambda (vy*)
                   (let ([x (car vx*)] [y (car vy*)])
                     (cond
                       [(int64/label? x)
                        (let ([u (new-u)])
                          (reselect `((set! ,u ,x) ,@(C `((,binop ,u ,y))))))]
                       [(int64/label? y)
                        (let ([u (new-u)])
                          (reselect `((set! ,u ,y) ,@(C `((,binop ,x ,u))))))]
                       [(equal? ct x)
                        (cond
                          [(and (eq? binop '*) (mem? x))
                           (let ([u (new-u)])
                             (reselect `((set! ,u ,x)
                                         (set! ,u (,binop ,u ,y))
                                         ,@(C `(,u)))))]
                          [(and (mem? x) (mem? y))
                           (let ([u (new-u)])
                             (reselect `((set! ,u ,y)
                                         ,@(C `((,binop ,x ,u))))))]
                          [else (C `((,binop ,x ,y)))])]
                       [(and (equal? ct y) (commut? binop))
                        (reselect (C `((,binop ,y ,x))))]
                       [else
                         (let ([u (new-u)])
                           (reselect `((set! ,u ,x)
                                       (set! ,u (,binop ,u ,y))
                                       ,@(C `(,u)))))]))))))]

          [((,relop ,x ,y)) (guard (relop? relop))
           (select `(,x) #f
             (lambda (vx*)
               (select `(,y) #f
                 (lambda (vy*)
                   (let ([x (car vx*)] [y (car vy*)])
                     (cond
                       [(and (not (ur? x)) (ur? y))
                        (reselect (C `((,(flip-op relop) ,y ,x))))]
                       [(int64/label? y)
                        (let ([u (new-u)])
                          (reselect `((set! ,u ,y) ,@(C `((,relop ,x ,u))))))]
                       [(or (integer? x) (and (mem? x) (mem? y)))
                        (let ([u (new-u)])
                          (reselect `((set! ,u ,x) ,@(C `((,relop ,u ,y))))))]
                       [else (C `((,relop ,x ,y)))]))))))]
          [(,exp)
           (cond
             [(or (and (eq? ct 'mref)
                       (or (mem? exp) (label? exp)))
                  (and (mem? ct)
                       (or (mem? exp) (int64/label? exp))))
              (let ((u (new-u)))
                `((set! ,u ,exp) ,@(C `(,u))))]
             [else (C `(,exp))])])))
    (match x
      [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [(locals (,local* ...)
         (ulocals (,ulocal* ...)
           (locate (,home* ...)
             (frame-conflict ,ct ,[select1 -> new* tail]))))
       `(locals (,local* ...)
          (ulocals (,ulocal* ... ,new* ...)
            (locate (,home* ...)
              (frame-conflict ,ct ,tail))))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
      [,x (errorf 'select-instructions "invalid program ~a" x)])))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; assign-registers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define assign-registers
  (lambda (x)
    (define adjust-ct
      (lambda (ct uloc*)
        (let* ([ut (map (lambda (x) (assq x ct)) uloc*)]
               [ct\ut
                 (let f ([ct (difference ct ut)] [out '()])
                   (if (null? ct) out
                       (let ([x1 (find-min length ct)])
                         (f (ct-remove-node (car x1) ct) (cons x1 out)))))])
          (append ut ct\ut))))

    (define find-used
      (lambda (vars* home*)
        (let f ([vars* vars*] [used '()])
          (cond
            [(null? vars*) used]
            [(register? (car vars*))
             (f (cdr vars*) (cons (car vars*) used))]
            [(assq (car vars*) home*) =>
             (lambda (x) (f (cdr vars*) (cons (cdr x) used)))]
            [else (f (cdr vars*) used)]))))

    (define (assign regs)
      (lambda (ct)
        (let f ([ct ct] [home* '()])
          (if (null? ct) (alist->list home*)
              (let* ([regs-used* (find-used (cdar ct) home*)]
                     [avail-reg* (difference regs regs-used*)])
                (cond
                  [(null? avail-reg*) (f (cdr ct) home*)]
                  [else (f (cdr ct)
                          (cons (cons (caar ct) (car avail-reg*)) home*))]))))))
    (match x
      [(letrec ([,lbl* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,lbl* (lambda () ,bd*)] ...) ,bd)]
      [(locals (,loc* ...)
         (ulocals (,uloc* ...)
           (locate (,frame-home* ...)
             (frame-conflict ,fct
               (register-conflict ,ct ,tail)))))
       (let* ([uvar* (append loc* uloc*)]
              [home* ((assign registers) (adjust-ct ct uloc*))]
              [sp* (difference uvar* (map car home*))])
         (cond
           [(null? sp*) `(locate (,frame-home* ... ,home* ...) ,tail)]
           [(null? (intersection uloc* sp*))
            (let ([loc* (difference loc* sp*)])
              `(locals (,loc* ...)
                 (ulocals (,uloc* ...)
                   (spills (,sp* ...)
                     (locate (,frame-home* ...)
                       (frame-conflict ,fct ,tail))))))]
           [else (errorf 'assign-registers
                   "unspillable variabels (~a) have been spilled"
                   (intersection uloc* sp*))]))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
      [,x (errorf 'assign-registers "invalid Program ~s" x)])))

(define-who everybody-home?
  (define all-home?
    (lambda (body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (spills (,spill* ...)
               (locate (,home* ...)
                 (frame-conflict ,ct ,tail))))) #f]
        [(locate (,home* ...) ,tail) #t]
        [,x (errorf who "invalid Body ~a" x)])))
  (lambda (x)
    (match x
      [(letrec ([,label* (lambda () ,body*)] ...) ,body)
       (andmap all-home? `(,body ,body* ...))]
      [,x (errorf who "invalid Program ~a" x)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; finalize-locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove the locate form and
;; replaced all locations variables with the registers or frame-vars

(define finalize-locations
  (lambda (p)
    (define (lookup v env)
      (cond
        [(assq v env) => cdr]
        [else (errorf 'finalize-locations "undefined variable ~a" v)]))
    (define (optimize-nops exp)
      (match exp
        [,x (guard (atom? x)) x]
        [(set! ,a ,b) (guard (eq? a b)) '(nop)]
        [(set! ,a (/ ,b 0)) (errorf 'finalize-locations "invalid div operand 0")]
        [(set! ,a (,op ,b 0)) (guard (memq op '(+ - sra ash))) '(nop)]
        [(set! ,a (* ,b 1)) '(nop)]
        [(,[a] . ,[d]) `(,a . ,d)]))
    (define finalize
      (lambda (p env)
        (match p
          [(begin ,[e*] ...) `(begin ,e* ...)]
          [(if ,[test] ,[conseq] ,[alt]) `(if ,test ,conseq ,alt)]
          [(return-point ,lab ,[tail]) `(return-point ,lab ,tail)]
          [(set! ,[x] (,binop ,[y] ,[z])) `(set! ,x (,binop ,y ,z))]
          [(set! ,[x] ,[y]) `(set! ,x ,y)]
          [(mset! ,[base] ,[off] ,[val]) `(mset! ,base ,off ,val)]
          [(mref ,[base] ,[off]) `(mref ,base ,off)]
          [(,op ,[x] ,[y]) (guard (or (binop? op) (relop? op)))
           `(,op ,x ,y)]
          [(ccall ,[reg] ,[label] ,[live*] ...) `(ccall ,reg ,label)]
          [(,[triv] ,[live*] ...) `(,triv)]
          [,v (guard (uvar? v)) (lookup v env)]
          [,x x])))
    (match p
      [(letrec ([,label* (lambda () ,[bd*])] ...) ,[bd])
       `(letrec ([,label* (lambda () ,bd*)] ...) ,bd)]
      [(locate ([,u* ,v*] ...) ,tail)
       (let ([exp (finalize tail `((,u* . ,v*) ...))])
         (if (*enable-optimize-nops*) (optimize-nops exp) exp))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expose-frame-var
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; converts the frame variable fvar into explicit memory operands
;; This pass turns fv0, fv1, ... into displacement forms.
;; fv1 → #<disp rbp 8>

(define expose-frame-var
  (lambda (p)
    (define fp frame-pointer-register)
    (define (walk1 exp)
      (let-values ([(exp rest) (walk exp 0)])
        exp))
    (define (walk exp fv)
      (match exp
        [(set! ,x (,op ,x ,v)) (guard (eq? x fp))
         (values exp ((eval op) fv v))]
        [,v (guard (frame-var? v))
          (values
            (make-disp-opnd fp (- (fxsll (frame-var->index v) align-shift) fv))
            fv)]
        [,v (guard (atom? v))
          (values exp fv)]
        [(,a . ,d)
         (letv* ([(ae nfv) (walk a fv)]
                 [(de ret) (walk d nfv)])
           (values `(,ae . ,de) ret))]))
    (match p
      [(letrec ([,lbl* (lambda () ,[walk1 -> tail*])] ...) ,[walk1 -> tail])
       `(letrec ([,lbl* (lambda () ,tail*)] ...) ,tail)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expose-basic-blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; eliminate ccall form:
;; converting the ccall into normal call
;; (ccall reg label Triv*) -> 1 | 2
;; 1: (call label) (r15)
;; 2: (call label) (set! reg rax) (r15)
;; depend on reg, 1 if reg equal to rax

;; basic-block:
;; converting the if expression into basic blocks
;; (if Pred Effect Effect) convert into (if (relop Triv Triv) (label) (label))

(define expose-basic-blocks
  (lambda (p)
    (define fp frame-pointer-register)
    (define ra return-address-register)
    (define rv return-value-register)
    (define new* '())
    (define (add-def def)
      (set! new* (union `(,def) new*)))
    (define (mk-def lbl seq)
      (match (shortcut seq)
        [() '()]
        [((,triv)) (guard (and (*enable-optimize-jumps*) (label? triv)))
         `((,triv))]
        [,seq
          (let ([lbl (if (label? lbl) lbl (unique-label lbl))])
            (add-def `(,lbl (lambda () ,(make-begin seq))))
            `((,lbl)))]))
    (define (shortcut x)
      (define (cut p)
        (match p
          [(if (true) ,a ,b) a]
          [(if (false) ,a ,b) b]
          [,p p]))
      (map cut x))
    (define expose1 (lambda (p) (make-begin (expose `(,p) id))))
    (define expose
      (lambda (exp C)
        (match exp
          [((begin ,e* ...)) (expose `(,e* ...) C)]
          [((if ,t ,c ,a) ,e* ...)
           (let* ([er* (if (null? e*) '() (mk-def 'j (expose `(,e* ...) C)))]
                  [C^ (if (null? e*) C (lambda (eh*) (C `(,@eh* ,@er*))))]
                  [ec* (mk-def 'c (expose `(,c) C^))]
                  [ea* (mk-def 'a (expose `(,a) C^))])
             (expose `(,t) (lambda (et*) (shortcut `((if ,@et* ,@ec* ,@ea*))))))]
          [((return-point ,lbl ,t) ,t* ...)
           (begin
             (mk-def lbl (expose `(,t* ...) C))
             (expose `(,t) id))]
          [((nop)) (C '())]
          [((ccall ,reg ,label))
           (if (eq? reg rv)
               (expose `((begin (call ,label) (,ra))) C)
               (expose `((begin (call ,label) (set! ,reg ,rv) (,ra))) C))]
          [(,h ,t ,t* ...)
           (expose `(,h) (lambda (eh*) `(,@eh* ,@(expose `(,t ,t* ...) C))))]
          [,exp (C exp)])))
    (match p
      [(letrec ([,lbl (lambda () ,[expose1 -> e*])] ...) ,[expose1 -> e])
       `(letrec ([,lbl (lambda () ,e*)] ... ,new* ...) ,e)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; optimize-jumps
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define optimize-jumps
  (lambda (p)
    (define dumb-jumps (make-parameter #f))
    (define (walk v env)
      (cond
        [(assq v env) =>
         (lambda (s)
           (let ([q (walk (cdr s) env)])
             (set-cdr! s q) q))]
        [else v]))
    (define build-jumps
      (lambda (bd)
        (define build
          (lambda (bd new-bd* env)
            (match bd
              [() (values new-bd* env)]
              [([,a (lambda () (,b))] ,bd* ...) (guard (label? b))
               (let ([b^ (walk b env)])
                 (cond
                   [(eq? a b^) (dumb-jumps a)
                    (build `(,bd* ...) (cons `(,a (lambda () (,a))) new-bd*) env)]
                   [else
                     (build `(,bd* ...) new-bd* (cons `(,a . ,b^) env))]))]
              [(,a . ,d)
               (build d (cons a new-bd*) env)])))
        (build bd '() '())))
    (define (opt env)
      (lambda (p)
        (match p
          [,v (guard (label? v))
            (walk v env)]
          [,x (guard (atom? x)) x]
          [(,[a] . ,[d]) `(,a . ,d)])))
    (match p
      [(letrec (,def* ...) ,tail)
       (if (*enable-optimize-jumps*)
           (let-values ([(new-def* env) (build-jumps def*)])
             (match new-def*
               [([,lbl* (lambda () ,[(opt env) -> tail*])] ...)
                (if (dumb-jumps)
                    (display (format "Warning: find a infinite loop: ~a\n"
                               (extract-root (dumb-jumps)))))
                `(letrec ([,lbl* (lambda () ,tail*)] ...) ,((opt env) tail))]))
           p)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flatten-program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; falttens the code into a straight sequence of labels and statements
;; This pass turns letrec form into formatted assembly code
;; eg:
;; (letrec ([double$1
;;            (lambda ()
;;              (begin
;;                (set! rsi #<disp rbp 0>)
;;                (set! rsi (+ rsi #<disp rbp 0>))
;;                (set! rdi rsi)
;;                (call print_int)
;;                (r15)))])
;;   (begin
;;     (set! #<disp rbp 0> 5)
;;     (set! rdi #<disp rbp 0>)
;;     (double$1)))
;; =>
;; (code
;;   (set! #<disp rbp 0> 5)
;;   (set! rdi #<disp rbp 0>)
;;   double$1
;;   (set! rsi #<disp rbp 0>)
;;   (set! rsi (+ rsi #<disp rbp 0>))
;;   (set! rdi rsi)
;;   (call print_int)
;;   (jump r15))
;;
;; ---------------------------------------------------------------------
;; | INPUT BNF
;; ---------------------------------------------------------------------
;; Program → (letrec ([label (lambda () Tail)]*) Tail)
;;    Tail → (Triv)
;;         | (call label)
;;         | (if (Relop Triv Triv) (jump Triv))
;;         | (begin Effect* Tail)
;;  Effect → (set! Triv (Binop Triv Triv))
;;         | (set! Triv Triv)
;;         | (mset! Triv Triv (Binop (mref (Triv Triv) Triv)))
;;         | (mset! Triv Triv Triv)
;;         | (push <Triv|Loc>)
;;         | (pop  Loc)
;;     Loc → reg
;;    Triv → Loc | int | label | string
;; ---------------------------------------------------------------------
;; | OUTPUT BNF
;; ---------------------------------------------------------------------
;; Program → ((code Stmt*) section*)
;;    Stmt → (set! Triv (Binop Triv Triv))
;;         | (set! Triv Triv)
;;         | (mset! Triv Triv (Binop (mref (Triv Triv) Triv)))
;;         | (mset! Triv Triv Triv)
;;         | (if (Relop Triv Triv) (jump Triv))
;;         | (jump Triv)
;;         | (push Triv)
;;         | (pop  Triv)
;;         | (call label)
;;    Triv → Var | int | label | string
;; ---------------------------------------------------------------------


(define flatten-program
  (lambda (p)
    (define proc-def
      (lambda (def*)
        (match def*
          [() '()]
          [(,def) `(,(flatten def #f))]
          [(,def1 [,lbl* (lambda () ,e*)] ...)
           (let ([lbl2 (car lbl*)])
             `(,(flatten def1 lbl2) ,@(proc-def `([,lbl* (lambda () ,e*)] ...))))])))
    (define flatten
      (lambda (x next)
        (match x
          [(letrec ,[proc-def -> def*] ,[tail])
           (let ([tail (if (null? def*) tail
                           (match tail
                             [(,st* ... (jump ,var))
                              (guard (eq? var (caar def*)))
                              `(,st* ...)]
                             [,tail tail]))])
             `(code ,@tail ,def* ... ...))]
          [(,lbl* (lambda () ,[tail*])) `(,lbl* ,@tail*)]
          [(begin ,[e*] ...) `(,e* ... ...)]
          [(if ,t (,c) (,a))
           (cond
             [(eq? next a) `((if ,t (jump ,c)))]
             [(eq? next c) `((if (not ,t) (jump ,a)))]
             [else `((if ,t (jump ,c)) (jump ,a))])]
          [(,t) (guard (or (disp-opnd? t) (register? t) (label? t)))
           (if (eq? t next) '() `((jump ,t)))]
          [,p `(,p)])))
    (flatten p #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect-strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; finding out the imutable strings and create a section form for store it.
;; goal: ((code stmt*) sec*) -> (code stmt*)
;; origin: (set! rax "hello world")
;; after : (set! rax .string$3)
;;         and create a section form:
;;                  .string$3
;;                  (string "hellow world")
;;                  (text)

(define collect-strings
  (lambda (exp)
    (define new-sec* '())
    (define (add-string str)
      (let ([sec (symbol-append (string->symbol ".") (unique-label 'string))])
        (set! new-sec* (cons* sec `(string ,str) `(text) new-sec*))
        sec))
    (define collect
      (lambda (exp)
        (match exp
          [(set! ,loc* ,str) (guard (string? str))
           (let ([sec (add-string str)])
             `(set! ,loc* ,sec))]
          [,exp exp])))
    (match exp
      [(code ,[collect -> s*] ...) `((code ,s* ...) ,new-sec* ...)]
      [,x (errorf 'collect-strings "invalid program ~a" x)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eliminate-deadcode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eliminate-deadcode
  (lambda (x uloc*)
    (if (*enable-optimize-deadcode*)
        (backward-delete (forward-propagate x uloc*))
        x)))

(define forward-propagate
  (lambda (x uloc*)
    (define (ext x v env)
      (cond
        [(eq? x v) env]
        [(not (pair? v))
         (cons `(,x . ,v) env)]
        [else env]))
    (define (lookup v env)
      (cond
        [(assq v env) => cdr]
        [else v]))
    (define (mem? x)
      (or (mref? x) (frame-var? x) (label? x)))
    (define eliminate
      (lambda (x env)
        (filter (lambda (p)
                  (and (not (or (eq? x (cdr p)) (eq? x (car p))))
                       (not (and (eq? x frame-pointer-register)
                               (or (frame-var? (car p))
                                   (frame-var? (cdr p)))))
                       (not (and (memq x uloc*)
                               (memq (cdr p) uloc*)))))
          env)))
    (define forward
      (lambda (x env lhs)
        (match x
          [((begin ,e* ...))
           (let-values ([(ve* new-env) (forward `(,e* ...) env #f)])
             (values `(,(make-begin ve*)) new-env))]
          [((if ,t ,c ,a))
           (letv* ([(vt envt) (forward `(,t) env #f)]
                   [(vc envc) (forward `(,c) envt #f)]
                   [(va enva) (forward `(,a) envt #f)])
             (match vt
               [((true))  (values vc envc)]
               [((false)) (values va enva)]
               [,vt (values `((if ,@vt ,@vc ,@va)) (intersection envc enva))]))]
          [((set! ,x ,y))
           (if (mem? y)
               (values `((set! ,x ,y)) env)
               (let-values ([(ey* new-env) (forward `(,y) env x)])
                 (values `((set! ,x ,@ey*)) (ext x (car ey*) (eliminate x env)))))]
          [((mset! ,base ,off ,val))
           (letv* ([(eb* _) (forward `(,base) env #f)]
                   [(eo* _) (forward `(,off) env #f)]
                   [(ev* _) (forward `(,val) env `(mref ,@eb* ,@eo*))])
             (let ([eb* (if (mem? (car eb*)) `(,base) eb*)]
                   [eo* (if (mem? (car eo*)) `(,off) eo*)]
                   [ev* (if (mem? (car ev*)) `(,val) ev*)])
               (values `((mset! ,@eb* ,@eo* ,@ev*)) env)))]
          [((mref ,base ,off))
           (letv* ([(eb* _) (forward `(,base) env #f)]
                   [(eo* _) (forward `(,off) env #f)])
             (let ([eb* (if (mem? (car eb*)) `(,base) eb*)]
                   [eo* (if (mem? (car eo*)) `(,off) eo*)])
               (values `((mref ,@eb* ,@eo*)) env)))]
          [((return-point ,lbl ,tail))
           (letv* ([(et* envt) (forward `(,tail) env #f)])
             (values `((return-point ,lbl ,@et*)) '()))]
          [(,h ,t ,t* ...)
           (letv* ([(eh* envh) (forward `(,h) env #f)]
                   [(et* envt) (forward `(,t ,t* ...) envh #f)])
             (values `(,@eh* ,@et*) envt))]
          [((,op ,x ,y)) (guard (or (binop? op) (relop? op)))
           (letv* ([(ex* envx) (forward `(,x) env #f)]
                   [(ey* envy) (forward `(,y) env #f)])
             (cond
               [(and (binop? op) (number? (car ex*)) (number? (car ey*)))
                (values (list (eval `(,op ,@ex* ,@ey*))) env)]
               [(and (relop? op) (number? (car ex*)) (number? (car ey*)))
                (values (if (eval `(,op ,@ex* ,@ey*)) `((true)) `((false))) env)]
               [else
                 (let* ([ex* (cond
                               [(and (equal? x lhs) (not (equal? (car ex*) lhs))) `(,x)]
                               [(and (eq? op '*) (mem? (car ex*))) `(,x)]
                               [(and (mem? (car ey*)) (mem? (car ex*))) `(,x)]
                               [(and (relop? op) (not (register? (car ex*)))) `(,x)]
                               [(and (int64? (car ex*)) (not (int32? (car ex*)))) `(,x)]
                               [else ex*])]
                        [ey* (cond
                               [(and (equal? y lhs) (not (equal? (car ey*) lhs))) `(,x)]
                               [(and (mem? (car ey*)) (mem? (car ex*))) `(,y)]
                               [(and (int64? (car ey*)) (not (int32? (car ey*)))) `(,x)]
                               [else ey*])])
                   (values `((,op ,@ex* ,@ey*)) env))]))]
          [((ccall ,reg ,lbl ,l* ...))
           (values `((ccall ,reg ,lbl ,l* ...)) env)]
          [((,f ,l* ...))
           (let-values ([(ef* envf) (forward `(,f) env #f)])
             (values `((,@ef* ,l* ...)) env))]
          [((nop)) (values `((nop)) env)]
          [(,x) (values (list (lookup x env)) env)])))
    (let-values ([(ex* _) (forward x '() #f)]) ex*)))


;; doing live-analysis
(define backward-delete
  (lambda (x)
    (define (location? x)
      (or (register? x) (frame-var? x) (uvar? x)))
    (define backward
      (lambda (exp live* f-live*)
        (match exp
          [((begin ,s* ...))
           (let-values ([(es* live^) (backward `(,s* ...) live* f-live*)])
             (values `((begin ,es* ...)) live^))]
          [((if ,t ,c ,a))
           (letv* ([(ec* lc*) (backward `(,c) live* f-live*)]
                   [(ea* la*) (backward `(,a) live* f-live*)]
                   [(et* lt*) (backward `(,t) lc* la*)])
             (if (and (equal? ec* `((nop))) (equal? ea* `((nop))))
                 (values `((nop)) lt*)
                 (values `((if ,@et* ,@ec* ,@ea*)) lt*)))]
          [((set! ,x ,y))
           (cond
             [(or (eq? x y) (not (memq x live*)))
              (values `((nop)) live*)]
             [else
               (letv* ([(ey* ly*) (backward `(,y) live* f-live*)])
                 (values `((set! ,x ,@ey*))
                   (union (difference live* `(,x)) ly*)))])]
          [((mset! ,base ,off ,val))
           (let-values ([(eb* lb*) (backward `(,base) live* f-live*)]
                        [(eo* lo*) (backward `(,off) live* f-live*)]
                        [(ev* lv*) (backward `(,val) live* f-live*)])
             (values `((mset! ,@eb* ,@eo* ,@ev*))
               (union live* lb* lo* lv*)))]
          [((return-point ,lbl ,tail))
           (let-values ([(et* lt*) (backward `(,tail) live* f-live*)])
             (values `((return-point ,lbl ,@et*)) lt*))]
          [(,h ,t ,t* ...)
           (letv* ([(et* lt*) (backward `(,t ,t* ...) live* f-live*)]
                   [(eh* lh*) (backward `(,h) lt* f-live*)])
             (values `(,@eh* ,@et*) lh*))]
          [((mref ,base ,off))
           (let-values ([(eb* lb*) (backward `(,base) live* f-live*)]
                        [(eo* lo*) (backward `(,off) live* f-live*)])
             (values `((mref ,@eb* ,@eo*)) (union live* lb* lo*)))]
          [((true)) (values `((true)) live*)]
          [((false)) (values `((false)) f-live*)]
          [((,op ,x ,y)) (guard (or (binop? op) (relop? op)))
           (let-values ([(ex* lx*) (backward `(,x) live* f-live*)]
                        [(ey* ly*) (backward `(,y) live* f-live*)])
             (values `((,op ,@ex* ,@ey*))
               (union live* f-live* lx* ly*)))]
          [((ccall ,reg ,lbl ,cl* ...))
           (let ([dx* (difference live* `(,reg))])
             (values `((ccall ,reg ,lbl ,cl* ...))
               (union dx* cl*)))]
          [((,t ,cl* ...))
           (let-values ([(et* lt*) (backward `(,t) live* f-live*)])
             (values `((,@et* ,cl* ...))
               (union live* lt* cl*)))]
          [((nop)) (values `((nop)) live*)]
          [(,x) (guard (location? x)) (values `(,x) `(,x))]
          [(,x) (values `(,x) '())]
          )))
    (letv* ([(ex* lx*) (backward x '() '())]) ex*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generate-x86-64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Program → ((code Stmt*) section*)
;;    Stmt → (set! Triv (Binop Triv Triv))
;;         | (set! Triv Triv)
;;         | (mset! Triv Triv (Binop (mref (Triv Triv) Triv)))
;;         | (mset! Triv Triv Triv)
;;         | (if (Relop Triv Triv) (jump Triv))
;;         | (jump Triv)
;;         | (push Triv)
;;         | (pop  Triv)
;;         | (call label)
;;    Triv → Var | int | label

(define generate-x86-64
  (lambda (x)
    (define binop->inst
      (lambda (op)
        (let ([op-map '([+ . addq]
                        [- . subq]
                        [* . imulq]
                        [/ . divq]
                        [sra . sarq]
                        [ash . salq]
                        [logand . andq]
                        [logor . orq])])
          (cdr (assq op op-map)))))
    (define relop->inst
      (lambda (op pos?)
        (let ([op-map '([= . (je . jne)]
                        [> . (jg . jle)]
                        [>= . (jge . jl)]
                        [< . (jl . jge)]
                        [<= . (jle . jg)])])
          ((if pos? cadr cddr) (assq op op-map)))))
    (define emit-test
      (lambda (test v)
        (match test
          [(,op ,a ,b)
           (emit 'cmpq b a)
           (emit-jump (relop->inst op #t) v)]
          [(not (,op ,a ,b))
           (emit 'cmpq b a)
           (emit-jump (relop->inst op #f) v)])))
    (define (SET! x)
      (match x
        [(,live1 (nop ,live2 ,live* ...)) ;; do nothing
         (void)]
        [(,dst (,op ,dst ,src)) (guard (binop? op))
         (if (eq? op '/)
             (emit (binop->inst op) src)
             (emit (binop->inst op) src dst))]
        [(,dst ,src) (guard (section? src))
         (emit 'movq src dst)]
        [(,dst 0) (guard (and (register? dst)))
         (emit 'xorq dst dst)]
        [(,dst ,src)
         (emit (if (label? src) 'leaq 'movq) src dst)]
        [,x (errorf 'generate-x86-64 "invalid set! form ~a " `(set! ,x ...))]))
    (define code-gen
      (lambda (p)
        (match p
          [(set! ,a* ...)
           (SET! `(,a* ...))]
          [(mset! ,base ,off (,op ,x ,y)) (guard (binop? op))
           (match x
             [(mref ,base^ ,off^)
              (unless (and (eq? base base^) (eq? off off^))
                (errorf 'code-gen
                  "src (~a,~a) and dst (~a,~a) not match" base^ off^ base off))]
             [else (errorf 'code-gen "illgal instructions ~a" p)])
           (emit (binop->inst op) y x)]
          [(mset! ,base ,off ,val)
           (emit 'movq val `(mref ,base ,off))]
          [(if ,test (jump ,lbl)) (emit-test test lbl)]
          [(jump ,lbl) (emit-jump 'jmp lbl)]
          [(push ,v) (emit 'pushq v)]
          [(pop ,v) (emit 'popq v)]
          [(call ,p) (emit-call p)]
          [(string ,s) (emit-string s)]
          [(text) (emit-text)]
          [,sec (guard (section? sec)) (emit-section sec)]
          [,lbl (guard (label? lbl)) (emit-label lbl)]
          [,p (errorf 'code-gen "invalid statement ~a" p)])))
    (match x
      [((code ,stmt* ...) ,sec* ...)
       (emit-program (entry (for-each code-gen stmt*)) (for-each code-gen sec*))]
      [,x (errorf 'generate-x86-64 "invalid program ~a" x)])))
)
