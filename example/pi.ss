(load "lib/big-num.ss") ;; subv addv divv
(define L 2000)
(define N (+ (/ L 4) 1))
(define pi
     (lambda (s w v q n k N)
       (if (<= k n)
           (begin
             (divv w 25 w N)
             (divv v 239 v N)
             (divv v 239 v N)
             (subv w v q N)
             (divv q (- (* 2 k) 1) q N)
             (if (= (logand k 1) 0)
                 (subv s q s N)
                 (addv s q s N))
             (pi s w v q n (+ k 1) N)))))

(define compute
  (lambda (L N)
    (define s (make-vector (+ N 3)))
    (define w (make-vector (+ N 3)))
    (define v (make-vector (+ N 3)))
    (define q (make-vector (+ N 3)))
    (define n 1431)

    (vector-set! w 0 (* 16 5))
    (vector-set! v 0 (* 4 239))
    (pi s w v q n 1 N)
    (display (vector-ref s 0))
    (display ".")
    (letrec ([loop (lambda (k)
                     (if (< k (+ N 1))
                         ;; (loop (+ k 1))
                         (begin
                           (ccall "rt_print4" (vector-ref s k))
                           (loop (+ k 1)))
                      ))])
      (loop 1))
    ))

(compute L N)
