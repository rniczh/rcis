(define A 10000)

(define subv
  (lambda (va vb vc N)
    (define loop
      (lambda (va vb vc borrow i)
        (if (>= i 0)
            (begin
              (vector-set! vc i (- (- (vector-ref va i) (vector-ref vb i)) borrow))
              (if (>= (vector-ref vc i) 0)
                  (loop va vb vc 0 (- i 1))
                  (begin
                    (vector-set! vc i (+ (vector-ref vc i) A))
                    (loop va vb vc 1 (- i 1))))))))
    (loop va vb vc 0 (+ N 1))))

(define addv
  (lambda (va vb vc N)
    (define loop
      (lambda (va vb vc carry i)
        (if (>= i 0)
            (begin
              (vector-set! vc i (+ (+ (vector-ref va i) (vector-ref vb i)) carry))
              (if (< (vector-ref vc i) A)
                  (loop va vb vc 0 (- i 1))
                  (begin
                    (vector-set! vc i (- (vector-ref vc i) A))
                    (loop va vb vc 1 (- i 1))))))))
    (loop va vb vc 0 (+ N 1))))

(define divv
  (lambda (va b vc N)
    (define loop
      (lambda (va b vc remain i)
        (if (<= i (+ N 1))
            (begin
              (let ([tmp (+ (vector-ref va i) remain)])
                (vector-set! vc i (/ tmp b))
                (loop va b vc (* (mod tmp b) A) (+ i 1)))))))
    (loop va  b vc 0 0)))
