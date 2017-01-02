(define (f n sum)
  (if (= n 0) sum
      (f (- n 1) (+ sum n))))

(display (f 1000000 0))
