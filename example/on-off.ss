(letrec ([time (lambda (f n)
                 (if (> n 0)
                     (begin
                       (display (f))
                       (time f (- n 1)))
                     (display 0)))]
         [f (let ([x #f])
              (lambda ()
                (set! x (not x))
                x))])
  (time f 10))
