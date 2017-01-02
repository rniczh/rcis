(define append
  (lambda (l m)
    (if (null? l) m
        (cons (car l) (append (cdr l) m)))))

(define pHelper
  (lambda (all chk l m)
    (if (null? all) (cons l (cons chk (cons m '())))
        (let ([x (car all)])
          (if (<= x chk)
              (pHelper (cdr all) chk (cons x l) m)
              (pHelper (cdr all) chk l (cons x m)))))))

(define partition
  (lambda (l) (pHelper (cdr l) (car l) '() '())))

(define quicksort
  (lambda (l)
    (if (null? l) l
        (let ([lx (partition l)])
          (append (quicksort (car lx)) (cons (car (cdr  lx)) (quicksort (car (cdr (cdr lx))))))))))
