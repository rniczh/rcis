(let ([x (box 1)]
      [y (box 2)])
  (if (ccall "change_var" x y)
      (begin (display x) (newline) (display y) (newline))
      (display "faild")))
