#lang sicp


(define (same-parity x . y)
  (define (iter-parity result rem)
    (if (null? rem)
        result      
        (if (= (remainder (car rem) x) 0)
            (iter-parity (append result (list (car rem))) (cdr rem))
            (iter-parity result (cdr rem)))))
  (iter-parity (list x) y))
        
      
