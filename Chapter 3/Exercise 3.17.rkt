#lang sicp

; Answer

(define (count-pairs x)

  (let ((counted-pairs '()))

    (define (not-counted? x) 
      (if (memq x counted-pairs) 
          0 
          (begin 
            (set! counted-pairs (cons x counted-pairs)) 
            1))) 
      
    (define (count-p x)
      (if (not (pair? x))
          0
          (+ (count-p (car x))
             (count-p (cdr x))
             (not-counted? x))))

  (count-p x)))


; Testing

(define x '(foo)) 
(define y (cons x x)) 
(define z (cons y y)) 
 
