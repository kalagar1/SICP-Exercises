#lang sicp

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)(improve guess)))))


(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))

  (define (improve guess)
  (average guess (/ x guess)))

  ((iterative-improve good-enough? improve) 1.0))



(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  
  (define (close-enough? guess)
    (< (abs (- guess (improve guess))) 
       tolerance))
  
  (define (improve guess)
    (f guess))
 
  ((iterative-improve close-enough? improve) first-guess))








  