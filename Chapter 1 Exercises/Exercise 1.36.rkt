#lang sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess counter)
    (display counter)
    (display ": ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ counter 1)))))
  (try first-guess 1))

(define (golden-ratio guess)
  (fixed-point
   (lambda (x) (+ (/ 1 x) 1))
   guess))

(define (x-to-x guess)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
   guess))

(define (average x y)
  (/ (+ x y) 2))

(define (x-to-x-avg guess)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
   guess))