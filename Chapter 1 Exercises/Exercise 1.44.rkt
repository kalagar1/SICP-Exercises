#lang sicp

(define (3avg x y z)
  (/ (+ x y z) 3))

(define dx 0.00001)

(define (smooth f dx)
  (lambda (x) (3avg (f x) (f (- x dx)) (f (+ x dx)))))

(define (repeated-f f n)
  (if (= n 1)
      f
      (compose f (repeated-f f (- n 1)))))

(define (n-smooth f dx n)
  (repeated-f (smooth n) f dx))
