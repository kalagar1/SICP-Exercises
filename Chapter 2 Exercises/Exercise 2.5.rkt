#lang sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (div-two? n)
  (integer? (/ n 2)))

(define (div-three? n)
  (integer? (/ n 3)))

(define (car x)
  (if (div-two? x)
      (+ 1 (car (/ x 2)))
      0))

(define (cdr x)
  (if (div-three? x)
      (+ 1 (car (/ x 3)))
      0))
