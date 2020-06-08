#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-f f n)
  (if (= n 1)
      f
      (compose f (repeated-f f (- n 1)))))

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))
