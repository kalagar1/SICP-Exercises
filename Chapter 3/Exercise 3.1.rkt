#lang sicp

(define (make-accumulator value)
  (lambda (amount)
    (begin (set! value (+ amount value)) value)))

(define a (make-accumulator 10))

(display (a 10))
