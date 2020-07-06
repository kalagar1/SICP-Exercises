#lang sicp

(define (make-accumulator value)
  (lambda (amount)
    (begin (set! value (+ amount value)) value)))