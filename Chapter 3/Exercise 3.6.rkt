#lang sicp

(define rand
  (let ((init-rand 0))
    (lambda (symbol)
      (cond
        ((eq? symbol 'reset) (lambda (new-value) (begin (set! init-rand new-value) init-rand)))
        ((eq? symbol 'generate) (begin (set! init-rand (rand-update init-rand)) init-rand))
        (else (error "NO FLAG AVAILABLE"))))))

(define (rand-update numb)
  (+ numb 2))