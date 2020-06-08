#lang sicp

(define (cont-recursive-frac n d k)
  (define (recursive-frac n d i)
    (if (> i k)
        1
    (/ (n i) (- (d i) (recursive-frac n d (+ i 1))))))
  (recursive-frac n d 1))

(define (tan-cf x k)
  (define (d i)
    (- (* i 2) 1))
  (define (n i)
    (if (= i 1)
        x
        (* x x)))
  (cont-recursive-frac n d k))
