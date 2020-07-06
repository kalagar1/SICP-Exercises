#lang sicp

(define (cont-recursive-frac n d k)
  (define (recursive-frac n d i)
    (if (> i k)
        1
    (/ (n i) (+ (d i) (recursive-frac n d (+ i 1))))))
  (recursive-frac n d 1))

(define (estimate-e k)
  (define (n x)
    1.0)
  (define (d x)
    (if (= (remainder x 3) 2)
      (* (+ 1 (/ (- x 2) 3)) 2)
      1))
  (+ 2 (cont-recursive-frac n d k)))

(define e (estimate-e 100))