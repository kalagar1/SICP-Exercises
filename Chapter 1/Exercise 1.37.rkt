#lang sicp


(define (cont-recursive-frac n d k)
  (define (recursive-frac n d i)
    (if (> i k)
        1
    (/ (n i) (+ (d i) (recursive-frac n d (+ i 1))))))
  (recursive-frac n d 1))

(define (estimate-recursive-phi counter)
  (define (f x)
    1.0)
  (/ 1 (cont-recursive-frac f f counter)))


(define (cont-iterative-frac n d k)
  (define (iterative-frac n d i result)
    (if (> i k)
        result
    (iterative-frac n d (+ i 1)(/ (n i) (+ (d i) result)))))
  (iterative-frac n d 1 0))

(define (estimate-iterative-phi counter)
  (define (f x)
    1.0)
  (/ 1 (cont-iterative-frac f f counter)))