#lang sicp

(define (product f a next b)
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))

(define (identity n)
  n)

(define (increment n)
  (+ n 1))

(define (factorial n)
  (product identity 1 increment n))

(define (square x)
  (* x x))

(define (approximate-pi n)

  (define (pi-next a)
    (+ a 2))

  (* 4.0 (/ (* 2 (product square 4 pi-next n))
            (product square 3 pi-next (- n 1)) n)))
  
