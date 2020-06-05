#lang sicp

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpsons-integral f a b n)

  (define (next-a a)
    (+ a 2))

  (define (h a b n)
    (/ (- b a) n))
  
  (define (y k)
    (f (+ a (* k (h a b n)))))

  (define (term a)
    (+ (* 2 (y a))
       (* 4 (y (+ a 1)))))

  (* (/ (h a b n) 3)
     (+ (y 0)
        (* 4 (y 1))
        (sum term (+ a 2) next-a n))))




