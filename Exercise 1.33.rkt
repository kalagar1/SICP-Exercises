#lang sicp

(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))


(define (increment x)
  (+ x 1))

(define (prime? n)
  (if (= n 1)
      false
      (= n (smallest-divisor n))))

 (define (smallest-divisor n)
  (find-divisor n 2))

(define (square x)
  (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (sum-of-prime-squares a b)
  (filtered-accumulate prime? + 0 square a increment b))



(define (identity x)
  x)

 (define (gcd m n) 
   (cond ((< m n) (gcd n m)) 
         ((= n 0) m) 
         (else (gcd n (remainder m n))))) 
  
 (define (relative-prime? m n) 
 (= (gcd m n) 1))


(define (product-rel-prime n)
  (define (filter x)
    (relative-prime? x n))
  (filtered-accumulate filter * 1 identity 1 increment (- n 1)))
