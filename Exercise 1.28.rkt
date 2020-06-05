#lang sicp

(define (miller-rabin-test n)
  (define (try-it a)
    (not (= (expmod a (- n 1) n) 0)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) 
         (fast-prime? n (- times 1)))
        (else false)))


(define (square x) (* x x)) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (if (and (= (remainder (square (expmod base (/ exp 2) m)) m) 1)
                   (not (= (expmod base (/ exp 2) m) 1))
                   (not (= (expmod base (/ exp 2) m) (- m 1))))
              0
              (remainder (square (expmod base (/ exp 2) m)) m)))
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))

(define (even? n)
  (= (remainder n 2) 0))

(define (prime? n)
  (fast-prime? n 100))

