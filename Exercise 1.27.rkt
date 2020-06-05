#lang sicp


(define (congruent-tester n)
  (single-congruent-test 1 n))

(define (single-congruent-test counter n)
    (cond ((and (= counter (- n 1)) (= (expmod counter n n) counter)) "IS Congruent for all a")
          ((= (expmod counter n n) counter) (single-congruent-test (+ counter 1) n))
          (else "NOT Congruent for all a")))



(define (square x) (* x x)) 
  
 (define (expmod base exp m) 
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (remainder (square (expmod base (/ exp 2) m)) 
                     m)) 
         (else 
          (remainder (* base (expmod base (- exp 1) m)) 
                     m))))