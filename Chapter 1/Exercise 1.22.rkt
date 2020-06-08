#lang sicp

(define (square n)
  (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

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

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (prime-search start)
  (if (even? start)
  (search-for-primes (+ start 1) (+ start 31))
  (search-for-primes start (+ start 30))))

(define (search-for-primes start end)
    (timed-prime-test start)
    (if (= start end)
        (display "\n QED")
        (search-for-primes (+ start 2) end)))

(define (even? n)
  (= (remainder n 2) 0))








  