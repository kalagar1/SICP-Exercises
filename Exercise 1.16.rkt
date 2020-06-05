;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 1.16|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (expo b n)
  (iterative-fast-exp b n 1))

(define (iterative-fast-exp b n a)
  (cond ((= n 0) a)
        ((= n 1) (* a b))
        (else (iterative-fast-exp b (- n 2) (* a b b)))))