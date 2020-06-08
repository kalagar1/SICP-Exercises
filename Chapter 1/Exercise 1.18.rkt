;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 1.18|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (multiply a b)
  (iterative-multiply a b 0))

(define (iterative-multiply a b x)
  (cond ((= b 0) x)
        ((= b 1) (+ x a))
        (else (iterative-multiply a (- b 2) (+ x (double a))))))

(define (double n)
  (+ n n))