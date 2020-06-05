;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 1.17|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (multiply a b)
  (cond ((= b 0) 
         0)
        ((even? b) 
         (double (multiply a (halve b))))
        (else 
         (+ a (multiply a (- b 1))))))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

