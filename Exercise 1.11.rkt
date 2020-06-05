;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Exercise 1.11|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))



(define (f-iterative n)
  (f-iter (- n 3) 0 0 1 2))

(define (f-iter max-count count a b c)
  (if (= max-count count)
      (+ c (* 2 b) (* 3 a))
      (f-iter max-count
              (+ count 1)
              b
              c
              (+ c (* 2 b) (* 3 a)))))