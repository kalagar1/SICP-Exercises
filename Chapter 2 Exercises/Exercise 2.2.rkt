#lang sicp

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (midpoint-segment seg)
  (let 
      ((x (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2))
       (y (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

    (print-point (make-point x y))))
