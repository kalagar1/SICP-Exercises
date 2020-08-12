#lang racket

; Dependencies

(define sum 0)
; Sum = 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
; Sum = 0


(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))
; Sum = 1

(define y (stream-filter even? seq))
; Sum = 6

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))
; Sum = 136