#lang sicp

; Selectors for Frame Implementation 

; Implementation 1 with List
(define (make-frame-l origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame-l frame)
  (car frame))

(define (edge1-frame-l frame)
  (car (cdr frame)))

(define (edge2-frame-l frame)
  (car (cdr (cdr frame))))


; Implementation 2 with Cons
(define (make-frame-c origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame-c frame)
  (car frame))

(define (edge1-frame-c frame)
  (car (cdr frame)))

(define (edge2-frame-c frame)
  (cdr (cdr frame)))