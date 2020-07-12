#lang sicp

; Answer

(define f
  (let ((number 1))
    (lambda (input) 
    (begin (set! number (* input number))
    number))))
 

