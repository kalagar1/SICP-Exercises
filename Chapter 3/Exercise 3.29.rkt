#lang sicp

; Or-gate in terms of AND and INVERTER

(define (or-gate a1 a2 output)
    (let ((x (make-wire)) 
         (y (make-wire)) 
         (z (make-wire))) 
      (inverter a1 x)
      (inverter a2 y)
      (and-gate x y z)
      (inverter z output)
  'ok))
