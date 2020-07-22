#lang sicp

; Dependencies

(define (full-adder a b c-in sum c-out)
  (let ((c1 (make-wire)) 
        (c2 (make-wire))
        (s  (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; Answer (Ripple-Carry Adder)

(define (ripple-carry-adder a b c-in s)
  (if (not (null? (cdr a)))
      (let ((c-out (make-wire))) 
               (full-adder (car a) (car b) c-in (car s) c-out) 
               (ripple-carry-adder (cdr a) (cdr b) c-out (cdr s))))) 
      
      
  
