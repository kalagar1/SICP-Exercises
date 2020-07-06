#lang sicp

; Answer

(define (make-monitored proc)
  (let ((counter 0))
    (lambda (input)
      (cond ((eq? input 'how-many-calls?) counter)
            ((eq? input 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1)) (proc input)))))))
            

; Testing

(define s (make-monitored sqrt))