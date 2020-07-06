#lang sicp

; Answer

(define (make-account balance password)
  (let ((counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance 
                       (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch pass arg)
      (if (eq? password pass)
          (begin (set! counter 0)
          (cond ((eq? arg 'withdraw) withdraw)
                ((eq? arg 'deposit) deposit)
                (else (error "Unknown request: 
                 MAKE-ACCOUNT" arg))))
          (lambda (x)
            (if (> counter 7)
                "Calling Cops!!!!"
                (begin
                  (set! counter (+ counter 1))
                  "Incorrect Password")))))
  dispatch))


; Testing

(define acc 
  (make-account 100 'secret-password))
