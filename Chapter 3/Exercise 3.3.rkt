#lang sicp

; Answer

(define (make-account balance password)
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
        (cond ((eq? arg 'withdraw) withdraw)
              ((eq? arg 'deposit) deposit)
              (else (error "Unknown request: 
                 MAKE-ACCOUNT" arg)))
    (error "Incorrect Password")))
  dispatch)


; Testing

(define acc 
  (make-account 100 'secret-password))