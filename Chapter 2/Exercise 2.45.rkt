#lang sicp
(#%require sicp-pict)

; Defns for testing

(define pt paint)
(define et einstein)

; Dependencies


; Answer

(define (split joiner operation)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split joiner operation)painter 
                                  (- n 1))))
          (joiner painter 
                  (operation smaller smaller))))))

; Testing

(define right-split (split beside below))
(define up-split (split below beside))