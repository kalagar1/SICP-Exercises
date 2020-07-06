#lang sicp

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))


; Testing

(define squares (list 1 4 9 16 25))

