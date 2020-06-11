#lang sicp

(define (square x)
  (* x x))

(define (square-list-reg items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list-reg (cdr items)))))


(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list-map items)
  (map square items))


;; Testing

(define tester (list 2 3 4 5 6))