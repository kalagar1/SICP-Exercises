#lang sicp

(define (squares x)
  (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (appends seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))


;; Testing
(define x (list 1 2 3 4 5))
(define y (list 22 90))