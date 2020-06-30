#lang sicp

; Answer

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
      (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

(define (union-set set1 set2)
  (cond
    ((null? set1) set2)
    (else (cons (car set1)
                (union-set (cdr set1) set2)))))

; Testing

(define a '(2 3 2 1 3 2 2))
(define b '(1 3 4 3))
(define e '())
(define s '(2 3 4 5))
(define t '(4 5 6 7))
  
