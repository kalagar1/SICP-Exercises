#lang sicp

; First try

(define (equal? list1 list2)
  (if (or (null? list1) (null? list2))
      (if (and (null? list1) (null? list2))
          #t
          #f
          )
          
  (if (or (symbol? list1) (symbol? list2))
      (if (and (symbol? list1) (symbol? list2))
          (eq? list1 list2)
          #f)

      (and (equal? +(car list1) (car list2))
           (equal? (cdr list1) (cdr list2))))))


; Testing

(define a '(a (b c) d (e)))
(define b '(a (b c) d (e)))
(define c '(a b c (d e)))

; Simpler version from online

 (define (equal2? a b) 
   (if (and (pair? a) (pair? b)) 
       (and (equal2? (car a) (car b)) (equal2? (cdr a) (cdr b))) 
       (eq? a b)))