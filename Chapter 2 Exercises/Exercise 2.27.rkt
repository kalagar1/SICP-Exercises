#lang sicp

(define (reverse items)
  (if (null? (cdr items))
      items
  (append (reverse (cdr items)) (list (car items)))))


 (define (deep-reverse items) 
   (if (pair? items) 
       (append (deep-reverse (cdr items))  
               (list (deep-reverse (car items)))) 
       items)) 

; Testing

(define y (list 1 2 3))
(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define z (list (list 1 2) (list 3 4)))