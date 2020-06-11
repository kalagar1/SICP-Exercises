#lang sicp

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (reverse items)
  (if (null? (cdr items))
      items
  (append (reverse (cdr items)) (list (car items)))))


; Testing

(define squares (list 1 4 9 16 25))