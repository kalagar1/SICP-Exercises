#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (enumerate-tree t)))


;; Testing
  (define z (list 1 2 (list 2 3 1 (list 2 2 1 2 (list 3 2))) 1 2))
