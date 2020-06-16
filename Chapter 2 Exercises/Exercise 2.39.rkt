#lang sicp

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define fold-right accumulate)


;; Answers

(define (reverse-r sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-l sequence)
  (fold-left
   (lambda (x y) (append (list y) x)) nil sequence))



;; Testing

(define x (list 1 2 3))
