#lang sicp

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))


;; 1.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define branch-length left-branch)

(define branch-structure right-branch)


;; 2.

(define (total-weight mobile)
  (if (not (list? (branch-structure mobile)))
      (branch-structure mobile)
      (+ (total-weight (left-branch mobile)) (total-weight (right-branch mobile)))))


;; 3.

(define (balanced? mobile)
  (if (not (list? mobile))
      #t
      (if (= (* (branch-length (left-branch mobile)) (total-weight (left-branch mobile)))
             (* (branch-length (right-branch mobile)) (total-weight (right-branch mobile))))
          (if (and (balanced? (branch-structure (left-branch mobile))) (balanced? (branch-structure (right-branch mobile))))
              #t
              #f)
          #f)))
      


;; testing
(define a (make-mobile (make-branch 1 2) (make-branch 4 5)))
(define bal (make-mobile (make-branch 2 3) (make-branch 1 6)))
(define test (make-mobile (make-branch 10 a) (make-branch 12 5))) 
  


