#lang sicp

; Dependencies

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? 
          x 
          (left-branch set)))
        ((> x (entry set))
         (element-of-set? 
          x 
          (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree 
          (entry set)
          (adjoin-set x (left-branch set))
          (right-branch set)))
        ((> x (entry set))
         (make-tree
          (entry set)
          (left-branch set)
          (adjoin-set x (right-branch set))))))

(define (union-set-olist set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    ((< (car set1) (car set2))
     (cons (car set1) (union-set-olist (cdr set1) set2)))
    ((> (car set1) (car set2))
     (cons (car set2) (union-set-olist set1 (cdr set2))))
    (else (cons (car set1)
                (union-set-olist (cdr set1) (cdr set2))))))

(define (tree-to-list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define (list-to-tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ol 
                         (cdr set1)
                         (cdr set2))))
              ((< x1 x2) (intersection-set-ol 
                          (cdr set1) 
                          set2))
              ((< x2 x1) (intersection-set-ol 
                          set1 
                          (cdr set2)))))))

; Answer

(define (union-set set1 set2)
    (list-to-tree (union-set-olist
                   (tree-to-list-2 set1)
                   (tree-to-list-2 set2))))

(define (intersection-set set1 set2)
    (list-to-tree (intersection-set-ol
                   (tree-to-list-2 set1)
                   (tree-to-list-2 set2))))

    

; Testing

(define a (make-tree 7
                     (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil))
                     (make-tree 9 nil (make-tree 11 nil nil))))

(define b (make-tree 3 (make-tree 1 nil nil)
                     (make-tree 7 (make-tree 5 nil nil) 
                                (make-tree 11 nil (make-tree 50 nil nil)))))

(define c (make-tree 5
                     (make-tree 2 (make-tree 1 nil nil) nil)
                     (make-tree 9 (make-tree 7 nil nil) (make-tree 50 nil nil))))