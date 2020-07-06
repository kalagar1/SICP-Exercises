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

; Answer

(define (tree-to-list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree-to-list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree-to-list-1 
              (right-branch tree))))))

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

; Testing

(define a (make-tree 7
                     (make-tree 3 (make-tree 1 nil nil) (make-tree 5 nil nil))
                     (make-tree 9 nil (make-tree 11 nil nil))))

(define b (make-tree 3 (make-tree 1 nil nil)
                     (make-tree 7 (make-tree 5 nil nil) 
                                (make-tree 9 nil (make-tree 11 nil nil)))))

(define c (make-tree 5
                     (make-tree 3 (make-tree 1 nil nil) nil)
                     (make-tree 9 (make-tree 7 nil nil) (make-tree 11 nil nil))))