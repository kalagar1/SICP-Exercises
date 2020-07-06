#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) nil seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs)))))

;; Answers

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))



;; Testing

(define x (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define y (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define z (list (list 1 0 0) (list 0 1 0) (list 1 2 3)))