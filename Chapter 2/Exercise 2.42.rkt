#lang sicp

; Dependencies

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))



; SOLUTION

(define empty-board nil)

(define (adjoin-position r c positions)
  (cons (list r c) positions))


(define (diagonal-spots coord)
    (map (lambda (x) (list x (- (car (cdr coord)) (abs (- (car coord) x)))))
         (append (enumerate-interval 1 (- (car coord) 1))
                 (enumerate-interval (+ (car coord) 1) (car (cdr coord))))))

  (define (contain x seq)
    (if (null? seq)
        #f
        (if (and (= (car x) (car (car seq))) (= (car (cdr x)) (car (cdr (car seq)))))
            #t
            (or #f (contain x (cdr seq))))))

   (define (contains positions seq)
     (if (null? positions)
         #f
        (if (contain (car positions) seq)
            #t
            (or #f (contains (cdr positions) seq)))))


(define (safe? k positions)
  
  (define (diagonal-safe positions diagonals)
   (not (contains positions diagonals)))
    
  (define (iter-row-safe x pos)
    (if (null? pos)
        #t
        (if (= x (car (car pos)))
            #f
            (and #t (iter-row-safe x (cdr pos))))))

  
  (and (iter-row-safe (car (car positions)) (cdr positions))
       (diagonal-safe (cdr positions) (diagonal-spots (car positions)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


;; testing

(define p (list (list 3 1) (list 7 2) (list 2 3) (list 8 4) (list 5 5) (list 1 6) (list 4 7) (list 6 8)))
(define diag (diagonal-spots (list 6 8)))

