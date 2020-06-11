#lang sicp

 (define (make-interval a b) 
         (if (< a b) 
                 (cons a b) 
                 (cons b a))) 

  
 (define (lower-bound interval) (car interval)) 
 (define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) 
        (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) 
        (lower-bound i)) 
     2))

(define (make-center-percent c p)
   (make-interval (- c (* c (/ p 100.0))) (+ c (* c (/ p 100.0)))))

(define (percent i)
  (* (/ (- (upper-bound i) (center i)) (center i)) 100))

