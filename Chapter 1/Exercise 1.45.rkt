#lang sicp

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-f f n)
  (if (= n 1)
      f
      (compose f (repeated-f f (- n 1)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (sqrt x)
  (fixed-point 
   (average-damp 
    (lambda (y) (/ x y)))
   1.0))

(define (nth-root-avg x n z)
  (define (avg-damp y)
    (repeated-f average-damp y))
  
  (fixed-point 
   ((avg-damp z) 
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))

(define (nth-root x n)
  (nth-root-avg x n 5))





