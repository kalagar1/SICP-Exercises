#lang sicp

; Dependencies

 (define (count-pairs x) 
   (let ((encountered '())) 
     (define (helper x) 
       (if (or (not (pair? x)) (memq x encountered)) 
         0 
         (begin 
           (set! encountered (cons x encountered)) 
           (+ (helper (car x)) 
              (helper (cdr x)) 
              1)))) 
   (helper x))) 


; Answer

(define (has-cycle? list)
  (define num-pairs (count-pairs list))

  (define (cycle-iter list counter max)
    (cond
      ((> counter max) #t)
      ((null? (cdr list)) #f)
      (else (cycle-iter (cdr list) (+ counter 1) max))))
  
  (cycle-iter list 0 num-pairs))


; Testing

(define tester (list 'a 'b 'c)) 
 (set-cdr! (cddr tester) tester) 