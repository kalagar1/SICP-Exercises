#lang sicp

; Queue representation using local state

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (front-ptr queue) front-ptr)
    
    (define (empty-queue?) (null? front-ptr))
    
    (define (delete-queue!)
      (set-front-ptr! (cdr front-ptr)))
    
    (define (insert-queue! item) 
       (let ((new-pair (cons item '()))) 
         (cond ((empty-queue?) 
                (set-front-ptr! new-pair) 
                (set-rear-ptr! new-pair)) 
               (else 
                (set-cdr! rear-ptr new-pair) 
                (set-rear-ptr! new-pair)))))
       
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?)) 
             ((eq? m 'front-queue) front-ptr) 
             ((eq? m 'insert-queue!) insert-queue!) 
             ((eq? m 'delete-queue!) (delete-queue!)) 
             (else (error "Undefined oepration"))))
    dispatch))

