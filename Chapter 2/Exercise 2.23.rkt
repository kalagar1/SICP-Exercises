#lang sicp

(define (for-each proc items)
  (proc (car items))
  (if (null? (cdr items))
      nil
      (for-each proc (cdr items))))
      
