#lang sicp

; Answer

(define (make-table same-key?)
  (define (assoc key records)
  (cond ((null? records) false)
        ((same-key? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))
  
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (let ((subtable 
             (assoc (car keys) (cdr local-table))))
        (if subtable
            (if (null? (cddr keys))
                (let ((record 
                       (assoc (cadr keys) 
                              (cdr subtable))))
                  (if record (cdr record) false))
                (lookup (cdr keys)))
            false)))
    (define (insert! keys value)
      (let ((subtable 
             (assoc (car keys) (cdr local-table))))
        (if subtable
            (if (null? (cddr keys))
                (let ((record 
                       (assoc (cadr keys) 
                              (cdr subtable))))
                  (if record
                      (set-cdr! record value)
                      (set-cdr! 
                       subtable
                       (cons (cons (cadr keys) value)
                             (cdr subtable)))))
                (set-cdr! 
                 local-table
                 (cons (list (car keys)
                             (cons (cadr keys) value))
                       (cdr local-table))))
            (insert! (cdr keys) value)))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

