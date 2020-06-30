#lang sicp

(define (lookup given-key set-of-records)
  (let (val (key (entry set-of-records)))
  
  (cond ((null? set-of-records) false)
        ((= given-key val)
         (car set-of-records))
        (else
         (if (< val given-key)
             (lookup given-key (left-branch set-of-records))
             (lookup given-key (right-branch set-of-records)))))))