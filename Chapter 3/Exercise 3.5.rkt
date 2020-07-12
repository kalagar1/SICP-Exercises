#lang sicp

; Dependencies

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; Answer

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (rect-area min-x max-x min-y max-y)
    (* (- max-x min-x) (- max-y min-y)))

  (define experiment
           (lambda () (pred (random-in-range x1 x2) (random-in-range y1 y2))))
  
  (* (rect-area x1 x2 y1 y2)
     (monte-carlo trials experiment)))
            

; Testing

(define (square x)
  (* x x))

(define (unit-circle x y)
  (<= (+ (square x) (square y)) 1))

(define pie
  (estimate-integral unit-circle -1.0 1.0 -1.0 1.0 200000))

