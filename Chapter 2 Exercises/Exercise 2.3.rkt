#lang sicp

;; points
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; segments
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (print-seg seg)
  (print-point (start-segment seg))
  (print-point (end-segment seg)))

;; rectangle implementation
(define (make-rectangle start-corner end-corner)
  (let ((mid-corner (make-point (x-point end-corner) (y-point start-corner))))
  (cons(make-segment start-corner mid-corner)
       (make-segment mid-corner end-corner))))

(define (first-side rect)
  (car rect))

(define (second-side rect)
  (cdr rect))

(define (rect-seg-length seg)
  (let ((x1 (x-point (start-segment seg)))
        (x2 (x-point (end-segment seg)))
        (y1 (y-point (start-segment seg)))
        (y2 (y-point (end-segment seg))))
    (abs (+ (- x2 x1) (- y2 y1)))))

(define (perimiter rect)
  (* 2 (+
        (rect-seg-length (first-side rect))
        (rect-seg-length (second-side rect)))))

(define (area rect)
  (* (rect-seg-length (first-side rect))
      (rect-seg-length (second-side rect))))


;; testing
(define test-c-st (make-point 5 2))
(define test-c-end (make-point 1 1))
(define test-rect (make-rectangle test-c-st test-c-end))
  