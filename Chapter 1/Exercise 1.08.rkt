;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname newtonsCubeRoot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (cubert-iter guess prev-guess x)
  (if (good-enough? guess prev-guess x)
      guess
      (cubert-iter (improve guess x) guess x)))

(define (good-enough? guess prev-guess x)
  (< (abs (- guess prev-guess)) (* x 0.001)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube x) (* x x x))

(define (cubeRoot x) (cubert-iter 1.0 0.0 x))