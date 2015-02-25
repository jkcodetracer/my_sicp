#lang racket

(define tolorent 0.00001)
; get the fixed point of xxx
; (fixed-point cos 1.0)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolorent))
  (define (try x)
    (let ((next (f x)))
      (if (close-enough? x next)
          x
          (begin
            (display x)
            (newline)
            (try next)))))
  (try first-guess))

;don't use the y = x/y as the next value
;use the average y (/ x y) to destroy the threshold
(define (sqrt-fixed-point x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-xx-normal x)
  (fixed-point (lambda (y) (/ (log x) (log y)))
               10.0))

(define (sqrt-xx-anti x)
  (fixed-point (lambda (y) (average y (/ (log x) (log y))))
               10.0))
