#lang racket
(define (pascar x y)
  (cond ((= x y) 1)
        ((= y 1) 1)
        ((> y x) (display "invalid input"))
        (else (+ (pascar (- x 1) y)
                 (pascar (- x 1) (- y 1))))))