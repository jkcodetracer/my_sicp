#lang racket

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negtive? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (positive? x)
  (if (> x 0)
      true
      false))

(define (average x y)
  (/ (+ x y) 2))

(define (negtive? x)
  (if (< x 0)
      true
      false))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))

; use (half-interval-method sin 2.0 4.0) to get pi

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negtive? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negtive? b-value))
           (search f b a))
          (else 
           (display "invalid input")))))


 