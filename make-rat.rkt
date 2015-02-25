#lang racket
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; constractor 
(define (make-rat n d) 
  (let ((real-n (/ n (gcd n d)))
        (real-d (/ d (gcd n d))))
    (cond ((negtive? (* real-n real-d)) (cons (* -1 (abs real-n)) (abs real-d)))
          (else (cons (abs real-n) (abs real-d))))))
          
; fatcher
(define (number x) (car x))
(define (denom x) (cdr x))

; calculator
(define (add-rat x y)
  (make-rat (+ (* (number x) (denom y))
               (* (denom x) (number y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (number x) (denom y))
               (* (number y) (denom x)))
            (* (denom x) (denom y))))

(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (negtive? x)
  (if (< x 0)
      true
      false))

(define (positive? y)
  (if (> y 0)
      true
      false))