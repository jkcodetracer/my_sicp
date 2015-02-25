#lang racket
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
; get the x^n%m
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) 
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else 
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (even? n)
  (if (= (remainder n 2) 1)
      false
      true))

(define (square x) (* x x))

