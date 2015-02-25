#lang racket

(define (cc amount coin-value)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-value)) 0)
        (else
         (+ (cc amount 
                (except-first-denomination coin-value))
            (cc (- amount 
                   (first-denomination coin-value))
                coin-value)))))

(define (first-denomination coin-value)
  (car coin-value))

(define (except-first-denomination coin-value)
  (cdr coin-value))

(define (no-more? coin-value)
  (if (null? coin-value)
      true
      false))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 100 5 2 1 0.5))

(define (same-parity first . others)
  (display first)
  (newline)
  (display others)
  (newline)
  (if (not (null? others))
      (if (odd? first)
	  (cons first (filter odd? others))
	  (cons first (remove odd? others)))
      (list first)))
