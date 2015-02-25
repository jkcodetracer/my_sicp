#lang racket

(define (make-point x y)
  (cons x y))

(define (point-x p)
  (car p))

(define (point-y p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (point-x p))
  (display ", ")
  (display (point-y p))
  (display ")"))

(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define void (lambda (x) x))