#lang racket
(define (funcf n)
  (cond ((< n 3) n)
        (else
         (+ (funcf (- n 1))
            (* 2 (funcf (- n 2)))
            (* 3 (funcf (- n 3)))))))

(define (funcf-iter n)
  (funcf-iter-iner 2 1 0 n))

(define (funcf-iter-iner a b c n)
  (cond ((< n 2) n)
        ((= n 2) a)
        (else 
         (funcf-iter-iner (+ a (* 2 b) (* 3 c)) a b (- n 1)))))
