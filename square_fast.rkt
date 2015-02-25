#lang racket
(define (expt b n)
  (expt-iter b 1 n))

(define (expt-iter b product counter)
  (cond ((= counter 0) product)
        ((even? counter) (expt-iter (* b b) product (/ counter 2)))
        (else (expt-iter b (* product b) (- counter 1)))))

(define (even? n)
  (= (remainder n 2) 0))