#lang racket

(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))
; 无穷连分式
(define (cont-frac-iter n d k)
  (define (cont-frac-test a n d k)
    (if (= k 0)
        a
        (cont-frac-test (/ (n k) (+ a (d k))) n d (- k 1))))
  (cont-frac-test 0 n d k))


; 求tan 
(define (tan-cf x k)
  (cont-frac-iter (lambda (i) 
                    (if (= i 1)
                        i
                        (* i i -1.0)))
                  (lambda (i)
                    (- (* 2 i) 1.0))
                  k))
                               
          