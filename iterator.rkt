#lang racket
(define (normal-sum a b)
  (if (> a b)
      0
      (+ a (normal-sum (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (new-sum a b)
  (define (next-a a)
    (+ a 1))
  (define (add a) a)
  (sum add a next-a b))


(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (iter-sum a b)
  (define (next-a a)
    (+ a 1))
  (define (add a) a)
  (sum-iter add a next-a b))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial x)
  (define (fact x) x)
  (define (next x) (+ x 1))
  (product-iter fact 1 next x))

(define (my-pi n)
  (define (my-pi-f x) 
    (* (/ (* 2 x)
          (- (* 2 x) 1))
       (/ (* 2 x)
          (+ (* 2 x) 1))))
  (define (next x) (+ x 1))
  (* 2 (product-iter my-pi-f 1 next n)))
