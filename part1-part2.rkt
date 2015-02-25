#lang racket

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (higher-bound x) (higher-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (higher-bound y)))
        (p3 (* (higher-bound x) (lower-bound y)))
        (p4 (* (higher-bound x) (higher-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (= (* (lower-bound x) (higher-bound y)) 0)
      (display "error invalid y")
      (mul-interval x 
                (make-interval (/ 1.0 (lower-bound y))
                               (/ 1.0 (higher-bound y))))))

;(define (sub-interval x y)
; (add-interval x 
;                (make-interval (* -1.0 (lower-bound y))
;                               (* -1.0 (higher-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (higher-bound x) (higher-bound y))))

(define (make-interval low high)
  (if (< low high)
      (cons low high)
      (cons high low)))

(define (lower-bound x)
  (car x))
(define (higher-bound x)
  (cdr x))

(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
