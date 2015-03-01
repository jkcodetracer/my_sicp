#lang racket

; filter  to get the ideal sequence
(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

; accumulate  
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

; enumerate list
(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low 
            (enumerate-interval (+ low 1) high))))

; enumerate tree
(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                     (enumerate-tree (cdr tree))))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (append (car list1) (append (cdr list1) list2))))

(define (map  proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))


(define (even? x)
  (if (= (remainder x 2) 0)
      true
      false))

(define (fib x)
  (define (fib-helper a b n)
    (if (= n 0)
        b
        (fib-helper (+ a b) a (- n 1))))
  (fib-helper 1 0 x))

(define (even-fibs n)
  (accumulate cons
              null
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
