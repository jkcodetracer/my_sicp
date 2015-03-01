#lang racket

(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define (append list1 list2)
  (if (null? list1) 
      list2
      (cons (car list1)
            (append (cdr list1) list2))))

(define x (list (list 1 2) (list 3 4)))