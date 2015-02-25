#lang racket


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (length-iter items)
  (define (length-it items n)
    (if (null? items)
        n
        (length-it (cdr items) (+ n 1))))
  (length-it items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) list2))))

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))
      
(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items)) (cons (car items) '()))))