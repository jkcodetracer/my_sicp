#lang racket
(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items))))) 

(define (square x) (* x x))

(define (square-list-a items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list-a (cdr items)))))
 
(define (square-list-b items)
  (map square items))

(define (square-list-c items)
   (define (iter things answer)
     (if (null? things)
         answer
         (iter (cdr things)
               (cons answer
                     (square (car things))))))
  (iter items null))

(define (for-each proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (for-each proc (cdr items)))))
; real for each, don't show the result unless the caller asked
(define (my-for-each proc items)
  (if (null? items)
      '()
      (begin
        (proc (car items))
        (cons (car items)
              (my-for-each proc (cdr items))))))