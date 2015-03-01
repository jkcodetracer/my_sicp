#lang racket

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree) (* tree factor)))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (square-tree tree)
  (map tree
       (lambda (sub-tree)
         (cond ((null? sub-tree) null)
               ((pair? sub-tree) (square-tree sub-tree))
               (else (square sub-tree))))))
   

(define (map items proc)
  (if (null? items)
      null
      (cons (proc (car items))
            (map (cdr items) proc))))

(define (square x)
  (* x x))

(define (tree-map tree proc)
  (map tree 
       (lambda (sub-tree)
         (cond ((null? sub-tree) null)
               ((pair? sub-tree) (tree-map sub-tree proc))
               (else (proc sub-tree))))))

(define (sqr-tree tree) (tree-map tree square))

(define tree-a (list 1 (list 2 (list 3 4) 5) (list 6 7)))