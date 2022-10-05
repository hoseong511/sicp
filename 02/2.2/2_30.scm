#lang sicp

(define (square-tree tree)
  (define square (lambda (x) (* x x)))
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (define square (lambda (x) (* x x)))
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))
(define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree tree)
(square-tree-map tree)