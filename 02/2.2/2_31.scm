#lang sicp

(define square (lambda (x) (* x x)))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))))
(define (square-tree2 tree)
  (tree-map square tree))