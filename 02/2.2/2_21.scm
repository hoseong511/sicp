#lang sicp

(define (square-list items)
  (define square (lambda (x) (* x x)))
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define square (lambda (x) (* x x)))

(define (square-list-map items)
  (map square items))

(square-list (list 1 2 3 4 5))
(square-list-map (list 1 2 3 4 5))