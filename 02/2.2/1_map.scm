#lang sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -2 -1 2 10 22))
(map (lambda (x) (* x x)) (list 1 2 3 4 5))