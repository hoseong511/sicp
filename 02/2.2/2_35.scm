#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(count-leaves (list 1 3 0 5 0 (list 1 2 3)))