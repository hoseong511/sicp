#lang sicp
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))
; ( () ) -> ( () (3) ) -> ( () (3) (2) (2 3) ) -> ( () (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3) )
; rest 원소를 (list (car s)) 에 append 한다
(define x (list 1 2 3 4))
(subsets x)