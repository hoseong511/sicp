#lang sicp
(define (fringe x)
  (define (iter things res)
    (if (null? things)
        res
        (if (pair? (car things))
            (iter (cdr things) (append res (fringe (car things))))
            (iter (cdr things) (append res (list (car things)))))))
  (iter x nil))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))