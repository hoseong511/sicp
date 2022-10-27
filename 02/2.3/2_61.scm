#lang sicp

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 4 (list 1 2 5 6))
(adjoin-set 6 (list 1 2 5 7))