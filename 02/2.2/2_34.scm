#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (horner-eval x seq)
  (accumulate (lambda (this higher-term) (+ this (* x higher-term)))
              0
              seq))

(horner-eval 2 (list 1 3 0 5 0 1))