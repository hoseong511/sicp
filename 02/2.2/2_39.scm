#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter result x)
    (if (null? x)
        result
        (iter (op result (car x))
              (cdr x))))
  (iter init seq))

(define (fold-right op init seq)
  (accumulate op init seq))

(define (reverse seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

(reverse (list 1 2 3))
(reverse2 (list 1 2 3))
