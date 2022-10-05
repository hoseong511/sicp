#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (sub) (dot-product v sub)) m))

(define (tranpose m)
  (accumulate-n (lambda (x y) (cons x y)) nil m))

(define (matrix-*-matrix m n)
  (let ((cols (tranpose n)))
    (map (lambda (i) (matrix-*-vector cols i)) m)))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(tranpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(matrix-*-matrix m m)