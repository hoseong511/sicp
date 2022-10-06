#lang sicp

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (+ 1 from) to))))
  
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (remove item seq)
  (filter (lambda (x) (not (= item seq)))
          seq))

(define (permutations seq)
  (if (null? seq)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               seq)))