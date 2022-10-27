#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(define (unique set)
  (cond ((null? set) '())
        ((element-of-set? (car set) (cdr set))
         (unique (cdr set)))
        (else (cons (car set) (unique (cdr set))))))

(define (union-set2 set1 set2)
  (define (union-set set1 set2)
    (cond ((null? set1) set2)
          ((element-of-set? (car set1) set2)
           (union-set (cdr set1) set2))
          (else (cons (car set1) (union-set (cdr set1) set2)))))
  (let ((uni-set1 (unique set1))
        (uni-set2 (unique set2)))
    (union-set uni-set1 uni-set2)))

(define (intersection-set2 set1 set2)
  (define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
           (cons (car set1)
                 (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))
  (let ((uni-set1 (unique set1))
        (uni-set2 (unique set2)))
    (intersection-set uni-set1 uni-set2)))


(define set1 (list 1 1 2 2 3 3 4 4))
(define set2 (list 5 6 1 4 5 5 5))
(union-set2 set1 set2)
(intersection-set2 set1 set2)
(unique (list 2 3 2 1 3 2 2))