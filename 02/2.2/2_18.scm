#lang sicp

(define (length items)
  (define (iter lst count)
    (if (null? lst)
        count
        (iter (cdr lst) (+ count 1))))
  (iter items 0))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

(define (list-ref items n)
(if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (reverse items)
  (define (iter lst1 lst2 len)
    (if (= len 0)
        lst2
        (iter lst1 (append lst2 (list (list-ref items (- len 1)))) (- len 1))))
  (iter items (list) (length items)))

(reverse (list 1 4 9 16 25))