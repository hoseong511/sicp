#lang sicp

(car (cons 1 (cons 2 (cons 3 (cons 4 nil)))))

(define one-through-four (list 1 2 3 4))

one-through-four
;(car one-through-four)
;(cdr one-through-four)
;(car (cdr (cdr one-through-four)))
;(cons 10 one-through-four)

(define (list-ref items n)
(if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9))

(list-ref squares 3)

(define (length items)
  (define (iter lst count)
    (if (null? lst)
        count
        (iter (cdr lst) (+ count 1))))
  (iter items 0))

(length squares)
(length odds)

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (append (cdr lst1) lst2))))

(append squares odds)