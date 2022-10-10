#lang sicp

(define a 2)
(define b 3)

(list a b)
(list 'a)
(list 'a b)
(list '(a b))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue shocks)))
(memq 'red '(red shoes blud shocks))

(define (equal? a b)
  (cond ((null? a) true)
        ((not (eq? (car a) (car b))) false)
        (else (equal? (cdr a) (cdr b)))))

(equal? '(apple banana) '(apple bananas))