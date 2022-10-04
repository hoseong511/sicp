(define (reverse x)
  (define (iter things res)
    (if (null? things)
        res
        (iter (cdr things) (cons (car things) res))))
  (iter x nil))

(define (deep-reverse x)
  (cond ((null? x) nil)
        ((not (list? (car x))) (reverse x))
        (else (cons (deep-reverse (car x))
                      (deep-reverse (cdr x))))))

(define x1 (list (list 1 2) (list 3 4) (list 5 6)))
(reverse x1)

(deep-reverse x1)