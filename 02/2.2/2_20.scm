#lang sicp

(define (same-pairity x . z)
  (let ((tmp (list))
        (check (if (even? x)
                   even?
                   odd?)))
    (define (iter lst)
      (if (null? lst)
          nil
          (if (check (car lst))
              (cons (car lst) (iter (cdr lst)))
              (iter (cdr lst)))))
    (cons x (iter z))))

(same-pairity 1 2 3 4 5 6 7)
(same-pairity 4 2 3 4 5 6 7)