(define (list-ref items n)
(if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (iter lst count)
    (if (null? lst)
        count
        (iter (cdr lst) (+ count 1))))
  (iter items 0))

(define (last-pair items)
	(list-ref items (- (length items) 1)))

(define odds (list 1 3 5 7))
(last-pair odds)