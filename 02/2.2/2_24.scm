(define items (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr items)))))

(define items2 (list (list 7)))
(car (car items2))

(define items3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items3))))))))))))
