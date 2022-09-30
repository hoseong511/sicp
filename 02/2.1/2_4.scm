(define (cons2 x y)
	(lambda (m) (m x y)))

(define (car2 z)
	(z (lambda (p q) p)))

(define (cdr2 z)
	(z (lambda (p q) q)))

(car2 (cons2 33 22))
