(define (compose f g)
	(lambda (x) (f (g x))))

(define (repeated f n)
	(if (= n 1)
		(lambda (x) (f x))
		(repeated (compose f f) (- n 1))))

(define (square x) (* x x))

((repeated square 2) 5)