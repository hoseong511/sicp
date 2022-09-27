
(define (product f a next b)
	(if (> a b)
		1
		(* (f a) (product f (next a) next b))))

(define (product-iter f a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (f a)))))
	(iter a 1))

(define (pi-quater n)
	(define (pi-term x)
		(/ (* (- x 1) (+ x 1)) (square x)))
	(define (next x)
		(+ 2 x))
	(product pi-term 3.0 next n))

(define (pi n)
	(* 4 (pi-quater n)))

(pi 10000)