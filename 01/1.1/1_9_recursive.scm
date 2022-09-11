(define (inc x)
	(+ x 1))

(define (dec x)
	(- x 1))

(define (p a b)
	(display a) (display " ") (display b)
	(newline)
	(if (= a 0)
		b
		(inc (p (dec a) b))))