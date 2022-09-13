(define (pascal a b)
	(if (or (= b 1) (= b a))
		1
		(+ (pascal (- a 1) b)
			(pascal (- a 1) (- b 1)))))