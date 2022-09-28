(define (close-enough? x y)
	(< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
	(let ((midpoint ((lambda (x y) (/ (+ x y) 2.0)) neg-point pos-point)))
		(if (close-enough? neg-point pos-point)
			midpoint
			(let ((test-val (f midpoint)))
				(cond ((positive? test-val)
						(search f neg-point midpoint))
					  ((negative? test-val)
					  	(search f midpoint pos-point))
					  (else midpoint))))))

(define (half-interval-method f a b)
	(let ((a-val (f a))
		  (b-val (f b)))
		(cond ((and (negative? a-val) (positive? b-val))
				(search f a b))
			  ((and (negative? b-val) (positive? a-val))
			  	(search f b a))
			  (else 
			  	(error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method sin 3.5 4.0)