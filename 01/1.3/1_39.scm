(define (cont-frac-iter n d k)
	(define (iter k result)
		(if (= k 0)
			result
			(iter (- k 1) (/ (n k) (- (d k) result)))))
	(iter k 0))

(cont-frac-iter (lambda (i) 1.0)
								(lambda (i)
									(cond ((= (remainder i 3) 1) (* 2 (+ 1 (floor (/ i 3)))))
												(else 1.0)))
         			  5)

(define (tan-cf x k)
	(cont-frac-iter 
		(lambda (i)
			(if (= i 1)
					x
					(square x)))
		(lambda (i)
			(- (* 2 i) 1))
		k))