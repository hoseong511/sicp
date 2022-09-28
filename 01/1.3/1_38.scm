(define (cont-frac-iter n d k)
	(define (iter k result)
		(if (= k 0)
			result
			(iter (- k 1) (/ (n k) (+ (d k) result)))))
	(iter k 0))

(cont-frac-iter (lambda (i) 1.0)
								(lambda (i)
									(cond ((= (remainder i 3) 1) (* 2 (+ 1 (floor (/ i 3)))))
												(else 1.0)))
         			  5)

(define (pred-e k)
	(+ 2
		(cont-frac-iter (lambda (i) 1.0)
										(lambda (i)
											(cond ((= (remainder i 3) 2)
														(* 2 (+ 1 (floor (/ i 3)))))
														(else 1.0)))
										k)))