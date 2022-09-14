(define (fast-expt b n)
	(define (square x)
		(* x x))
	(define (even? x)
		(= (remainder x 2) 0))
	(define (fast-expt-iter b count result)
		(display count)
		(newline)
		(cond	((= count 0)result)
				((even? count)
					(fast-expt-iter
						(square b)
						(/ count 2)
						result))
				(else
					(fast-expt-iter 
						b
						(- count 1)
						(* result b)))))
	(fast-expt-iter b n 1))