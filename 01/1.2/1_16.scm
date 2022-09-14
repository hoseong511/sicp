(define (fast-expt b n)
	(define (square x)
		(* x x))
	(define (even? n)
		(= (remainder n 2) 0))
	(define (fast-expt-iter b count result)
		(display result)
		(newline)
		(if (= count 0)
			result
			(cond	((even? n) 
						(fast-expt-iter
							b
							(- count 2)
							(* result (square b))))
					(else
						(fast-expt-iter 
							b
							(- count 1)
							(* result b))))))
	(fast-expt-iter b n 1))