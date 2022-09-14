(define (expt-recur b n) ; 시간복잡도 n, 공간복잡도 n
	(display n)
	(newline)
	(if (= n 0)
		1
		(* b (expt-recur b (- n 1))))) 

(define (expt b n) ; 시간복잡도 n, 공간복잡도 1
	(define (expt-iter base count result)
		(display count)
		(newline)
		(if (= count 0)
			result
			(expt-iter b (- count 1) (* result b))))
	(expt-iter b n 1))
