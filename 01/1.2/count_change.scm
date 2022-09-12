(define (count-change amount)
	(cc amount 5))

(define (cc amount kind-coins)
	(cond	((= amount 0) 1)
			((or (< amount 0) (= kind-coins 0)) 0)
			(else (+ (cc amount 
						(- kind-coins 1))
					 (cc (- amount
					 	(first-denomination kind-coins))
						kind-coins)))))

(define (first-denomination kind-coins)
	(cond	((= kind-coins 1) 1)
			((= kind-coins 2) 5)
			((= kind-coins 3) 10)
			((= kind-coins 4) 25)
			((= kind-coins 5) 50)))