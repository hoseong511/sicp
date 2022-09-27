(define (accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner result (term a)))))
	(iter a null-value))

(define (accumulate-re1 combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a)
				  (accumulate-re1 combiner null-value term (next a) next b))))
				  
(define (accumulate-re2 combiner null-value term a next b)
	(define (iter a)
		(if (> a b)
			null-value
			(combiner (iter (next a)) (term a))))
	(iter a))

(define (sum term a next b)
	(accumulate + 0 term a next b))

(define (prodcut term a next b)
	(accumulate * 1 term a next b))