(define (f n)
	(if (< n 3)
		n
		(f-iter n 2 1 0)))

(define (f-iter n a b c)
	(display a)
	(newline)
	(if (= n 2)
		a
		(f-iter (- n 1) (+ a (* 2 b) (* 3 c)) a b)))

(define (f-recur n)
	(cond	((< n 3) n)
			(else (+ (f-recur (- n 1))
					 (* 2 (f-recur (- n 2)))
					 (* 3 (f-recur (- n 3)))))))