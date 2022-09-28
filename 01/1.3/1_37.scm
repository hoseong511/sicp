(define (cont-frac n d k)
    (if (= k 1)
        (/ (n 1) (d 1))
				(/ (n 1) (+ (d 1) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
	(define (iter k result)
		(if (= k 0)
			result
			(iter (- k 1) (/ (n k) (+ (d k) result)))))
	(iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           6)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           7)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           8)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           9)

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           5)

(define (iter i)
	(if (= i 0)
		0
		((display 
			(cont-frac (lambda (i) 1.0)
    						(lambda (i) 1.0)
								i))
		 (iter (- i 1)))))