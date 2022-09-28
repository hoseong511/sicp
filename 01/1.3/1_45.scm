(define tolerance 0.00001)

(define (error x)
	(display x)
	(newline))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (fixed-point f first-guess)
	(define (close-enough? x y)
		(< (abs (- x y)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (compose f g)
	(lambda (x) (f (g x))))

(define (repeated f n)
	(if (= n 1)
		(lambda (x) (f x))
		(repeated (compose f f) (- n 1))))

(define (n-th f n)
	(if (= n 1)
		(lambda (x) (f x))
		(n-th (lambda (x) (* x (f x))) (- n 1))))

(define (avg-damp f)
	(lambda (x) (average x (f x))))

(define (n-th-sqrt x n)
	(define (n-avg-damp n)
		(repeated avg-damp n))
	(define (n-square n)
		(n-th (lambda (x) x) n))
	(if (< n 2)
		(error "n > 1!")
		(let ((n-temp 
							((n-avg-damp (- n 1)) (lambda (y) (/ x ((n-square (- n 1)) y))))))
		(fixed-point n-temp 1.0))))
	
(n-th-sqrt 4 2)
(n-th-sqrt 1000 3)
(n-th-sqrt 10000 4)
(n-th-sqrt 100000 5)
(n-th-sqrt 1000000 6)
(n-th-sqrt 1000000 1)