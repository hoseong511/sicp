(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? x y)
		(< (abs (- x y)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (sqrt2 x)
	(fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (golden-ratio x)
  (fixed-point (lambda (a) (+ 1 (/ 1 a))) x))

(sqrt2 10)
(sqrt 10)
(sqrt2 9)
(golden-ratio 1.0)
