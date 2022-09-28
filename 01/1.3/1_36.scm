(define tolerance 0.00001)

(define (fixed-point f first-guess)
	(define (close-enough? x y)
		(< (abs (- x y)) tolerance))
	(define (try guess)
		(let ((next (f guess)))
			(display guess)
			(newline)
			(if (close-enough? guess next)
				next
				(try next))))
	(try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (sqrt2 x)
	(fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (golden-ratio x)
  (fixed-point (lambda (a) (+ 1 (/ 1 a))) x))

(define (function x)
	(fixed-point (lambda (a) (/ (log 1000) (log a))) x))

(define (avg x y)
	(/ (+ x y) 2))

(define (function-avg x)
	(fixed-point (lambda (a) (avg a (/ (log 1000) (log a)))) x))

(golden-ratio 1.0)
(function 2.0)
