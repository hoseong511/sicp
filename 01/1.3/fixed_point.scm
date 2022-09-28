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

(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)

(define (sqrt x)
	(fixed-point (lambda (y) (/ x y)) 1.0))