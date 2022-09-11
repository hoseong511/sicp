(define (cube-root-iter guess x)
	(display guess)
	(newline)
	(if (good-enough? guess x)
		guess
		(cube-root-iter (improve guess x) x)
	)
)

(define (improve guess x)
	(average guess (/ x (square guess))))

(define (average x y)
	(/ (+ x (+ y y) 3)))

(define (good-enough? guess x)
	(< (abs (- (/ (t-square guess) x) 1)) 0.001))

(define (t-square x) 
	(* x x x))

(define (square x) 
	(* x x))

(define (cube-root x)
	(cube-root-iter 1.0 x))