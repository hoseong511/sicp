(define (cube a)
	(* a a a))

(define (inc a)
	(+ a 1))

(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))))

(define (sum-cubes a b)
	(sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
	(sum identity a inc b))

(define (sum-pi a b)
	(define (pi-term x)
		(/ 1.0 (* x (+ x 2))))
	(define (pi-next x)
		(+ x 4))
	(sum pi-term a pi-next b))

(define (integral f a b dx)
	(define (add-dx x) (+ x dx))
	(* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(sum-cubes 1 10)
(sum-integers 1 10)
(integral cube 0 1 0.01)