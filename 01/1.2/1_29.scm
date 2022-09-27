(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b))))

(define (simpson f a b n)
	(define (next x)
		(+ x 1))
	(define h
		(/ (- b a) n))
	(define (y k)
		(f (+ a (* k h))))
	(define (simp-term k)
		(cond ((= k 0) (y 0))
			  ((= k n) (y n))
			  ((even? k) (* 2 (y k)))
			  (else (* 4 (y k)))))
	(* (/ h 3) (sum simp-term 0 next n)))

(simpson cube 0 1.0 1000)