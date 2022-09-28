(define (double f)
	(lambda (x) (f (f x))))

(define (inc x)
	(+ x 1))
	
((double inc) 1)
(((double (double double)) inc) 5)
; (double double) -> 4 이고, t로 치환
; (double t) -> 4 * 2 이므로, 8
; (double t) inc -> 8 * 2 이므로, 16