(define (add-interval x y)
	(make-interval (+ (lower-bound x) (lower-bound y))
								 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
	(let ((p1 (* (upper-bound x) (upper-bound y)))
				(p2 (* (upper-bound x) (lower-bound y)))
				(p3 (* (lower-bound x) (upper-bound y)))
				(p4 (* (lower-bound x) (lower-bound y))))
		(make-interval (max p1 p2 p3 p4) (min p1 p2 p3 p4))))

(define (div-interval x y)
	(mul-interval x (make-interval 
										(/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
	(make-interval (- (upper-bound x) (lower-bound y))
								 (- (upper-bound y) (lower-bound x))))

(define (make-interval x y) (cons x y))
(define (upper-bound inter)
	(let ((x (car inter))
				(y (cdr inter)))
		(max x y)))
(define (lower-bound inter)
	(let ((x (car inter))
				(y (cdr inter)))
		(min x y)))

(upper-bound (make-interval 2.3 1.2))