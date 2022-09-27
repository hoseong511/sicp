(define (square x)
	(* x x))

(define (report-prime elapsed-time n)
	(display n)
	(display "***")
	(display elapsed-time)
	(newline))

(define (expmod base exp m)
	(cond ((= exp 0) 1)
		  ((even? exp)
			(remainder (square (expmod base (/ exp 2) m)) m))
		  (else 
			(remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
	(define (try-it a)
		(= (expmod a n n) a))
	(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
	(cond ((= times 0) true)
		  ((fermat-test n) (fast-prime? n (- times 1)))
		  (else false)))

(define (start-prime-test n start-time)
	(if (fast-prime? n 100)
		(report-prime (- (runtime) start-time) n)))

(define (timed-prime-test n)
	(start-prime-test n (runtime)))

(define (iter from to)
	(cond	((> from to) (display "\nend"))
			(else (timed-prime-test from) (iter (+ from 2) to))))

(define (search-for-primes from to)
	(if (even? from)
		(iter (+ from 1) to)
		(iter from to)))

(search-for-primes 100000 100200)
(search-for-primes 10000000 10000200)
(search-for-primes 1000000000 1000000200)

