(define (report-prime elapsed-time n)
	(display n)
	(display "***")
	(display elapsed-time)
	(newline))

(define (divides? a b) 
		(= (remainder b a) 0))

(define (find-divisor n test-divisor)
	(cond	((> (square test-divisor) n) n)
			((divides? test-divisor n) test-divisor)
			(else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
	(find-divisor n 2))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (start-prime-test n start-time)
	(if (prime? n)
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