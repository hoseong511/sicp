#lang sicp

(define (prime? x)
  (define divides? (lambda (x y) (= (remainder x y) 0)))
  (define square (lambda (x) (* x x)))
  (define (find-divisor x test-divisor)
    (cond ((> (square test-divisor) x) x)
          ((divides? x test-divisor) test-divisor)
          (else (find-divisor x (+ test-divisor 1)))))
  (= ((lambda (x) (find-divisor x 2)) x) x))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (+ 1 from) to))))
  
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(enumerate-interval 1 10)
(prime-sum-pairs 10)