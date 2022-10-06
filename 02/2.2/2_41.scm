#lang sicp

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq) (filter predicate (cdr seq))))
        (else (filter predicate (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-interval from to)
  (if (> from to)
      nil
      (cons from (enumerate-interval (+ from 1) to))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (permutations s)
  (define remove
    (lambda (item seq)
      (filter (lambda (x) (not (= item x))) seq)))
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (fringe s)
  (cond ((null? s) nil)
        ((not (pair? s)) (list s))
        (else (append (fringe (car s))
                      (fringe (cdr s))))))

; 처음 생각했던 방식
; (define (triple-pair n)
;   (define (remove-null seq)
;     (filter (lambda (x) (not (null? x))) seq))
;   (remove-null (flatmap (lambda (i)
;                           (map (lambda (j)
;                                  (map (lambda (k) (list i j k))
;                                       (enumerate-interval 1 (- j 1))))
;                                (enumerate-interval 1 (- i 1))))
;                         (enumerate-interval 1 n))))
; (define (bind-triple s)
;   (let ((refined (fringe s)))
;     (if (null? refined)
;         nil
;         (let ((x (car refined))
;               (y (car (cdr refined)))
;               (z (car (cdr (cdr refined)))))
;           (cons (list x y z) (bind-triple (cdr (cdr (cdr refined)))))))))

; (define (pick-permutations n)
;   (map permutations (bind-triple (triple-pair n))))

; (pick-permutations 6)

(define (triple-pair n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(define (pick-permutations n)
	(map permutations (triple-pair n)))

(define (triples-of-sum s n)
  (filter (lambda (seq) (= (accumulate + 0 seq) s)) 
          (triple-pair n)))
(triples-of-sum 20 30)