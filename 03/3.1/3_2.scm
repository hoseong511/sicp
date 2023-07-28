#lang sicp

(define (make-accumulator init_val)
  (define (sum amount)
    (begin (set! init_val (+ init_val amount))
           init_val))
  sum)

(define A (make-accumulator 10))

(A 15)
(A 20)

(define (make-monitored f)
  (let ((cnt 0))
    (define (oper num)
      (begin (set! cnt (+ cnt 1))
             (f num)))    
    (define (dispatch param)
      (cond ((eq? param 'how-many-calls?) cnt)
            ((eq? param 'reset-count) (set! cnt 0))
            ((if (number? param) (oper param)))
            (else (error "no param" param))))
    dispatch))

(define s (make-monitored sqrt))
(s 10)
(s 10)
(s 'how-many-calls?)