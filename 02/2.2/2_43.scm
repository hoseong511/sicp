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

(define empty-board nil)

(define (safe? k pos)
  (define (last seq)
    (if (null? (cdr seq))
        (car seq)
        (last (cdr seq))))
  (define (check? last seq)
    (let ((x (car (car seq)))
          (y (cdr (car seq))))
      (cond ((equal? (car seq) last) #t)
            ((= (car (car seq)) (car last)) #f)
            ((= (abs (/ (- x (car last)) (- y (cdr last)))) 1) #f)
            (else (check? last (cdr seq))))))
  (if (= k 1)
      #t
      (check? (last pos) pos)))

(define adjoin-position
  (lambda (x y z) (append z (list (cons x y)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board) 
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (new-row)
                           (map (lambda (rest-of-queens)
                                  (adjoin-position new-row k rest-of-queens))
                                (queen-cols (- k 1))))
                         (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(queens 8)

; 점 하나를 만들 때 마다 재귀호출된 함수를 기다려야 하므로 오래걸린다.
; ex 1 -> queen-cols * (board-size - 1), 2 -> queen-cols * (board-size - 1), ...
; board-size^2 만큼이 소요된다.

; 1 -> queen-cols 