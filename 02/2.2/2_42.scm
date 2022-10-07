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
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 8)
; 1. 자료구조 (여기에서 오래걸림)
; 2. 작성된 코드에서 작동되는 방식에 대한 이해
; 3. 알고리즘 -> dfs
; ( () () () ) -> 이러한 형식의 자료를 safe? 검사함, 처음부터 마지막 까지 추가된 좌표를 검사한다.
