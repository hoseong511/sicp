#lang sicp

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (fold-left op init seq)
  (define (iter result x)
    (if (null? x)
        result
        (iter (op result (car x))
              (cdr x))))
  (iter init seq))

(define (fold-right op init seq)
  (accumulate op init seq))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
(equal? (list 2 1) (list 1 2))
(= (2 * 3) (3 * 2))
; 교환법칙이 성립해야한다(피연산자들 자리가 바뀌었을 때, 연산 결과 값이 같아야한다)
; a op b = b op a
; ex) a * b = b * a 
;			a + b = b + a
;			a / b != b / a
;			a - b != b - a
;			(() 1) != (1 ())