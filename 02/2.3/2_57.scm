#lang sicp
;
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;(define (make-sum a1 a2) (list '+ a1 a2))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;(define (make-product m1 m2) (list '* m1 m2))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (augend s)(car s))

(define (addend s)
  ;(if (pair? (cdddr s))
   ;   (make-sum (caddr s) (addend (cdr s)))
      (caddr s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplicand p) (car p))

(define (multiplier p)
  ;(if (pair? (cdddr p))
   ;   (make-product (caddr p)
    ;                (multiplier (cdr p)))
      (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (make-exponent base exp)
  (cond ((=number? exp 1) base)
        ((number? base) 0)
        ((number? exp) (list base '** (- exp 1)))
        (else (list base '** (list exp '- 1)))))
  
;deriv 
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (augend exp) var)
                   (deriv (addend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (deriv (multiplicand exp) var)
                        (multiplier exp))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (if (number? (base exp))
             0
             (make-product (exponent exp)
                           (make-exponent (base exp)
                                          (exponent exp)))))
        (else
         (error "unkown expression type -- DERIV" exp))))

(deriv '(x + 2) 'x)
(deriv '(2 * (x * x)) 'x)
(deriv '(x * y) 'x)
(deriv '((x * y) * (x + 3)) 'x)
;(deriv '(** x 4) 'x)
;(deriv '(+ (** x 2) (* 2 x) (+ x 3)) 'x)
;(deriv '(* 4 (** x y)) 'x)
;(deriv '(** 3 4) 'x)
;(deriv '(** x 0) 'x)
;(deriv '(+ x y (+ x 3)) 'x)
;(deriv '(* x y (+ x 3)) 'x)

