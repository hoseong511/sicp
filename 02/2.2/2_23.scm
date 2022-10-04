(define (for-each2 proc items)
  (if (null? items)
      #t
      (and (proc (car items))
            (for-each2 proc (cdr items)))))

(for-each2 (lambda (x) (newline) (display x)) (list 1 3 5))