(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (below2 painter2 painter)
  (let ((split-pint (make-vect 0.0 0.5)))
    (let ((painter-up
           (transform-painter2 painter1
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 1.0)))
          (painter-down
           (transform-painter2 painter2
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)))
      (lambda (frame)
        (painter-up frame) (painter-down frame)))))