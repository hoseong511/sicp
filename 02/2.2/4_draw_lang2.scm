#lang sicp
(#%require sicp-pict)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (split x y)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split x y) painter (- n 1))))
          (x painter (y smaller smaller))))))

(define right-split (split beside below))

(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 4))
(paint (up-split einstein 4))

(paint diagonal-shading)

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (car (cdr (cdr frame))))
  

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
  

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-segment start end) (cons start end))
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))



(define a-frame (make-frame (make-vect 1 2) (make-vect 5 5) (make-vect 9 9)))
((frame-coord-map a-frame) (make-vect 0 0))

(define a-segment (segment (vect 0 0) (vect 0 1)))
(define b-segment (segment (vect 0 0) (vect 1 0)))
(define c-segment (segment (vect 1 0) (vect 1 1)))
(define d-segment (segment (vect 0 1) (vect 1 1)))

(define outline
  (segments->painter
   (list a-segment b-segment
         c-segment d-segment)))
(define x
  (segments->painter
   (list (segment (vect 0 0) (vect 1 1))
         (segment (vect 1 0) (vect 0 1)))))

(define diamond
  (segments->painter
   (list (segment (vect 0.5 0) (vect 0 0.5))
         (segment (vect 0 0.5) (vect 0.5 1))
         (segment (vect 0.5 1) (vect 1 0.5))
         (segment (vect 1 0.5) (vect 0.5 0)))))

(define wave
  (segments->painter
   (list (segment (vect .25 0) (vect .35 .5)) 
      (segment (vect .35 .5) (vect .3 .6)) 
      (segment (vect .3 .6) (vect .15 .4)) 
      (segment (vect .15 .4) (vect 0 .65)) 
      (segment (vect 0 .65) (vect 0 .85)) 
      (segment (vect 0 .85) (vect .15 .6)) 
      (segment (vect .15 .6) (vect .3 .65)) 
      (segment (vect .3 .65) (vect .4 .65)) 
      (segment (vect .4 .65) (vect .35 .85)) 
      (segment (vect .35 .85) (vect .4 1)) 
      (segment (vect .4 1) (vect .6 1)) 
      (segment (vect .6 1) (vect .65 .85)) 
      (segment (vect .65 .85) (vect .6 .65)) 
      (segment (vect .6 .65) (vect .75 .65)) 
      (segment (vect .75 .65) (vect 1 .35)) 
      (segment (vect 1 .35) (vect 1 .15)) 
      (segment (vect 1 .15) (vect .6 .45)) 
      (segment (vect .6 .45) (vect .75 0)) 
      (segment (vect .75 0) (vect .6 0)) 
      (segment (vect .6 0) (vect .5 .3)) 
      (segment (vect .5 .3) (vect .4 0)) 
      (segment (vect .4 0) (vect .25 0)))))

(paint (below (beside diamond wave) (beside outline x)))

(define (transform-painter2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-vert2 painter)
  (transform-painter2 painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (vect 0.5 0.5)
                     (vect 1.0 0.5)
                     (vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (vect 1.0 0.0)
                     (vect 1.0 1.0)
                     (vect 0.0 0.0)))

(define (flip-horiz2 painter)
  (transform-painter painter
                     (vect 1.0 0.0)
                     (vect 0.0 0.0)
                     (vect 1.0 1.0)))

(define (beside2 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((painter-left
           (transform-painter2 painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (painter-right
           (transform-painter2 painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
    (lambda (frame)
      (painter-left frame)
      (painter-right frame)))))
  
(paint (shrink-to-upper-right wave))
(paint (rotate90 wave))