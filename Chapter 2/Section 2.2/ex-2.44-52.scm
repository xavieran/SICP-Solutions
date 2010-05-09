- #lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;;Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;Exercise 2.45
(define (split op1 op2)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (op1 painter (op2 smaller smaller)))))
  rec)

;;(define up-split (split below beside))
;;(define right-split (split beside below))
;;In order to define a left split or down-split, we would have
;;to define an above function

;;Exercise 2.46
(define (make-myvect x y)
  (list x y))
(define (xcor-myvect vect)
  (car vect))
(define (ycor-myvect vect)
  (cadr vect))

(define (add-myvect a b)
  (make-myvect (+ (xcor-myvect a) (xcor-myvect b))
             (+ (ycor-myvect a) (ycor-myvect b))))

(define (sub-myvect a b)
  (make-myvect (- (xcor-myvect a) (xcor-myvect b))
             (- (ycor-myvect a) (ycor-myvect b))))

(define (scale-myvect s v)
  (make-myvect (* s (xcor-myvect v))
             (* s (ycor-myvect v))))

;;Exercise 2.47
;;First definition
(define (make-myframe origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-myframe f) (car f))

(define (edge1-myframe f) (cadr f))

(define (edge2-myframe f) (caddr f))

;;Second definition
;;(define (make-myframe origin edge1 edge2)
;;  (cons origin (cons edge1 edge2)))

;;(define (origin-myframe f) (car f))

;;(define (edge1-myframe f) (cadr f))

;;(define (edge2-myframe f) (cddr f))

;;Exercise 2.48
(define (make-mysegment start end)
  (cons start end))

(define (start-mysegment seg) (car seg))

(define (end-mysegment seg) (cdr seg))

;;Exercise 2.49
;;A
(define outline (segments->painter (list
  (make-segment (make-vect 0 0)
                (make-vect 0 1))
  (make-segment (make-vect 0 0)
                (make-vect 1 0))
  (make-segment (make-vect .99 0)
                (make-vect .99 1))
  (make-segment (make-vect 0 .99)
                (make-vect 1 .99)))))
;;B
(define X (segments->painter (list
  (make-segment (make-vect 0 0)
                (make-vect 1 1))
  (make-segment (make-vect 1 0)
                (make-vect 0 1)))))
;;C
(define diamond (segments->painter (list
  (make-segment (make-vect 0 .5)
                (make-vect .5 0))
  (make-segment (make-vect .5 0)
                (make-vect 1 .5))
  (make-segment (make-vect 1 .5)
                (make-vect .5 1))
  (make-segment (make-vect .5 1)
                (make-vect 0 .5)))))
;;D
(define wave (segments->painter (list
  (make-segment (make-vect 0 .84) (make-vect .13 .6))
  (make-segment (make-vect .13 .6) (make-vect .3 .66))
  (make-segment (make-vect .3 .66) (make-vect .4 .66))
  (make-segment (make-vect .4 .66) (make-vect .36 .84))
  (make-segment (make-vect .36 .84) (make-vect .4 1))
  (make-segment (make-vect .6 1) (make-vect .64 .84))
  (make-segment (make-vect .64 .84) (make-vect .6 .66))
  (make-segment (make-vect .6 .66) (make-vect .71 .66))
  (make-segment (make-vect .71 .66) (make-vect 1 .34))
  (make-segment (make-vect 1 .16) (make-vect .6 .44))
  (make-segment (make-vect .6 .44) (make-vect .74 0))
  (make-segment (make-vect .6 0) (make-vect .5 .3))
  (make-segment (make-vect .5 .3) (make-vect .4 0))
  (make-segment (make-vect .24 0) (make-vect .34 .5))
  (make-segment (make-vect .34 .5) (make-vect .3 .6))
  (make-segment (make-vect .3 .6) (make-vect .14 .4))
  (make-segment (make-vect .14 .4) (make-vect 0 .64)))))
                                                                        
;;Exercise 2.50
(define (myflip-horiz painter)
  ((transform-painter (make-vect 1 0)
                      (make-vect 0 0)
                      (make-vect 1 1)) painter))

(define (rotate-90 painter)
  ((transform-painter (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)) painter))

(define (rotate-180 painter)
  ((transform-painter (make-vect 1 1)
                      (make-vect 0 1)
                      (make-vect 1 0)) painter))

(define (rotate-270 painter)
  ((transform-painter (make-vect 0 1)
                      (make-vect 0 0)
                      (make-vect 1 1)) painter))

;;Exercise 2.51
(define (my-below painter1 painter2)
  (let ((split-point (make-vect 0 .5)))
    (let ((paint-top
           ((transform-painter split-point
                               (make-vect 1 .5)
                               (make-vect 0 1)) painter1))
          (paint-bottom
           ((transform-painter (make-vect 0 0)
                               (make-vect 1 0)
                               split-point) painter2)))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (my-below2 painter1 painter2)
  (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter2))))


;;Bonus
(define (sierpinski painter n)
  (if (= n 0)
      painter
            (let ((top ((transform-painter (make-vect .25 0) 
                      (make-vect .75 0) 
                      (make-vect .25 1)) 
                      painter)))
        (sierpinski (below (beside painter painter) top) (- n 1))))) 

(define (crystal painter n)
  (if (= n 0)
      painter
            (let ((top ((transform-painter (make-vect .25 0) 
                      (make-vect .75 0) 
                      (make-vect .25 1)) 
                      painter)))
        (crystal (below (beside (rotate-90 painter) 
                                   (rotate-270 painter)) 
                           top) 
                    (- n 1))))) 
