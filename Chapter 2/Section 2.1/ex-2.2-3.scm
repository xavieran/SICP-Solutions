#lang scheme
;;Exercise 2.2

;;Segment constructor and selectors
(define (make-segment a b) (cons a b))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
;;Point constructor and selectors
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;;Midpoint
(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s))
                    (x-point (end-segment s)))
                 2)
              (/ (+ (y-point (start-segment s))
                    (y-point (end-segment s)))
                 2)))
                 
(print-point (midpoint-segment (make-segment (make-point 2 12) (make-point 8 4))))

;;Exercise 2.3
(define (make-dim l h) (cons l h))
(define (length-dim d) (car d))
(define (height-dim d) (cdr d))

;;First implementation of make-rect.
;;Represented as a top-left corner, and a height and length
(define (make-rect top-left dim) (cons top-left dim))
(define (top-left-rect r) (car r))
(define (dim-r r) (cdr r))
(define (length-rect r) (length-dim (dim-rect r)))
(define (height-rect r) (height-dim (dim-rect r)))

;;Second implementation of make-rect
;;Implemented as a top-left and bottom-right corner
(define (make-rect top-left bottom-right) (cons top-left bottom-right))
(define (top-left-rect r) (car r))
(define (bottom-right-rect r) (cdr r))
(define (length-rect r)
  (- (point-x (bottom-right-rect r))
     (point-x (top-left-rect r))))
(define (height-rect r)
  (- (point-y (bottom-right-rect r))
     (point-y (top-left-rect r))))
     
;;Area and perimeter of either rect
(define (area-rect r)
  (* (length-rect r) (height-rect r)))
(define (perimeter-rect r)
  (+ (* (length-rect r) 2)
     (* (height-rect r) 2)))


