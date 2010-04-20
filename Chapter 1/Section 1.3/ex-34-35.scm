#lang scheme
;;Exercise 1.34
;;The interpreter will fail, because f will try to apply
;;the value 2 as a function, ie.
;;(f f)
;;(f 2)
;;(2 2)
;;(ERROR!!

;;Exercise 1.35
;;Can't do this



(define (fixed-point f first-guess)
  (define tolerance .0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;Exercise 1.35
(define (golden-approx)
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

(golden-approx)
