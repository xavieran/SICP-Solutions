#lang scheme

;;Exercise 1.40
;;Can't do it yet.

;;Exercise 1.41

(define (double g) (lambda (x) (g (g x))))

;;(((double (double double)) inc) 5) -> 21

;;Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;;Exercise 1.43
(define (square x) (* x x))

(define (repeated f n)
  (define (recur g a)
    (if (= a n)
        g
        (recur (compose f g) (+ a 1))))
  (recur f 1))

;;((repeated square 2) 5) -> 625


;;Exercise 1.44
(define (smooth f)
  (define (average a b c) (/ (+ a b c) 3))
  (let ((dx .00001))
    (lambda (x)
      (average (f x)
               (f (- x dx))
               (f (+ x dx))))))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

(define (fixed-point f first-guess)
  (let ((tolerance .00000001))
    (define (close-enough? v1 v2)
      (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
  (try first-guess)))


(define (average-damp f)
  (define (average x y) (/ (+ x y) 2))
  (lambda (x) (average x (f x))))

(define (log2 n)
  (if (= 1 n)
  0
  (+ 1 (log2 (floor (/ n 2))))))
  
(define (nth-root x n)
  (fixed-point ((repeated average-damp (log2 n)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))
  
;;Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (result x)
    (define (iter x)
      (if (good-enough? x)
          x
          (iter (improve x))))
  (iter x))
  result)

