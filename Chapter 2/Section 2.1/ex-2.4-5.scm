#lang scheme
;;Exercise 2.4
;;To show this, we will use the substitution model
;;Let's apply car to (cons 4 8)
;;(car (cons 4 8))
;;(car (lambda (m) (m 4 8)))
;;((lambda (m) (m 4 8)) (lambda (p q) p))
;;((lambda (p q) p) 4 8)
;;4

;;Definition of cdr

;;(define (cdr z) (lambda (p q) q))

;;Exercise 2.5
;;This was tough... took me an hour of fiddling with various calculations
;;Eventually, I got it...
;;This is related to godelization...
;;Check it out on wikipedia

(define (divisible-by-2 x)
  (if (not (integer? x))
      false
      (= (remainder x 2) 0)))
      
(define (divisible-by-3 x)
  (if (not (integer? x))
      false
      (= (remainder x 3) 0)))
      
      
(define (int-cons a b)
  (* (expt 2 a)
     (expt 3 b)))
     
(define (int-car p)
  (define (try q n)
    (if (not (divisible-by-2 q))
        n
        (try (/ q 2) (+ n 1))))
  (try (/ p 3) 0))

(define (int-cdr p)
  (define (try q n)
    (if (not (divisible-by-3 q))
        n
        (try (/ q 3) (+ n 1))))
  (try (/ p 2) 0))

(newline)
(display (int-car (int-cons 3 7)))
(newline)
(display (int-cdr (int-cons 8 12)))
(newline)
