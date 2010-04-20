#lang scheme
;;Exercise 1.29
;;Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;Exercise 1.21a
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (next x) (+ 1 x))
  (define (term x) x)
  (product term 1 next n))

;;Exercise 1.21b
(define (product term a next b)
  (cond ((> a b) 1)
        (else (* (term a) (product term (next a) next b)))))
