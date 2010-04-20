#lang scheme
;;Exercise 1.32a
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
  (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null))

;;Exercise 1.32b
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null
      (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))))

;;Exercise 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (sum-square-primes a b)
  (define (term x) (square x))
  (define (next x)
    (if (even? x)
        (+ x 1)
        (+ x 2)))
  (filtered-accumulate prime? + 0 term a next b))

(define (product-rel-prime n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (define (rel-prime? x)
    (and (< x n) (= (gcd x n) 1)))
  (filtered-accumulate rel-prime? * 1 term 1 next n))
