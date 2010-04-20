#lang scheme
;;Exercise 1.30-1.31
;;(define (sum term a next b)
;; (if (> a b)
;;      0
;;      (+ (term a) (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral f a b n) 
  (define h (/ (- b a) n))
  (define (next a) (f (+ a (* n h))))
  (* (/ h 3) (sum f a next b)))

(define (square x) (* x x))

(define (cube x) (* x x x))

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
;;(factorial 6)


(define (prod-squares a n)
  (define (next x) (+ x 2))
  (define (term x) (square x))
  (product term a next n))
;;Doesn't work properly for some reason...
(define (pi-approx n)
  (* 4 (/ (* 2 (prod-squares 4 n)) (prod-squares 3 n))))

;;(pi-approx 5)

;;Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))


(define (fact-acc n)
  (define (next x) (+ 1 x))
  (define (term x) x)
  (prod-acc term 1 next n))
(fact-acc 6)
