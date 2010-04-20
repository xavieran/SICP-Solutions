#lang scheme

;;Exercise 2.1
(define (numer rat) (car rat))
(define (denom rat) (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((n (if (< d 0)
               (- n)
               n))
        (d (if (< d 0)
               (- d)
               d)))
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g)))))

(define (print-rat rat)
  (newline)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  (newline))
  
(print-rat (make-rat -5 -6))
(print-rat (make-rat -8 -4))
