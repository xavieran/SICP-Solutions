;;Put ex 1.11 here

;;Exercise 1.12

(define (pascal x y)
  (cond ((= x 1) 1)
        ((or (< x 1) (> x y)) 0)
        (else (+ (pascal (-x 1) (- y 1)) (pascal x (- y 1))))))

;;Exercise 1.13
;;Can't do this

;;Exercise 1.14
;;Tree was drawn on paper
;;It is O(n^5) growth

;;Exercise 1.15
;;Give answer

;;Exercise 1.16
;;Need to define even? and square
(define (even? x) (= (remainder x 2) 0))
(define (square x) (* x x))

(define (fast-expt b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt (square b) (/ n 2) a))
        (else (fast-expt b (- n 1) (* a b)))))

;;Exercise 1.17
;;Need to define double and halve
(define (double x) (+ x x))
(define (halve x) (/ x 2))


(define (fast-mul a x)
    (cond ((= x 0) 0)
          ((even? x) (double (mul-rec a (/ x 2))))
          (else (+ a (mul-rec a (- x 1))))))

;;Exercise 1.18
(define (fast-mul-iter b n)
    (define (mul-iter b n a)
        (cond ((= n 0) a)
              ((even? n) (mul-iter (double b) (halve n) a))
              (else (mul-iter b (- n 1) (+ b a)))))
    (mul-iter b n 1))
