#lang scheme
;;Exercise 2.7
;;Stuff already defined by the authors:


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;;(define (div-interval x y)
;;  (mul-interval x 
;;                (make-interval (/ 1.0 (upper-bound y))
;;                               (/ 1.0 (lower-bound y)))))


(define (make-interval a b) (cons a b))


(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;;Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

;;Exercise 2.9
;;The width of an interval being added is:
;;w(x+y) = w(x)+w(y)
;;The width of an interval being subtracted is:
;;w(x-y) = w(x)-w(y)
;;
;;Exercise 2.10
(define (zero-span? i)
  (if (and (< (lower-bound i) 0) (> (upper-bound i) 0))
      true
      false))

(define (div-interval x y)
  (if (zero-span? y) 
      (error "Attempt to divide by an interval that spans 0")
      (mul-interval x 
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 1 10) (make-interval -3 4))
;;Exercise 2.11
;;nope

;;Exercise 2.12
;;
(define (make-center-percent c p)
  (make-interval (- c (* c p))
                 (+ c (* c p))))

(define (percent i)
  (/ (- (upper-bound i)
        (center i))
     (center i)))

;;Exercise 2.13
;;Let' try:
;;(define a (make-center-percent 1 .1))
;;(define b (make-center-percent 1 .2))
;;(define x (* a b))
;;(percent x)=.294
;;So we can see that (percent x) ~= (percent a) + (percent b)

;;Exercise 2.14
;;(define A (make-center-percent 8 .01))
;;(define B (make-center-percent 9 .05))
;;Let's process (/ A A)
;;(mul-interval A (make-interval (/ 1 8.08) (/ 1 7.92)))
;;p1 = .980
;;p2 = 1
;;p3 = 1
;;p4 = 1.02
;;So the result is: (make-interval .980 1.02)
;;Instead of (make-interval 1 1)
;;Let's do (/ A B)
;;(mul-interval A (make-interval (/ 1 9.45) (/ 1 (8.55))))
;;p1 = .838
;;p2 = .926
;;p3 = .855
;;p4 = .945
;;So the result is: (make-interval .838 .945)
;;From these two examples, we can see that the error in the answer
;;is directly related to the uncertainty in the intervals given

;;Exercise 2.15
;;She is correct. As stated above, the amount of error in the answer
;;is directly related to the uncertainty in the intervals given.
;;Because there are more intervals in par1, there is a higher 
;;error in the answer. That is why par2 is a better method.

;;Exercise 2.16
;;To be honest, as a 16 year old high school student, I don't think
;;I'm qualified to devise an interval-arithmetic package without these
;;limitations. :)
