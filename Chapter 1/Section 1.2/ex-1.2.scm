#!/usr/bin/env mzscheme
#lang scheme


(define (f-rec n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3)))))))


;;WT??!
(define (F-iter n)
(if (< n 3)
  n
  (F-iter-aux 2 1 0 n)))

(define (F-iter-aux a b c count)
(if (= count 2)
  a
  (F-iter-aux (+ a (* 2 b) (* 3 c))
              a
              b
              (- count 1))))
;;
;;(display (F-iter 5))
;;(display (f-rec 5))


;;Exercise 1.12

(define (pascal x y)
  (cond ((= x 1) 1)
        ((or (> x y) (< x 1)) 0)
        (else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))))
        
;;(pascal 2 4)

;;Exercise 1.15
(define (cube x ) (* x x x))

(define (p x)
    (display '-!)
    (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
    (if (not (> (abs angle) .1))
          angle
          (p (sine (/ angle 3.0)))))


;;(define x 8192)
;;(display x)
;;(sine x)
;;It's run 5 times
;;x n
;;2 3
;;4 4
;;8 4
;;16 5
;;32 6
;;64 6
;;128 7
;;256 8
;;512 8
;;1024 9
;;2048 10
;;4096 10
;;8192 11
;;Looks logarithmic...
;;As x has to be increased a lot for a corresponding increase in n

;;Exercise 1.16
;;Transformations:
;;b<-b
;;n<-n-1
;;a<-b*a
;;If n even
;;b<-b^2
;;n<-n/2
;;a<-a:w

(define (even? x)
    (= (remainder x 2) 0))
(define (square x) (* x x))

(define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n) (expt-iter b n 1))

;;(fast-expt 2 8)
;;(fast-expt 3 9)
;;(fast-expt 2 1002)

;;Exercise 1.17

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mul a x)
    (if (= x 0)
        0
        (+ a (mul a (- x 1)))))

;;(mul 2 4)
;;(mul 4 5)

(define (mul-rec a x)
    (cond ((= x 0) 0)
          ((even? x) (double (mul-rec a (/ x 2))))
          (else (+ a (mul-rec a (- x 1))))))

;;(fast-mul 6 8192)
(define (fast-mul b n)
    (define (mul-iter b n a)
        (cond ((= n 0) a)
              ((even? n) (mul-iter (double b) (halve n) a))
              (else (mul-iter b (- n 1) (+ b a)))))
    (mul-iter b n 1))

;;(fast-mul 6 16782)

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n) (find-divisor n 2))

;;(smallest-divisor 199)
;;(smallest-divisor 1999)
;;(smallest-divisor 19999)


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (prime? n (- times 1)))
        (else false)))


(define (mr-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (and 
               (not (and (= 1 base) (= (- m 1) base))) 
               (= (square base) (remainder 1 m))) 
             0 
             (remainder (square (mr-expmod base (/ exp 2) m)) m)))
        (else (remainder (* base (mr-expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (define ans (mr-expmod a (- n 1) n))
    (if (= ans 0)
        false
        (= (expmod a (- n 1) n) (remainder 1 n))))
  (try-it (+ 1 (random (- n 1)))))

(define (mr-prime n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (mr-prime n (- times 1)))
        (else false)))

(mr-prime 6 5)
(mr-prime 3 7)
(mr-prime 1999 10)

(define (test-carmichael n a)
  (cond ((> a n) true)
        ((= (expmod a n n) (remainder a n)) (test-carmichael n (+ a 1)))
        (else false)))


;;(test-carmichael 561 1)
;;(test-carmichael 1105 1)
;;(test-carmichael 1729 1)
;;(test-carmichael 2465 1)
;;(test-carmichael 2821 1)
;;(test-carmichael 6601 1)
