#lang scheme
;;Exercise 36
(define (fixed-point f first-guess)
  (define tolerance .0001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))


(define (solve-x-avg)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 10.0))

(define (solve-x)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 10.0))

;;(newline)
;;(display "No average")
;;(solve-x)
;;(newline)
;;(display "Average")
;;(solve-x-avg)

;;We can see that it takes 28 steps to compute without an average, and only 8
;;steps with an average.

;;Exercise 1.37
;;Part A (This one is recursive)
(define (cont-frac n d k)
  (define (rec a)
    (if (= a k)
        (/ (n k) (d k))
        (/ (n a) (+ (d a) (rec (+ a 1))))))
  (rec 1))


(define (approx-kappa k)
  (/ 1 (cont-frac-i (lambda (i) 1.0) (lambda (i) 1.0) k)))

;;(approx-kappa 10)
;;(approx-kappa 11)
;;(approx-kappa 12)
;;k must be >= 12

;;Part B (This one is iterative)
(define (cont-frac-i n d k)
  (define (iter a result)
    (if (< a 0)
        result
        (iter (- a 1) (/ (n a) (+ (d a) result)))))
  (iter k (/ (n k) (d k))))


;;Exercise 1.38
;;Definition of D stolen from eli...
;;My version did not work perfectly, and was rather slow...
(define (e-approx k)
  (define (N i) 1.0)
  (define (D i)
    (let ((incI (+ i 1)))
      (if (= (remainder incI 3) 0)
          (* 2 (/ incI 3))
          1)))
  (+ 2 (cont-frac N D k)))
;;(e-approx 1000)

;;Exercise 1.39
(define (square x) (* x x))
(define (tan-cf x k)
  (define (D b) (- (* 2 b) 1))
  (define (recur b)
    (let ((x2 (square x)))
      (cond ((= b k) (/ x2 (D b)))
            (else (/ x2 (- (D b) (recur (+ b 1))))))))
  (/ x (- 1 (recur 2))))

(tan-cf 1 100)

