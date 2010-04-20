;;

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
